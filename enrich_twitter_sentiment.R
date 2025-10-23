#!/usr/bin/env Rscript
# ──────────────────────────────────────────────────────────────────────────────
#  enrich_twitter_sentiment.R   (from twitter_raw_plus_flags → twitter_raw_plus_sentiment)
# ──────────────────────────────────────────────────────────────────────────────
#  1) read     public.twitter_raw_plus_flags
#  2) compute  sentiment + 8 NRC emotions on text
#  3) write    public.twitter_raw_plus_sentiment  (overwrite)
#     • preserves: tweet_type, date, main_id, user_id (and other useful fields)
# ──────────────────────────────────────────────────────────────────────────────

## 0 – packages ----------------------------------------------------------------
need <- c(
  "DBI","RPostgres","dplyr","stringr","tibble",
  "tidytext","tidyr","purrr","lubridate",
  "sentimentr","lexicon","data.table"
)
new <- need[!need %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org", quiet = TRUE)
invisible(lapply(need, library, character.only = TRUE))

## 1 – Supabase credentials ----------------------------------------------------
creds <- Sys.getenv(
  c("SUPABASE_HOST","SUPABASE_PORT","SUPABASE_DB","SUPABASE_USER","SUPABASE_PWD"),
  names = TRUE
)
if (any(!nzchar(creds))) stop("❌ One or more Supabase env vars are missing – aborting.")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = creds[["SUPABASE_HOST"]],
  port     = as.integer(creds[["SUPABASE_PORT"]] %||% "5432"),
  dbname   = creds[["SUPABASE_DB"]]   %||% "postgres",
  user     = creds[["SUPABASE_USER"]],
  password = creds[["SUPABASE_PWD"]],
  sslmode  = "require"
)

`%||%` <- function(x, y) if (is.null(x) || is.na(x) || x == "") y else x

## 2 – download enriched base table -------------------------------------------
src_tbl <- "twitter_raw_plus_flags"
tweets0 <- DBI::dbReadTable(con, src_tbl)
cat("✓ downloaded", nrow(tweets0), "rows from", src_tbl, "\n")

# Basic sanity + keep one row per tweet_id
tweets0 <- tweets0 %>%
  tibble::as_tibble() %>%
  mutate(
    date = case_when(
      inherits(date, "POSIXt") ~ date,
      TRUE ~ suppressWarnings(lubridate::ymd_hms(as.character(date), tz = "UTC"))
    )
  ) %>%
  arrange(date) %>%
  distinct(tweet_id, .keep_all = TRUE)

# Ensure the key columns exist (fallbacks if missing)
if (!"tweet_type" %in% names(tweets0)) {
  tweets0 <- tweets0 %>%
    mutate(
      is_rt_text = stringr::str_detect(text %||% "", "^RT @"),
      tweet_type = dplyr::case_when(
        is_rt_text ~ "retweet",
        TRUE       ~ "original"
      )
    )
}
if (!"main_id" %in% names(tweets0)) tweets0$main_id <- NA_character_

## 3 – clean text for NLP ------------------------------------------------------
tweets_clean <- tweets0 %>%
  transmute(
    # keep ids & context we’ll want to carry through
    tweet_id, tweet_url = .data[["tweet_url"]] %||% NA_character_,
    username, user_id, main_id, date, tweet_type,
    text_raw = text %||% ""
  ) %>%
  mutate(
    clean_text = text_raw %>%
      stringr::str_replace_all("&amp;|&gt;|&lt;", " ") %>%
      stringr::str_remove_all("http\\S+|@\\w+|[[:punct:]]") %>%
      stringr::str_squish()
  ) %>%
  # keep tweets with ≥ 6 words so the lexicons have signal
  filter(stringr::str_count(clean_text, "\\w+") > 5)

## 4 – sentiment polarity ------------------------------------------------------
sent_key <- sentimentr::update_key(
  lexicon::hash_sentiment_jockers_rinker
  # , x = data.frame(x = "hot", y = 1)  # example override; keep commented unless you need it
)

polarity <- sentimentr::sentiment_by(
  sentimentr::get_sentences(tweets_clean$clean_text),
  polarity_dt = sent_key
)

tweets_sent <- dplyr::bind_cols(
  tweets_clean %>% dplyr::select(tweet_id, username, user_id, main_id, date, tweet_type, tweet_url, clean_text),
  polarity %>% dplyr::select(element_id, ave_sentiment)
) %>%
  mutate(
    sentiment = dplyr::case_when(
      ave_sentiment >  0.00 ~ "positive",
      ave_sentiment < -0.00 ~ "negative",
      TRUE                  ~ "neutral"
    )
  )

## 5 – NRC emotions (8 basic) --------------------------------------------------
nrc_key <- lexicon::hash_nrc_emotions %>%
  dplyr::filter(emotion %in% c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")) %>%
  dplyr::filter(!token %in% c("damn","damned","dammit","goddamn","heck","fuck","fucks","crazy","shit"))

emo_raw <- sentimentr::emotion_by(tweets_sent$clean_text, emotion_dt = nrc_key) %>%
  dplyr::select(element_id, emotion_type, ave_emotion)

emotions <- emo_raw %>%
  tidyr::pivot_wider(
    names_from  = emotion_type,
    values_from = ave_emotion,
    values_fill = 0
  ) %>%
  # net-out negated versions (if present), then drop the *_negated columns
  mutate(
    dplyr::across(
      c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"),
      \(col) {
        pos <- get(col, inherits = FALSE)
        neg_name <- paste0(col, "_negated")
        neg <- if (exists(neg_name)) get(neg_name, inherits = FALSE) else 0
        pos - neg
      }
    )
  ) %>%
  dplyr::select(-tidyselect::ends_with("_negated"), tidyselect::everything())

## 6 – combine & final tidy frame ---------------------------------------------
result <- tweets_sent %>%
  dplyr::bind_cols(emotions) %>%
  # Reorder/keep the context columns up front (explicitly including the ones you asked for)
  dplyr::select(
    tweet_id, tweet_url, username, user_id, main_id, date, tweet_type,
    ave_sentiment, sentiment,
    anger, anticipation, disgust, fear, joy, sadness, surprise, trust,
    clean_text
  )

cat("✓ sentiment & emotions computed –", nrow(result), "rows ready\n")

## 7 – upload to Supabase ------------------------------------------------------
dest_tbl <- "twitter_raw_plus_sentiment"
DBI::dbWriteTable(
  con,
  name      = dest_tbl,
  value     = as.data.frame(result),
  overwrite = TRUE,
  row.names = FALSE
)
cat("✓ uploaded to table", dest_tbl, "\n")

DBI::dbDisconnect(con)
cat("✓ finished at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
