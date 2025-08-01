

#!/usr/bin/env Rscript
# ──────────────────────────────────────────────────────────────────────────────
#  enrich_twitter_sentiment.R
# ──────────────────────────────────────────────────────────────────────────────
#  1.  download  twitter_raw            (existing table)
#  2.  compute sentiment + 8 NRC emotions
#  3.  write     public.twitter_raw_plus_sentiment   (overwrite)
# ──────────────────────────────────────────────────────────────────────────────

## 0 – packages ----------------------------------------------------------------
need <- c(
  "DBI", "RPostgres", "dplyr", "stringr", "tibble",
  "tidytext", "purrr", "lubridate", "sentimentr", "lexicon", "data.table"
)
new <- need[!need %in% rownames(installed.packages())]
if (length(new))
  install.packages(new, repos = "https://cloud.r-project.org", quiet = TRUE)
invisible(lapply(need, library, character.only = TRUE))

## 1 – Supabase credentials (taken from ENV in the GitHub Action) -------------
creds <- Sys.getenv(
  c("SUPABASE_HOST", "SUPABASE_PORT", "SUPABASE_DB",
    "SUPABASE_USER", "SUPABASE_PWD"),
  names = TRUE
)

if (any(!nzchar(creds)))
  stop("❌ One or more Supabase env vars are missing – aborting.")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = creds["SUPABASE_HOST"],
  port     = as.integer(creds["SUPABASE_PORT"]),
  dbname   = creds["SUPABASE_DB"],
  user     = creds["SUPABASE_USER"],
  password = creds["SUPABASE_PWD"],
  sslmode  = "require"
)

## 2 – download raw tweets -----------------------------------------------------
twitter_raw <- DBI::dbReadTable(con, "twitter_raw")
cat("✓ downloaded", nrow(twitter_raw), "rows from twitter_raw\n")

## 3 – canonical IDs  ----------------------------------------------------------
main_ids <- tibble::tribble(
  ~username,            ~main_id,
  "weave_db",           "1206153294680403968",
  "OdyseeTeam",         "1280241715987660801",
  "ardriveapp",         "1293193263579635712",
  "redstone_defi",      "1294053547630362630",
  "everpay_io",         "1334504432973848577",
  "decentlandlabs",     "1352388512788656136",
  "KYVENetwork",        "136377177683878784",
  "onlyarweave",        "1393171138436534272",
  "ar_io_network",      "1468980765211955205",
  "Permaswap",          "1496714415231717380",
  "communitylabs",      "1548502833401516032",
  "usewander",          "1559946771115163651",
  "apus_network",       "1569621659468054528",
  "fwdresearch",        "1573616135651545088",
  "perma_dao",          "1595075970309857280",
  "Copus_io",           "1610731228130312194",
  "basejumpxyz",        "1612781645588742145",
  "AnyoneFDN",          "1626376419268784130",
  "arweaveindia",       "1670147900033343489",
  "useload",            "1734941279379759105",
  "protocolland",       "1737805485326401536",
  "aoTheComputer",      "1750584639385939968",
  "ArweaveOasis",       "1750723327315030016",
  "aox_xyz",            "1751903735318720512",
  "astrousd",           "1761104764899606528",
  "PerplexFi",          "1775862139980226560",
  "autonomous_af",      "1777500373378322432",
  "Liquid_Ops",         "1795772412396507136",
  "ar_aostore",         "1797632049202794496",
  "FusionFiPro",        "1865790600462921728",
  "vela_ventures",      "1869466343000444928",
  "beaconwallet",       "1879152602681585664",
  "VentoSwap",          "1889714966321893376",
  "permawebjournal",    "1901592191065300993",
  "Botega_AF",          "1902521779161292800",
  "samecwilliams",      "409642632",
  "TateBerenbaum",      "801518825690824707",
  "ArweaveEco",         "892752981736779776"
)

## 4 – basic pre-processing ----------------------------------------------------
tweets <- twitter_raw %>%
  left_join(main_ids, by = "username") %>%
  mutate(
    is_rt_text = str_detect(text, "^RT @"),
    tweet_type = case_when(
      is_rt_text                                  ~ "retweet",
      user_id == main_id & !is_rt_text &
        str_detect(text, "https://t.co")          ~ "quote",
      user_id == main_id                          ~ "original",
      TRUE                                        ~ "other"
    ),
    publish_dt = ymd_hms(date, tz = "UTC")
  ) %>%
  arrange(publish_dt) %>%
  distinct(tweet_id, .keep_all = TRUE)

## 5 – clean text for NLP ------------------------------------------------------
tweets_clean <- tweets %>%
  mutate(
    clean_text = text %>%
      str_replace_all("&amp;|&gt;|&lt;", " ") %>%
      str_remove_all("http\\S+|@\\w+|[[:punct:]]") %>%
      str_squish()
  ) %>%
  filter(str_count(clean_text, "\\w+") > 5)   # keep tweets with ≥ 6 words

## 6 – sentiment polarity ------------------------------------------------------
sent_key <- update_key(
  lexicon::hash_sentiment_jockers_rinker,
  x = data.frame(x = "hot", y = 1)   # custom override (optional)
)

polarity <- sentiment_by(get_sentences(tweets_clean$clean_text),
                         polarity_dt = sent_key)

tweets_sent <- bind_cols(
  tweets_clean %>% select(tweet_id, clean_text),
  polarity %>% select(element_id, ave_sentiment)
) %>%
  mutate(sentiment = case_when(
    ave_sentiment >  0.00 ~ "positive",
    ave_sentiment < -0.00 ~ "negative",
    TRUE                  ~ "neutral"
  ))

## 7 – NRC emotions (8 basic) --------------------------------------------------
nrc_key <- lexicon::hash_nrc_emotions %>%
  filter(emotion %in% c("anger","anticipation","disgust","fear",
                        "joy","sadness","surprise","trust")) %>%
  filter(!token %in% c(
    "damn","damned","dammit","goddamn","heck","fuck","fucks","crazy","shit"
  ))

emo_raw <- emotion_by(tweets_sent$clean_text, emotion_dt = nrc_key) %>%
  select(element_id, emotion_type, ave_emotion)

emotions <- emo_raw %>%
  pivot_wider(names_from = emotion_type, values_from = ave_emotion,
              values_fill = 0)

## 8 – combine & final tidy frame ---------------------------------------------
result <- tweets_sent %>%
  bind_cols(emotions) %>%
  select(tweet_id, ave_sentiment, sentiment,
         anger:trust, everything())

cat("✓ sentiment & emotions computed –", nrow(result), "rows ready\n")

## 9 – upload to Supabase ------------------------------------------------------
dest_tbl <- "twitter_raw_plus_sentiment"   # ← unchanged table name

DBI::dbWriteTable(
  con,
  name      = dest_tbl,                    # no schema prefix
  value     = as.data.frame(result),
  overwrite = TRUE,                        # overwrite each run
  row.names = FALSE
)

cat("✓ uploaded to table", dest_tbl, "\n")

DBI::dbDisconnect(con)
cat("✓ finished at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

