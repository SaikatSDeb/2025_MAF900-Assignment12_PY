# analysis_main.R
# expects globals.R to be sourced first

# WRDS
wrds <- DBI::dbConnect(
  RPostgres::Postgres(),
  host   = "wrds-pgdata.wharton.upenn.edu",
  port   = 9737,
  dbname = "wrds",
  sslmode = "require",
  user   = "s225233606"
)

# CRSP daily (SPY + sectors)
tickers <- c("SPY","XLF","XLI","XLK","XLU","XLP")
qry <- sprintf("
  SELECT a.date, b.ticker, a.ret, a.vol, a.prc
  FROM crsp.dsf a
  JOIN crsp.dsenames b ON a.permno = b.permno
   AND b.namedt <= a.date AND a.date <= b.nameendt
  WHERE b.ticker IN (%s) AND a.date >= '1993-01-01'
  ORDER BY a.date", paste(sprintf("'%s'", tickers), collapse = ","))
market_sector_data <- DBI::dbGetQuery(wrds, qry) |> dplyr::mutate(date = as.Date(date))

# VIX (Yahoo)
quantmod::getSymbols("^VIX", src = "yahoo", from = "1993-01-01", auto.assign = TRUE)
vix_df <- data.frame(date = as.Date(index(VIX)), VIX = as.numeric(VIX$VIX.Close))

# SPY panel
spy_data <- market_sector_data |>
  dplyr::filter(ticker == "SPY") |>
  dplyr::select(date, ret, vol) |>
  dplyr::distinct() |>
  dplyr::left_join(vix_df, by = "date") |>
  dplyr::mutate(abs_ret = abs(as.numeric(ret)))

# Trump tweets (archive) + market-sensitive flag
urls <- c(
  "https://raw.githubusercontent.com/MarkHershey/CompleteTrumpTweetsArchive/master/data/realDonaldTrump_bf_office.csv",
  "https://raw.githubusercontent.com/MarkHershey/CompleteTrumpTweetsArchive/master/data/realDonaldTrump_in_office.csv"
)
trump_tweets <- do.call(rbind, lapply(urls, read.csv, stringsAsFactors = FALSE))
names(trump_tweets) <- make.names(names(trump_tweets))
dt_col   <- intersect("Time",       names(trump_tweets))[1]
text_col <- intersect("Tweet.Text", names(trump_tweets))[1]

dt_raw <- trimws(as.character(trump_tweets[[dt_col]]))
dt_raw <- gsub("T", " ", dt_raw, fixed = TRUE)
dt_raw <- sub("Z$", "", dt_raw, perl = TRUE)
dt_parsed <- as.POSIXct(dt_raw, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
na_idx <- is.na(dt_parsed)
if (any(na_idx)) dt_parsed[na_idx] <- as.POSIXct(dt_raw[na_idx], format = "%Y-%m-%d %H:%M", tz = "UTC")

trump_tweets$datetime_utc <- dt_parsed
trump_tweets$datetime_ny  <- lubridate::with_tz(trump_tweets$datetime_utc, tzone = "America/New_York")
trump_tweets$date         <- as.Date(trump_tweets$datetime_ny)
trump_tweets$tweet_text   <- as.character(trump_tweets[[text_col]])
trump_tweets <- dplyr::filter(trump_tweets, !stringr::str_detect(tweet_text, "^RT\\b"))

pat_cashtag <- "\\$(?!US[A-Z]?\\b)[A-Z]{1,5}\\b"
idx_terms <- c("S&P\\s*500","S\\s*&\\s*P","\\bSP500\\b","\\bSPX\\b","\\bS&P\\b",
               "\\bNasdaq\\b","Dow(\\s|$|,|\\.)","Dow\\s*Jones","\\bDJIA\\b",
               "Wall\\s*Street","(?<!super)market\\b","equities\\b","\\bVIX\\b",
               "rally\\b","sell[- ]?off\\b","crash\\b","plunge\\b","surge\\b",
               "record\\s+high\\b","limit[- ]?down\\b","limit[- ]?up\\b")
macro_terms <- c("econom(y|ic)\\b","\\bGDP\\b","inflation\\b","\\bCPI\\b","\\bPPI\\b",
                 "jobs?\\b","unemployment\\b","tariff(s)?\\b","sanction(s)?\\b",
                 "trade\\s+(war|deal|deficit|talks?)\\b","Phase\\s*1\\b","\\bUSMCA\\b","\\bNAFTA\\b",
                 "\\bChina\\b","\\bEU\\b","\\bECB\\b",
                 "interest\\s*rate(s)?\\b","rate\\s*hike(s)?\\b","rates?\\s*cut(s)?\\b",
                 "Treasur(y|ies)\\b","yield(s)?\\b","bond(s)?\\b","deregulation\\b","regulation\\b",
                 "corporate\\s*tax\\b","tax\\s*cuts?\\b","\\bOPEC\\b","\\boil\\b","\\benergy\\b","pipeline\\b")
co_names <- c("Amazon\\b","\\bAMZN\\b","Apple\\b","\\bAAPL\\b","Google\\b","Alphabet\\b","\\bGOOGL\\b",
              "Facebook\\b","Meta\\b","\\bFB\\b","AT&T\\b","\\bT\\b","Comcast\\b","\\bCMCSA\\b","Time\\s*Warner\\b",
              "Boeing\\b","\\bBA\\b","Lockheed\\b","\\bLMT\\b","General\\s*Motors\\b","\\bGM\\b",
              "Ford\\b","\\bF\\b","Carrier\\b","United\\s*Technologies\\b","Merck\\b","\\bMRK\\b","Pfizer\\b","\\bPFE\\b",
              "Exxon\\b","\\bXOM\\b","Goldman\\s*Sachs\\b","\\bGS\\b","JPMorgan\\b","\\bJPM\\b","Bank\\s*of\\s*America\\b","\\bBAC\\b")
micro_terms <- c("short\\s*squeeze\\b","shorts?\\b","liquidit(y|ies)\\b","circuit\\s*breaker(s)?\\b")
verbs <- c("sign(s|ed)?\\b","announce(s|d)?\\b","impose(s|d)?\\b","approve(s|d)?\\b",
           "raise(s|d|ing)?\\b","cut(s|ting)?\\b","ban(s|ned)?\\b","sanction(s|ed)?\\b",
           "invest(ment|ing|s)?\\b","deal(s)?\\b","buyback(s)?\\b","upgrade(s|d)?\\b","downgrade(s|d)?\\b")
num_move <- "(\\b\\d{3,}\\s*(points?|pt|pts)\\b|\\b\\d+(?:\\.\\d+)?%\\b)"
stop_terms <- c("TV\\s*ratings\\b","crowd\\s*size\\b","rally\\s*tonight\\b","Fake\\s*News\\b",
                "CNN\\b","MSNBC\\b","Fox\\s*News\\b","reporter\\b","poll(s|ing)?\\b",
                "elect(ion|oral)\\b","witch\\s*hunt\\b","NFL\\b","NCAA\\b","Super\\s*Bowl\\b","golf\\b","supermarket\\b")

re_equity  <- regex(paste(c(pat_cashtag, idx_terms, co_names), collapse="|"), ignore_case=TRUE)
re_macro   <- regex(paste(macro_terms, collapse="|"), ignore_case=TRUE)
re_action  <- regex(paste(c(verbs, micro_terms), collapse="|"), ignore_case=TRUE)
re_numbers <- regex(num_move, ignore_case=TRUE)
re_stop    <- regex(paste(stop_terms, collapse="|"), ignore_case=TRUE)

txt <- trump_tweets$tweet_text
trump_tweets$market_sensitive <- stringr::str_detect(txt, re_equity) &
  (stringr::str_detect(txt, re_macro) | stringr::str_detect(txt, re_action) | stringr::str_detect(txt, re_numbers)) &
  !stringr::str_detect(txt, re_stop)

tweet_daily <- trump_tweets |>
  dplyr::filter(market_sensitive) |>
  dplyr::group_by(date) |>
  dplyr::summarise(tweet_day = 1L, tweet_count = dplyr::n(), .groups="drop")

# Realized vol (GK, RS)
spy_ohlc <- get_spy_ohlc_wrds(wrds)
pos <- function(x) ifelse(x <= 0 | is.na(x), NA, x)
spy_rv_daily <- spy_ohlc |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    O = pos(Open), H = pos(High), L = pos(Low), C = pos(Close),
    co = log(C/O), hl = log(H/L),
    var_gk = 0.5*hl^2 - (2*log(2) - 1)*(co^2),
    var_rs = log(H/C)*log(H/O) + log(L/C)*log(L/O),
    var_gk = pmax(var_gk, 0),
    var_rs = ifelse(var_rs < 0, NA, var_rs),
    rvol_gk = sqrt(var_gk),
    rvol_rs = sqrt(var_rs)
  ) |>
  dplyr::select(date, rvol_gk, rvol_rs)

# Master panel + labels
panel <- spy_data |>
  dplyr::left_join(spy_rv_daily, by="date") |>
  dplyr::left_join(tweet_daily, by="date") |>
  dplyr::mutate(
    tweet_day   = dplyr::if_else(is.na(tweet_day),   0L, tweet_day),
    tweet_count = dplyr::if_else(is.na(tweet_count), 0L, tweet_count)
  ) |>
  tag_president()

panel_trump <- panel |>
  dplyr::filter(date >= as.Date("2017-01-20") & date < as.Date("2021-01-20"))

# H1: volatility vs presidencies
h1_df <- panel |>
  dplyr::filter(president %in% c("Clinton","Bush","Obama","Trump"))
h1_summ <- h1_df |>
  dplyr::group_by(pres4) |>
  dplyr::summarise(N=n(),
                   mean_VIX = mean(VIX, na.rm=TRUE),
                   mean_RV  = mean(rvol_gk, na.rm=TRUE),
                   .groups="drop")
m_vix <- lm(VIX ~ pres4, data = h1_df)
m_rv  <- lm(rvol_gk ~ pres4, data = h1_df)

# H2: volume & extreme-day freq
preTrump_vol_thresh <- stats::quantile(spy_data$vol[spy_data$date < "2017-01-01"], 0.95, na.rm = TRUE)
spy_all <- panel |>
  dplyr::select(date, VIX, rvol_gk) |>
  dplyr::right_join(spy_data, by = "date") |>
  dplyr::mutate(
    Trump        = as.integer(date >= as.Date("2017-01-20") & date < as.Date("2021-01-20")),
    extreme_vol  = as.integer(vol > preTrump_vol_thresh),
    lvol         = log(pmax(vol, 1)),
    dow          = lubridate::wday(date, label = TRUE, abbr = TRUE),
    mon          = lubridate::month(date, label = TRUE, abbr = TRUE)
  )
h2_means <- spy_all |>
  dplyr::mutate(period = dplyr::if_else(Trump==1L,"Trump","Pre/Post non-Trump")) |>
  dplyr::group_by(period) |>
  dplyr::summarise(mean_vol = mean(vol, na.rm=TRUE),
                   share_extreme = mean(extreme_vol, na.rm=TRUE), .groups="drop")
m_lvol    <- lm(lvol ~ Trump + dow + mon, data = spy_all)
m_extreme <- lm(extreme_vol ~ Trump + dow + mon, data = spy_all)

# H3: sector DiD
trade_sensitive <- c("XLF","XLI","XLK")
defensive       <- c("XLU","XLP")
sec <- market_sector_data |>
  dplyr::filter(ticker != "SPY", ticker %in% c(trade_sensitive, defensive)) |>
  dplyr::mutate(
    Trump   = as.integer(date >= as.Date("2017-01-20") & date < as.Date("2021-01-20")),
    trade   = as.integer(ticker %in% trade_sensitive),
    abs_ret = abs(as.numeric(ret)),
    lvol    = log(pmax(vol, 1)),
    dow     = lubridate::wday(date, label = TRUE, abbr = TRUE),
    mon     = lubridate::month(date, label = TRUE, abbr = TRUE)
  )
m_h3_vol <- lm(lvol   ~ Trump*trade + dow + mon + factor(ticker), data = sec)
m_h3_vty <- lm(abs_ret ~ Trump*trade + dow + mon + factor(ticker), data = sec)
sec_summ <- sec |>
  dplyr::mutate(group = dplyr::if_else(trade==1,"Trade-sensitive","Defensive"),
                era = dplyr::if_else(Trump==1,"Trump","Pre/Other")) |>
  dplyr::group_by(group, era) |>
  dplyr::summarise(mean_lvol = mean(lvol, na.rm=TRUE),
                   mean_absret = mean(abs_ret, na.rm=TRUE), .groups="drop")

# H4: tweets impact
tweet_dates <- tweet_daily$date
tweet_win1  <- unique(c(tweet_dates, tweet_dates + 1, tweet_dates - 1))
panel_trump <- panel_trump |>
  dplyr::mutate(in_win1 = as.integer(date %in% tweet_win1))

h4 <- panel_trump |>
  dplyr::mutate(
    lvol = log(pmax(vol, 1)),
    dow  = lubridate::wday(date, label = TRUE, abbr = TRUE),
    mon  = lubridate::month(date, label = TRUE, abbr = TRUE)
  )
m_h4_abs  <- lm(abs_ret ~ tweet_day + dow + mon, data = h4)
m_h4_rv   <- lm(rvol_gk ~ tweet_day + dow + mon, data = h4)
m_h4_lvol <- lm(lvol    ~ tweet_day + dow + mon, data = h4)

m_h4w_abs  <- lm(abs_ret ~ in_win1 + dow + mon, data = h4)
m_h4w_rv   <- lm(rvol_gk ~ in_win1 + dow + mon, data = h4)
m_h4w_lvol <- lm(lvol    ~ in_win1 + dow + mon, data = h4)

event_ks   <- -3:3
tweet_days <- sort(unique(tweet_daily$date))
h4$k <- vapply(h4$date, function(d) mk_nearest_k(tweet_days, d), integer(1L))
h4_ev <- dplyr::filter(h4, !is.na(k))
h4_ev$k_fac <- stats::relevel(factor(h4_ev$k), ref = "-1")
m_ev_abs  <- lm(abs_ret ~ k_fac + dow + mon, data = h4_ev)
m_ev_rv   <- lm(rvol_gk ~ k_fac + dow + mon, data = h4_ev)
m_ev_lvol <- lm(lvol    ~ k_fac + dow + mon, data = h4_ev)

# objects for report are available in global env after source()

