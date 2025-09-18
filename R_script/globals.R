# globals.R
# Packages & settings
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(lubridate)
  library(quantmod)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(kableExtra)
  library(lmtest)
  library(sandwich)
  library(broom)
})

options(stringsAsFactors = FALSE, scipen = 999)

# HAC-standard errors (Newey–West)
s_hac <- function(model, lag = 10, prewhite = TRUE) {
  coeftest(model, vcov. = NeweyWest(model, lag = lag, prewhite = prewhite))
}
# (NeweyWest details: sandwich::NeweyWest builds Bartlett-kernel HAC vcov. :contentReference[oaicite:1]{index=1})

# Presidency labels (4-level factor)
tag_president <- function(df) {
  df %>%
    mutate(president = case_when(
      date >= as.Date("1993-01-20") & date < as.Date("2001-01-20") ~ "Clinton",
      date >= as.Date("2001-01-20") & date < as.Date("2009-01-20") ~ "Bush",
      date >= as.Date("2009-01-20") & date < as.Date("2017-01-20") ~ "Obama",
      date >= as.Date("2017-01-20") & date < as.Date("2021-01-20") ~ "Trump",
      TRUE ~ "Other"
    ),
    pres4 = factor(president, levels = c("Clinton","Bush","Obama","Trump")))
}

# Pull SPY OHLC from WRDS CRSP
get_spy_ohlc_wrds <- function(con) {
  sql <- "
    SELECT a.date, b.ticker,
           a.openprc AS open, a.askhi AS high, a.bidlo AS low, a.prc AS close, a.vol
    FROM crsp.dsf a
    JOIN crsp.dsenames b ON a.permno = b.permno
     AND b.namedt <= a.date AND a.date <= b.nameendt
    WHERE b.ticker = 'SPY' AND a.date >= '1993-01-01'
    ORDER BY a.date;"
  dbGetQuery(con, sql) |>
    transmute(
      date  = as.Date(date),
      Open  = as.numeric(open),
      High  = as.numeric(high),
      Low   = as.numeric(low),
      Close = as.numeric(close),
      Volume= as.numeric(vol)
    )
}

# Nearest tweet-day event time in [-3, +3]
mk_nearest_k <- function(tweet_days, d) {
  if (!length(tweet_days)) return(NA_integer_)
  dif <- as.integer(d - tweet_days)
  j <- which.min(abs(dif))
  k <- dif[j]
  if (is.finite(k) && abs(k) <= 3L) as.integer(k) else NA_integer_
}

# Simple formatters
fmt_num <- function(x, d = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
fmt_pct <- function(x, d = 2) ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = d), "%"))

