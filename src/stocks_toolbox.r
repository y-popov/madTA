library(TTR)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(highcharter)
library(quantstrat)
library(CandleStickPattern)


# Tim Tillson's T3 indicator
t3 <- function(x, n=10, v=1) DEMA(DEMA(DEMA(x, n, v), n, v), n, v)

plot_candles <- function(df, title = 'Candle plot') {
  highchart(type = "stock") %>%
    hc_title(text = title) %>%
    hc_add_series(xts::as.xts(df), yAxis = 0) %>%
    hc_add_yAxis(nid = 1L, relative = 2) %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = VOLUME), yAxis = 1, type = "column", name = "Volume", color = "gray") %>%
    hc_add_yAxis(nid = 1L, relative = 1) %>%
    hc_tooltip(valueDecimals = 2)
}

# indicators
add_sma <- function(df, short_run = 30, long_run = 200) {
  df %>%
    mutate(sma_short = SMA(CLOSE, n = short_run),
           sma_long = SMA(CLOSE, n = long_run))
}

plot_sma <- function(chart, df, short_run = 30, long_run = 200) {
  df <- add_sma(df, short_run, long_run)
  chart %>%
    hc_add_series(df, hcaes(y = sma_short, x = TRADEDATE),
                  type = "line", name = str_glue("SMA {short_run} days")) %>%
    hc_add_series(df, hcaes(y = sma_long, x = TRADEDATE),
                  type = "line", name = str_glue("SMA {long_run} days"))
}

add_ema <- function(df, short_run = 30, long_run = 200) {
  df %>%
    mutate(sma_short = EMA(CLOSE, n = short_run),
           sma_long = EMA(CLOSE, n = long_run))
}

plot_ema <- function(chart, df, short_run = 30, long_run = 200) {
  df <- add_ema(df, short_run, long_run)
  chart %>%
    hc_add_series(df, hcaes(y = sma_short, x = TRADEDATE),
                  type = "line", name = str_glue("EMA {short_run} days")) %>%
    hc_add_series(df, hcaes(y = sma_long, x = TRADEDATE),
                  type = "line", name = str_glue("EMA {long_run} days"))
}

add_bband <- function(df, n = 20, sd = 2) {
  df %>%
    mutate(bbands = BBands(CLOSE, n = n, sd = sd) %>%
      as.data.frame() %>%
      split(f = seq(nrow(.)))
    ) %>%
    unnest(bbands)
}

plot_bband <- function(chart, df, n = 20, sd = 2) {
  df <- add_bband(df, n, sd)
  chart %>%
    hc_add_series(df, hcaes(x = TRADEDATE, low = dn, high = up), zIndex = -3,
                  type = "arearange", name = "band", color = hex_to_rgba("gray", 0.1)) %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = mavg), type = "line", name = "trend", dashStyle = "Dot")
}

add_momentum <- function(df, n = 1) {
  df %>%
    mutate(moment = momentum(CLOSE, n = n))
}

plot_momentum <- function(chart, df, n = 1) {
  df <- add_momentum(df, n)
  n_yaxis <- length(chart$x$hc_opts$yAxis)
  chart %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = moment), type = "line", yAxis = n_yaxis,
                  name = str_glue("{n}-day momentum"), color = "blue") %>%
    hc_add_yAxis(nid = n_yaxis + 1L, relative = 1)
}

# rate of change
add_roc <- function(df, n = 7) {
    df %>%
      mutate(roc = ROC(CLOSE, n))
}

plot_roc <- function(chart, df, n = 7) {
  df <- add_roc(df, n)
  n_yaxis <- length(chart$x$hc_opts$yAxis)
  chart %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = roc), type = "line", yAxis = n_yaxis,
                  name = str_glue("{n}-day ROC"), color = "red") %>%
    hc_add_yAxis(nid = n_yaxis + 1L, relative = 1)
}

# Moving average convergence/divergence
add_macd <- function(df, fast = 12, slow = 26, sig = 9) {
  df %>%
    mutate(macd = MACD(CLOSE, nFast = fast, nSlow = slow, nSig = sig, percent = FALSE) %>%
      as.data.frame() %>%
      split(f = seq(n()))
    ) %>%
    unnest(macd)
}

# Buy signal arises when MACD crosses from below to above the signal line
# Sell signal arrises when MACD crosses from above to below the signal line
plot_macd <- function(chart, df, fast = 12, slow = 26, sig = 9) {
  df <- add_macd(df, fast, slow, sig)
  n_yaxis <- length(chart$x$hc_opts$yAxis)
  chart %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = macd), type = "line", yAxis = n_yaxis, color = 'lightgrey',
                  name = str_glue('MACD ({fast}, {slow}, {sig})')) %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = signal), type = "line", yAxis = n_yaxis, color = 'red',
                  name = "MACD signal", dashStyle = "Dot") %>%
    hc_add_yAxis(nid = n_yaxis + 1L, relative = 1)
}

# Relative Strength Index
add_rsi <- function(df, n = 14, fun = EMA) {
  df %>%
    mutate(rsi = RSI(CLOSE, n = n, maType = fun))
}

# Buy signal arises when RSI is less than 30
# Sell signal arrises when RSI is higher than 30
plot_rsi <- function(chart, df, n = 14, fun = EMA) {
  df <- add_rsi(df, n, fun)
  n_yaxis <- length(chart$x$hc_opts$yAxis)
  chart %>%
    hc_add_series(df, hcaes(x = TRADEDATE, y = rsi), type = "line", yAxis = n_yaxis, name = str_glue("{n}-day RSI")) %>%
    hc_add_yAxis(nid = n_yaxis + 1L, relative = 1)
}

# signals

find_candle_patterns <- function(df) {
  df_series = df %>% select(-LEGALCLOSEPRICE) %>% as.xts()
  cbind(
    doji(df_series),
    dragonfly.doji(df_series)
  )
}
