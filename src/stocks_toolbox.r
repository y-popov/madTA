library(TTR)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(highcharter)


# Tim Tillson's T3 indicator
T3 <- function(x, n=10, v=1) DEMA(DEMA(DEMA(x,n,v),n,v),n,v)

plot_candles <- function(df){
  highchart(type = 'stock') %>%
    hc_add_series(xts::as.xts(df))
}

# indicators
add_SMA_TA <- function(df, short_run = 30, long_run = 200){
  df %>%
    mutate(sma_short = SMA(CLOSE, n = short_run),
           sma_long = SMA(CLOSE, n = long_run))
}

plot_SMA_TA <- function(chart, df, short_run = 30, long_run = 200){
  df = add_SMA_TA(df, short_run, long_run)
  chart %>%
    hc_add_series(df, hcaes(y = sma_short, x = TRADEDATE),
                  type = 'line', name = str_glue('SMA {short_run} days')) %>%
    hc_add_series(df, hcaes(y = sma_long, x = TRADEDATE),
                  type = 'line', name = str_glue('SMA {long_run} days'))
}

# signals
