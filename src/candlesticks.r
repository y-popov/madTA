library(R6)
library(xts)
library(dplyr)
library(CandleStickPattern)
# https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/candle-stick-pattern.html

candle_pattern_info <- list(
  doji = "Слабый разворотный сигнал после тренда. Возобновление тренда сразу не произойдёт",
  marubozu = "Уверенный тренд",
  harami = "Беременный, слабый разворотный сигнал, требует подтверждения",
  engulfing = "Поглощение, сильный разворотный сигнал, подтверждение не требует",
  start = "После тенденции возникает особая тройка, сильный разворотный сигнал",
  three_white_soldiers = "Цены растут с ускорением"

)

Candlesticks <- R6Class(  # nolint
  "Candlesticks",
  public = list(
    df = data.frame(),
    delta = NA_real_,
    initialize = function(df, delta = 0.1) {
      stopifnot(is.data.frame(df), is.numeric(delta))
      self$delta <- delta
      self$df <- df %>%
        mutate(candle_length = HIGH - LOW,
               body_length = abs(OPEN - CLOSE)) %>%
        as.xts()
    },

    marubozu = function() {  # лысый
      result <- with(self$df, (1 - self$delta) * candle_length < body_length)
      colnames(result) <- "marubozu"
      return(result)
    },

    advance_block = function() {
      browser()
      result <- with(
        self$df,
        CLOSE > lag.xts(CLOSE, 1) & lag.xts(CLOSE, 1) > lag.xts(CLOSE, 2) &
          OPEN > lag.xts(OPEN, 1) & lag.xts(OPEN, 1) > lag.xts(OPEN, 2) &
          lag.xts(body_length, 2) < lag.xts(body_length, 1) & body_length < lag.xts(body_length, 2)
      )
      colnames(result) <- "advance block"
      return(result)
    }
  )
)


#candlesticks <- Candlesticks$new(df)
#candlesticks$marubozu()