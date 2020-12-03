library(R6)
library(rvest, quietly = T)  # https://github.com/tidyverse/rvest
library(dplyr, warn.conflicts = F)

DohodParser <- R6Class(  # nolint
  "DohodParser",
  private = list(
    base_url = "https://www.dohod.ru/ik/analytics/dividend/"
  ),
  public = list(
    tickers = NULL,
    parse_main = function() {
      # зайдём на главную страницу и
      # получим список тикеров из таблицы
      page <- read_html(private$base_url)
      page %>%
        html_node("#table-dividend") %>%
        html_table(fill = T) %>%
        magrittr::extract2(21) %>%
        setdiff("XXXX") -> self$tickers
      message("Получил ", length(self$tickers), " тикеров")
    },

    # возвращает таблицу с дивидендной доходностью бумаги
    parse_ticker = function(symbol) {
      url <- paste0(private$base_url, symbol)
      page <- read_html(url)
      page %>%
        html_node("table.content-table:nth-child(9)") -> html_df

      if (length(html_df) == 0) {
        return(NULL)
      }

      html_table(html_df) -> symbol_df
      symbol_df %>%
        magrittr::set_colnames(c("record_date", "payment_date", "dividend", "profit_percent")) %>%
        mutate(date = as.Date(payment_date, format = "%d.%m.%Y"),
               year = data.table::year(date)) %>%
        select(year, dividend)
    }
  )
)
