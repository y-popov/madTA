# http://iss.moex.com/iss/reference/
library(R6)
library(httr)
library(rlist)
library(dplyr)
library(stringr)
library(RcppTOML)
library(data.table)

config <- parseTOML("./config.txt")

MoexApi <- R6Class(  # nolint
  "MoexApi",

  private = list(
    credentials = NULL,
    passport = NULL,

    # получает куки с сертификатом
    auth = function() {
      url <- "https://passport.moex.com/authenticate"
      res <- GET(url, authenticate(private$credentials$user, private$credentials$password))

      cookie <- cookies(res) %>% select(name, value) %>% tibble::deframe()
      private$passport <- cookie["MicexPassportCert"]
    },

    # делает запрос к бирже
    request = function(query_string, args = list(), format = c("json", "xml", "csv", "html")) {
      base_url <- "http://iss.moex.com/"
      format <- match.arg(format)

      res <- GET(base_url,
                 path = paste0("iss/", query_string, ".", format),
                 query = list(iss.meta = "off") %>% append(args),
                 set_cookies(.cookies = private$passport))

      status <- status_code(res)
      micexpassport_marker <- headers(res)[["x-micexpassport-marker"]]
      if (is.null(micexpassport_marker) || micexpassport_marker != "granted") {
        if (status != 200) {
          warning("micex passport denied")
          print(res)
        }
      }

      out <- content(res)
      if (status_code(res) != 200) {
        stop(out)
      }

      return(out)
    },

    # делает запросы к бирже, пока не перестанет получать данные
    paginate_request = function(query_string, params = list()) {
      n <- 1
      start <- 0
      df_list <- list()
      while (n > 0) {
        res <- private$request(query_string, args = list(start = start) %>% append(params))
        out <- moex_json_to_df(res[[1]])
        n <- nrow(out)
        start <- start + n
        df_list <- list.append(df_list, distinct(out))
        cat("\rRetrieved ", start, " records")
      }
      cat("\n")
      df <- rbindlist(df_list)
      return(df)
    }
  ),

  public = list(
    initialize = function(credentials) {
      stopifnot(is.list(credentials), length(credentials) == 2)
      private$credentials <- credentials
      private$auth()
    },

    make_requst = function(string, args = list()) {
      private$request(string, args)
    },

    # получает таблицу истории по бумаге
    sec_history = function(engine, market, sec) {
      string <- str_glue("history/engines/{engine}/markets/{market}/securities/{sec}/candles")
      private$paginate_request(query_string = string)
    },

    # получает таблицу дивидендов по бумаге
    get_dividends = function(secid) {
      uri <- str_glue("securities/{secid}/dividends")
      res <- private$request(query_string = uri)
      moex_json_to_df(res$dividends)
    },

    # получает таблицы купонов и амортизаций
    get_bondisation = function(isin) {
      # https://iss.moex.com/iss/statistics/engines/stock/markets/bonds/bondization/RU000A0JXU71
      uri <- str_glue("securities/{isin}/bondization")
      res <- private$request(query_string = uri)
      lapply(res, moex_json_to_df)
    },

    # получает общую информацию о бирже (рынки, режимы торгов и пр.)
    get_moex_info = function() {
      res <- private$request("index")
      lapply(res, moex_json_to_df)
    },

    # получает информацию обо всех акциях, торгуемых на бирже
    get_all_securities = function() {
      res <- private$paginate_request("securities",
                                      params = list("is_trading" = 1, engine = "stock", market = "shares"))
      #moex_json_to_df(res$securities)
    }

  )
)

# лист из двух узлов columns и data переводит в таблицу
moex_json_to_df <- function(x) {
  if (length(x$data) == 0) {
    return(data.frame())
  }
  df <- suppressWarnings(list.stack(x$data))
  colnames(df) <- x$columns
  return(df)
}

#df = moex$get_moex_info()
#dt = moex$get_all_securities()
#r = moex$sec_history(engine = "stock", market = "shares", sec = "SBER")
