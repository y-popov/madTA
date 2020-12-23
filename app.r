library(shiny)
library(stringr)
source("src/moex.r")
source("src/stocks_toolbox.r")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(locale = "Russian")
}

moex <- MoexApi$new(config$moex)
all_sec_df <- moex$get_all_securities()

ui <- fluidPage(
  title = "Stocks analyzer",
  sidebarLayout(
    sidebarPanel(
      selectInput("sec", "Ticker symbol", choices = c("", all_sec_df$secid)),
      conditionalPanel(
        condition = "input.sec != ''",
        selectInput("board_id", "Board ID", choices = "")
      ),
      hr(),
      checkboxGroupInput("ta", "Select TA",
                         choices = c("sma", "ema", "bband", "momentum", "roc", "macd", "rsi"))
    ),
    mainPanel(
      highchartOutput("candles", height = "600px"),
      DT::dataTableOutput("candle_patterns")
    )
  )
)

server <- function(input, output, session) {

  reactive_sec_history <- reactive({
    validate(need(input$sec, "Select security id"))
    moex$sec_history(engine = "stock", market = "shares", sec = input$sec)
  })

  observe({
    ids <- unique(reactive_sec_history()$BOARDID)
    updateSelectInput(session, "board_id", choices = c("", ids))
  })

  reactive_data <- reactive({
    validate(need(input$board_id, "Select board id"))
    reactive_sec_history() %>%
      filter(BOARDID == input$board_id) %>%
      filter(!is.na(CLOSE)) %>%
      select(-BOARDID, -SHORTNAME, -SECID) %>%
      rename(LEGALCLOSINGPRICE = LEGALCLOSEPRICE) %>%  # иначе определяется вместо CLOSE
      mutate(TRADEDATE = as.Date(TRADEDATE))
  })

  output$candles <- renderHighchart({
    title <- all_sec_df %>% filter(secid == input$sec) %>% pull(name)
    plot_candles(reactive_data(), title) %>%
      purrr::when("sma" %in% input$ta ~ plot_sma(., reactive_data()), ~ .) %>%
      purrr::when("ema" %in% input$ta ~ plot_ema(., reactive_data()), ~ .) %>%
      purrr::when("bband" %in% input$ta ~ plot_bband(., reactive_data()), ~ .) %>%
      purrr::when("momentum" %in% input$ta ~ plot_momentum(., reactive_data()), ~ .) %>%
      purrr::when("roc" %in% input$ta ~ plot_roc(., reactive_data()), ~ .) %>%
      purrr::when("macd" %in% input$ta ~ plot_macd(., reactive_data()), ~ .) %>%
      purrr::when("rsi" %in% input$ta ~ plot_rsi(., reactive_data()), ~ .)
  })

  output$sec_title <- renderText({
    all_sec_df %>% filter(secid == input$sec) %>% pull(name) %>% enc2native()
  })

  output$candle_patterns <- DT::renderDataTable({
    reactive_data() %>%
      find_candle_patterns() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "DATE")
  }, rownames = FALSE)
}

shinyApp(ui, server)
