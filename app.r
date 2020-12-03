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
      checkboxGroupInput("ta", "Select TA", choices = c("sma_ta", "ema_ta"))
    ),
    mainPanel(
      textOutput("sec_title"),
      highchartOutput("candles")
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
      select(-BOARDID, -SHORTNAME, -SECID) %>%
      mutate(TRADEDATE = as.Date(TRADEDATE))
  })

  output$candles <- renderHighchart({
    plot_candles(reactive_data()) %>%
      purrr::when("sma_ta" %in% input$ta ~ plot_sma_ta(., reactive_data()), ~ .) %>%
      purrr::when("ema_ta" %in% input$ta ~ plot_ema_ta(., reactive_data()), ~ .)
  })

  output$sec_title <- renderText({
    all_sec_df %>% filter(secid == input$sec) %>% pull(name) %>% enc2native()
  })
}

shinyApp(ui, server)
