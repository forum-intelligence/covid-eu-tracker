#' europe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_europe_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("A Closer Look"),
    p(
      "Under the “Country” view below you can hover over each country to see its absolute number of confirmed cases.",
      "By switching to the “Province” view you can hover over individual provinces, regions, and states to see case totals.",
      "The “Focus on Country” button on the right enables a more in-depth view."
    ),
    div(
      class = "panel panel-default",
      div(
        class = "panel-body",
        id = "flippy",
        actionButton(ns("flip"), "Focus on Country", icon = icon("search-plus"), class = "btn-warning"),
        div(
          class = "flip-container",
          div(
            class = "flipper",
            div(
              class = "front",
              shinyWidgets::radioGroupButtons(
                inputId = ns("level"),
                label = "Map",
                choices = c("Country", "Province"),
                justified = TRUE,
                width = "50%",
              ),
              echarts4r::echarts4rOutput(ns("map"), height = "70vh"),
              p("Embed this map with the code below"),
              uiOutput(ns("copy"))
            ),
            div(
              class = "back",
              fluidRow(
                column(
                  6, 
                  selectInput(
                    ns("countriez"),
                    "Country",
                    choices = c("Austria", "Belgium", "Germany", "Italy", "Netherlands", "Norway", "Poland", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"),
                    selected = "Italy",
                    width = "45%"
                  ),
                  echarts4r::echarts4rOutput(ns("counties_map"), height = "70vh"),
                  p("Embed this chart with the code below"),
                  uiOutput(ns("copy_map"))
                ),
                column(
                  6, 
                  echarts4r::echarts4rOutput(ns("counties_bars"), height = "80vh"),
                  p("Embed this map with the code below"),
                  uiOutput(ns("copy_bars"))
                )
              )
            )
          )
        )
      )
    ),
    br()
  )
}
    
#' europe Server Function
#'
#' @noRd 
mod_europe_server <- function(input, output, session, eu, ch, data){
  ns <- session$ns

  observeEvent(input$flip, {
    session$sendCustomMessage("flip-card", list())
  })

  output$copy <- renderUI({
    params <- paste0("?chart=eu&map=", input$level)
    url <- copy_to_clipboard_url(params)
    copy_to_clipboard_ui(url)
  })

  output$copy_map <- renderUI({
    input$countriez
    params <- paste0("?chart=country-chart&country=", tolower(input$countriez))
    url <- copy_to_clipboard_url(params)
    copy_to_clipboard_ui(url)
  })

  output$copy_bars <- renderUI({
    input$countriez
    params <- paste0("?chart=country-map&country=", tolower(input$countriez))
    url <- copy_to_clipboard_url(params)
    copy_to_clipboard_ui(url)
  })

  output$counties_map <- echarts4r::renderEcharts4r({
    echarts_counties_bar(data, input$countriez)
  })

  output$counties_bars <- echarts4r::renderEcharts4r({
    echarts_detail_map(data, input$countriez)
  })

  output$map <- echarts4r::renderEcharts4r({

    is_log <- FALSE

    if(input$level == "Country"){

      eu_countries_map(eu, is_log)

    } else {
      eu_counties_map(data, is_log)
    }

  })

  outputOptions(output, "counties_bars", suspendWhenHidden = FALSE)
  outputOptions(output, "counties_map", suspendWhenHidden = FALSE)
  outputOptions(output, "copy_map", suspendWhenHidden = FALSE)
  outputOptions(output, "copy_bars", suspendWhenHidden = FALSE)
  outputOptions(output, "map", suspendWhenHidden = FALSE)
 
}

 
