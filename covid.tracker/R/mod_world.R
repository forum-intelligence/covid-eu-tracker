#' world UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_world_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("A European Timeline"),
    p("The tool below illustrates the timing between the first reported case and a subsequent increase in reported cases in Europe and select countries."),
    fluidRow(
      column(10, uiOutput(ns("countries_selected_generated"))),
      column(
        2,
        br(),
        tags$button(
          type = "button", class = "btn btn-default collapsible2", 
          style = "width: 100%;margin:4px;",
          "More chart options"
        )
      )
    ),
    div(
      id = "content2",
      class = "panel",
      p(class = "labels", "Customise the variables to plot."),
      fluidRow(        
        column(
          4,
          shinyWidgets::radioGroupButtons(
            inputId = ns("days"),
            label = "X Axis", 
            choices = c("Date", "Days Since 100th Case"),
            justified = TRUE,
            selected = "Days Since 100th Case"
          )
        ),
        column(
          8,
          shinyWidgets::radioGroupButtons(
            inputId = ns("cumul"),
            label = "Y Axis", 
            choices = c("Cumulative Cases", "New Cases", "Cumulative Deaths", "New Deaths"),
            justified = TRUE
          )
        )
      ),
      div(
        class = "norm",
        p(class = "labels", "Transform variables on the y axis."),
        fluidRow(
          column(
            6,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("capita"),
              label = "Normalisation", 
              choices = c("Cases", "Cases per 1 million inhabitant"),
              inline = TRUE, 
              status = "warning",
              icon = icon("check")
            )
          ),
          column(
            6,
            shinyWidgets::prettyRadioButtons(
              inputId = ns("scale"),
              label = "Scaling", 
              choices = c("Linear", "Logarithmic"),
              inline = TRUE, 
              status = "warning",
              icon = icon("check")
            )
          )
        )
      )
    ),  
    uiOutput(ns("help")),
    div(
      class = "panel panel-default",
      div(
        class = "panel-body",
        echarts4r::echarts4rOutput(ns("plot"), height = "70vh"),
        br(),
        p("Embed this chart with the code below"),
        uiOutput(ns("copy"))
      )
    ),
    br()
  )
}
    
#' world Server Function
#'
#' @noRd 
mod_world_server <- function(input, output, session, eu){
  ns <- session$ns

  europe <- eu %>% 
    dplyr::filter(continent == "Europe" | country %in% c("United Kingdom", "Italy", "Turkey")) %>% 
    dplyr::mutate(country = "Europe") %>% 
    dplyr::group_by(country, date_time) %>% 
    dplyr::summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE),
      population = sum(population, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  eu <- dplyr::bind_rows(eu, europe)

  output$help <- renderUI({
    is_log <- ifelse(input$scale == "Linear", FALSE, TRUE)
    is_cumul <- ifelse(input$cumul == "New Cases", FALSE, TRUE)
    per_capita <- ifelse(input$capita == "Cases", FALSE, TRUE)
    date <- ifelse(input$days == "Date", TRUE, FALSE)

    cases <- "number of daily new cases"

    if(is_cumul)
      cases <- "cumulative number of cases"

    if (per_capita)
      cases <- paste0(cases, " per 1 million inhabitant")

    dsc <- paste0("The chart displays the ", cases, " in function of ")
    
    if (date)
      dsc <- paste0(dsc, " the date.")
    else
      dsc <- paste0(dsc, " the number of days since a country has recorded its 100th confirmed case of Covid-19 infection.")
    
    if(is_log)
      dsc <- paste0(dsc, " The log scale applies a log10(y + 1) transformation to every daily value, which allows a better comparison between countries at the beginning of the spread.")

    p(dsc)
  })

  output$copy <- renderUI({
    req(input$countries_selected)
    regions <- paste0(tolower(input$countries_selected), collapse = "|")
    metric <- ifelse(input$cumul == "Cumulative Cases", "cumulative", "new-cases")
    per_capita <- ifelse(input$capita == "Cases", FALSE, TRUE) %>% tolower()
    date <- ifelse(input$days == "Date", TRUE, FALSE) %>% tolower()
    metric <- tolower(input$cumul)
    metric <- gsub(" ", "-", metric)
    
    params <- paste0("?chart=country&scale=", input$scale, "&regions=", regions, "&metric=", metric, "&capita=", per_capita, "&date=", date)
    url <- copy_to_clipboard_url(params)
    copy_to_clipboard_ui(url)
  })

  output$plot <- echarts4r::renderEcharts4r({
    req(input$countries_selected)
    is_log <- ifelse(input$scale == "Linear", FALSE, TRUE)
    is_cumul <- ifelse(input$cumul %in% c("New Cases", "New Deaths"), FALSE, TRUE)
    per_capita <- ifelse(input$capita == "Cases", FALSE, TRUE)
    date <- ifelse(input$days == "Date", TRUE, FALSE)
    var <- ifelse(grepl("Deaths", input$cumul), "deaths", "cases")
    country_trend(eu, input$countries_selected, log = is_log, cumul = is_cumul, per_capita = per_capita, date = date, var = var)
  })
  
  output$countries_selected_generated <- renderUI({
    cns <- eu %>% 
      dplyr::filter(continent == "Europe" | country == "United Kingdom" | country == "Europe") %>% 
      dplyr::distinct(country) %>% 
      dplyr::arrange(country) %>% 
      dplyr::pull(country)
    
    selectizeInput(
      ns("countries_selected"),
      "Select one or multiple countries to view respective timelines",
      choices = cns,
      selected = c("Europe", "Italy", "Spain"),
      width = "100%",
      multiple = TRUE
    )
  })
 
}

