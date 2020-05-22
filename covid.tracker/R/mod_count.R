#' count UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param class Class to apply to countup.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_count_ui <- function(id, cl = ""){
  ns <- NS(id)
  div(
    class = paste("panel panel-default", cl),
    div(
      class = "panel-body centerize",
      uiOutput(ns("title")),
      h4(countup::countupOutput(ns("count"))),
      tags$small("confirmed cases"),
      reactrend::reactrendOutput(ns("trend"))
    )
  )
}
    
#' count Server Function
#'
#' @noRd 
mod_count_server <- function(input, output, session, n, title, total = NULL){
  ns <- session$ns

  output$title <- renderUI({
    h5(title)
  })

  output$count <- countup::renderCountup({
    if(is.null(total))
      countup::countup(sum(n))
    else
      countup::countup(total)
  })

  output$trend <- reactrend::renderReactrend({
    pal <- sequential_palette[2:6]

    if(title == "Europe")
      pal <- c("#feedde", "#ffffff")

    reactrend::reactrend(
      cumsum(n),
      gradient = pal,
      smooth = TRUE,
      draw = TRUE,
      draw_easing = "ease-out"
    )
  })
 
}
    
## To be copied in the UI
# mod_count_ui("count")
    
## To be copied in the server
# callModule(mod_count_server, "count")
 
