#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param embed_url Base URL to embed chart.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(..., embed_url = "http://embed.org") {

  # connection
  con <- connect()
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
      onStart = function(){
        onStop(function(){
          disconnect(con)
        })
      }
    ),
    golem_opts = list(con = con, embed_url = embed_url, ...)
  )
}
