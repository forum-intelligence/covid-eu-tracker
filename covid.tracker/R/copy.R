copy_to_clipboard_ui <- function(url){
  id <- paste0(sample(letters, 26), collapse = "")
  div(
    class = "input-group",
    tags$input(
      type = "text",
      class = "form-control",
      value = url,
      id = id
    ),
    span(
      class = "input-group-btn",
      tags$button(
        class = "btn btn-default",
        type = "button",
        onClick = paste0("copyToClipBoard('", id, "')"),
        "Copy"
      )
    )
  )
}

copy_to_clipboard_url <- function(params){
  params <- tolower(params)
  base_url <- golem::get_golem_options("embed_url")
  base_url %>% paste0(params) %>% utils::URLencode()
}