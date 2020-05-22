#' Database
#' 
#' Manage database connections for covid.
#' 
#' @param con Connection, as returned by \code{connect}.
#' @param state Whether to connect to dev or prod.
#' 
#' @name connection
#' 
#' @export
connect <- function(state = c("dev", "prod")){
  state <- match.arg(state)

  conf <- read_config()
  conf <- conf[[state]]
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = conf$dbname,
    host = conf$host,
    port = 5432, user = conf$user,
    password = conf$password
  )
}

#' @rdname connection
#' @export
disconnect <- function(con){
  DBI::dbDisconnect(con)
}

write_table <- function(con, name, table){
  DBI::dbWriteTable(con, name, table, overwrite = TRUE)
}
