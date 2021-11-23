#' Get database connection
#'
#' @return
#' @export
getDbConnection <- function() {

  config <- config::get()

  conn <- DBI::dbConnect(
    dbDriver("PostgreSQL"),
    host = config$db$host,
    dbname = config$db$dbname,
    port = config$db$port,
    user = config$db$user,
    password = config$db$password
  )
}

#' Get the results of an SQL query from the database
#'
#' @param sql The SQL query
#' @param params The SQL query parameters
#'
#' @return
#' @export
getTableQuery <- function(sql, params = list()) {
  conn <- getDbConnection()
  data <- dbGetQuery(conn = conn, statement = sql, params = params)
  dbDisconnect(conn)
  data
}

#' Get the results of a spatial query from the PostgreSQL database
#'
#' @param sql The SQL query
#'
#' @return
#' @export
getSpatialQuery <- function(sql) {
  conn <- getDbConnection()
  data <- st_read(conn, query = sql)
  dbDisconnect(conn)
  data
}

#' Get the rasters from a PostGIS table
#'
#' @param srcRaster A character string specifying a PostgreSQL schema and table/view name holding the geometry (e.g., name = c("schema","table"))
#'
#' @return
#' @export
getRasterQuery <- function(srcRaster) {
  conn <- getDbConnection()
  data <- pgGetRast(conn, srcRaster)
  dbDisconnect(conn)
  data
}
