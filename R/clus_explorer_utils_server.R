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
getTableQuery <- function(sql, params = list(), conn = NULL) {
  if (is.null(conn)) {
    conn <- getDbConnection()
  }
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

#' Get schema tables from information_schema database
#'
#' @param schema
#'
#' @return
#' @export
getInformationSchemaTables <- function(schema) {
  query <- "SELECT * FROM information_schema.tables
WHERE table_schema = ?"

  getTableQuery(sql = query, params = schema)
}

#' Get available study areas to populate the "Area of interest" dropdown
#'
#' @return
#' @export
getAvailableStudyAreas <- function() {
  query <- "SELECT nspname
FROM pg_catalog.pg_namespace
WHERE nspname NOT IN (
  'pg_toast', 'pg_temp_1', 'pg_toast_temp_1', 'pg_catalog','information_schema', 'topology', 'public'
)"
  areas <- getTableQuery(query)
  area_names <- stringr::str_replace(
    stringr::str_to_title(
      stringr::str_replace_all(
        unlist(areas, use.names = FALSE), '_', ' '
      )
    ),
    'Tsa',
    'TSA'
  )

  available_study_areas <- setNames(as.list(areas$nspname), area_names)
}
