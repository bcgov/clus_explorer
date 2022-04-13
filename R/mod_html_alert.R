#' html_alert UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_html_alert_ui <- function(id, type = 'neutral', message = 'No data found'){
  ns <- NS(id)
  tagList(
    div(
      class = paste0('alert clus-alert clus-alert-', type),
      message
    )
  )
}

#' html_alert Server Functions
#'
#' @noRd
mod_html_alert_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
