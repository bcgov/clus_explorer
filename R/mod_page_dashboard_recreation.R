#' page_dashboard_recreation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_recreation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBox(
        "BAU",
        "Footprint",
        icon = icon("shoe-prints"),
        # color = "aqua"
      ),
      valueBox(
        "0",
        "Jobs",
        icon = icon("shoe-prints"),
        # color = "aqua"
      ),
      valueBox(
        "0",
        "GDP",
        icon = icon("shoe-prints"),
        # color = "aqua"
      )
    )
  )
}

#' page_dashboard_recreation Server Functions
#'
#' @noRd
mod_page_dashboard_recreation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_recreation_ui("page_dashboard_recreation_ui_1")

## To be copied in the server
# mod_page_dashboard_recreation_server("page_dashboard_recreation_ui_1")
