#' page_dashboard_mining UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_mining_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBox(
        "BAU",
        "Footprint",
        icon = icon("industry"),
        # color = "navy"
      ),
      valueBox(
        "0",
        "Jobs",
        icon = icon("industry"),
        # color = "navy"
      ),
      valueBox(
        "0",
        "GDP",
        icon = icon("industry"),
        # color = "navy"
      )
    )
  )
}

#' page_dashboard_mining Server Functions
#'
#' @noRd
mod_page_dashboard_mining_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_mining_ui("page_dashboard_mining_ui_1")

## To be copied in the server
# mod_page_dashboard_mining_server("page_dashboard_mining_ui_1")
