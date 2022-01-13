#' page_dashboard_oil_and_gas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_oil_and_gas_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBox(
        "BAU",
        "Footprint",
        icon = icon("bolt"),
        # color = "navy"
      ),
      valueBox(
        "0",
        "Jobs",
        icon = icon("bolt"),
        # color = "navy"
      ),
      valueBox(
        "0",
        "GDP",
        icon = icon("bolt"),
        # color = "navy"
      )
    )
  )
}

#' page_dashboard_oil_and_gas Server Functions
#'
#' @noRd
mod_page_dashboard_oil_and_gas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_oil_and_gas_ui("page_dashboard_oil_and_gas_ui_1")

## To be copied in the server
# mod_page_dashboard_oil_and_gas_server("page_dashboard_oil_and_gas_ui_1")
