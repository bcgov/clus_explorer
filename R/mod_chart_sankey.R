#' chart_sankey UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_sankey_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' chart_sankey Server Functions
#'
#' @noRd
mod_chart_sankey_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_chart_sankey_ui("chart_sankey_ui_1")

## To be copied in the server
# mod_chart_sankey_server("chart_sankey_ui_1")
