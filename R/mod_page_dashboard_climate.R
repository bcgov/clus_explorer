#' page_dashboard_climate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_climate_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Conceptual Path Diagram",
      width = 12,
      # background = "yellow",
      solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput(outputId = "climatemap", height = "800px")
    ),
    box(
      title = "Modelled Path Diagram",
      width = 12,
      # background = "yellow",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE
    )
  )
}

#' page_dashboard_climate Server Functions
#'
#' @noRd
mod_page_dashboard_climate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_climate_ui("page_dashboard_climate_ui_1")

## To be copied in the server
# mod_page_dashboard_climate_server("page_dashboard_climate_ui_1")
