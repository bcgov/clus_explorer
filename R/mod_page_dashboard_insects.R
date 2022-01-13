#' page_dashboard_insects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_insects_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBox("0", "Outbreaks", icon = icon("bug"), color = "teal"),
      valueBox("0", "Area spread", icon = icon("bug"), color = "teal"),
      valueBox("0", "Volume Lost", icon = icon("bug"), color = "teal")
    )
  )
}

#' page_dashboard_insects Server Functions
#'
#' @noRd
mod_page_dashboard_insects_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_insects_ui("page_dashboard_insects_ui_1")

## To be copied in the server
# mod_page_dashboard_insects_server("page_dashboard_insects_ui_1")
