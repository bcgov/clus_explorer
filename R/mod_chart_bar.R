#' chart_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' chart_bar Server Functions
#'
#' @noRd 
mod_chart_bar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_chart_bar_ui("chart_bar_ui_1")
    
## To be copied in the server
# mod_chart_bar_server("chart_bar_ui_1")
