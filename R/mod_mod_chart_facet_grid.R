#' mod_chart_facet_grid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_chart_facet_grid_ui <- function(id, chart_height = '600px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_facet_grid'), height = chart_height)
  )
}
    
#' mod_chart_facet_grid Server Functions
#'
#' @param id 
#' @param plots 
#' @param share_x 
#' @param title_x 
#' @param share_y 
#' @param title_y 
#'
#' @noRd 
#' @export
mod_mod_chart_facet_grid_server <- function(
  id, plots = list(), share_x = FALSE, title_x = FALSE, share_y = FALSE, title_y =  FALSE
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    p <- subplot(
      plots, 
      nrows = length(plots), 
      shareX = share_x, 
      titleX = title_x,
      shareY = share_y,
      titleY = title_y
    )
 
    output$chart_facet_grid <- plotly::renderPlotly({
      p
    })
    
  })
}
    
## To be copied in the UI
# mod_mod_chart_facet_grid_ui("mod_chart_facet_grid_ui_1")
    
## To be copied in the server
# mod_mod_chart_facet_grid_server("mod_chart_facet_grid_ui_1")
