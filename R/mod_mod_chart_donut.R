#' mod_chart_donut UI Function
#'
#' @description A shiny Module for plotly donut chart.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_chart_donut_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_donut'), height = chart_height)
  )
}
    
#' mod_chart_donut Server Function for plotly donut chart
#'
#' @noRd 
mod_mod_chart_donut_server <- function(
  id, dataset, x, y, trace_name = '', traces = list(),
  label = NULL, add_auto_text = TRUE, marker_colors, sort = TRUE
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    label <- rlang::enquo(label)
    
    p <- plotly::plot_ly(
      dataset,
      labels = x,
      values = y,
      textinfo = 'percent',
      hovertemplate = paste0("%{label}<br>%{text:,} (<b>%{value:.1%f}</b>)<extra></extra>"),
      text = label,
      textfont = list(family = plotly_presets$font$family, size = 13),
      direction = 'clockwise',
      sort = sort,
      opacity = 0.75,
      marker = list(
        colors = marker_colors,
        line = list(color = '#FFFFFF', width = 1)
      )
    ) %>%
      plotly::add_pie(hole = 0.6
      ) %>%
      plotly::layout(
        showlegend = TRUE,
        legend = plotly_presets$legend_format,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = plotly_presets$remove_buttons
      )
    
    output$chart_donut <- plotly::renderPlotly({
      p
    })
    
  })
}
    
## To be copied in the UI
# mod_mod_chart_donut_ui("mod_chart_donut_ui_1")
    
## To be copied in the server
# mod_mod_chart_donut_server("mod_chart_donut_ui_1")
