# Module UI

#' @title   mod_chart_area_ui and mod_chart_area_server
#' @description A shiny Module for plotly stacked area chart.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_chart_area
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_chart_area_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_area'), height = chart_height)
  )
}

# Module Server

#' @rdname mod_chart_area
#' @export
#' @keywords internal
#' @export
mod_chart_area_server <- function(
  id, dataset, x, y, trace_name = '', chart_type = 'scatter', stackgroup = 'one',
  chart_mode = 'markers+lines', traces = list(), marker_color = '#F29B9B',
  line_shape = 'linear', line_fill = 'none', line_fillcolor = 'rgba(160, 129, 217, 0.25)',
  x_axis_tick_format = '', x_axis_nticks = 0, legend_y = -0.1, x_axis_tick_prefix = '',
  y_axis_tick_format = '', y_axis_tick_prefix = ''
){
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Chart formatting ----
    x_tick_format <- ''
    exponentformat = 'B'
    x_tick_format <- switch(
      x_axis_tick_format,
      'string' = plotly_presets$tick$format$string,
      'integer' = plotly_presets$tick$format$integer,
      'currency' = plotly_presets$tick$format$currency,
      'percentage' = plotly_presets$tick$format$percentage,
      'date' = plotly_presets$tick$format$date
    )
    
    y_tick_format <- switch(
      y_axis_tick_format,
      'string' = plotly_presets$tick$format$string,
      'integer' = plotly_presets$tick$format$integer,
      'currency' = plotly_presets$tick$format$currency,
      'percentage' = plotly_presets$tick$format$percentage,
      'date' = plotly_presets$tick$format$date
    )
    
    x_axis_format <- plotly_presets$axis_x
    x_axis_format$tickformat = x_tick_format
    x_axis_format$tickprefix = x_axis_tick_prefix
    
    y_axis_format <- plotly_presets$axis_y
    y_axis_format$tickformat = y_tick_format
    y_axis_format$tickprefix = y_axis_tick_prefix
    
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    
    p <- plotly::plot_ly(
      dataset,
      x = x,
      y = y,
      
      name = trace_name,
      type = chart_type,
      mode = chart_mode,
      line = list(
        color = marker_color,
        shape = line_shape
      ),
      stackgroup = stackgroup,
      fillcolor = line_fillcolor
      
    ) %>%
      plotly::layout(
        xaxis = x_axis_format,
        yaxis = y_axis_format,
        margin = plotly_presets$margin_format,
        legend = plotly_presets$legend_format,
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = plotly_presets$remove_buttons
      )
    
    if (length(traces) > 0) {
      for (trace in traces) {
        p <- plotly::add_trace(
          p,
          y = trace$y,
          name = trace$trace_name,
          line = list(color = trace$color),
          fillcolor = trace$fillcolor
        )
      }
    }
    
    output$chart_area <- plotly::renderPlotly({
      p
    })
  })
}
