#' mod_chart_line UI Function
#'
#' @description A shiny Module for plolty line chart.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' 
#' @export
mod_mod_chart_line_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_line'), height = chart_height)
  )
}
    
#' mod_chart_line Server Function for plotly line chart
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param dataset dataset to be plotted
#' @param x X coordinate variable
#' @param y Y coordinate variable
#' @param trace_name Trace name
#' @param chart_type Chart type
#' @param chart_mode Chart mode
#' @param traces Traces
#' @param marker_color Marker colour
#' @param line_shape Line shape
#' @param line_fill Line fill
#' @param line_fillcolor Line fill colour
#' @param x_axis_tick_format X axis tick format
#' @param x_axis_nticks X axis nticks
#' @param legend_y Legend Y
#' @param x_axis_tick_prefix X axis tick prefix
#' @param y_axis_tick_format Y axis tick format
#' @param y_axis_tick_prefix Y axis tick prefix
#' @param shapes Shapes to add to the plot
#' @param add_annotations Add annotations
#' @param annotation_text Annotation text
#' @param annotation_x Annotation text X coordinate
#' @param annotation_y Annotation text Y coordinate
#' @param annotation_legend Show annotation legend or not
#' @param add_loess Add LOESS smoothing or not
#' @param loess_frequency LOESS smoothing frequency
#' @param loess_mean_colour LOESS mean colour
#' @param loess_ribbon_colour LOESS ribbon colour
#' 
#' @import stats
#' @importFrom lubridate year
#' @rdname mod_chart_line
mod_mod_chart_line_server <- function(
  id, dataset, x, y, trace_name = '', chart_type = 'scatter',
  chart_mode = 'marker+lines', traces = list(), marker_color = '#F29B9B',
  line_shape = 'linear', line_fill = 'none', line_fillcolor = 'rgba(160, 129, 217, 0.25)',
  x_axis_tick_format = 'string', x_axis_nticks = 0, legend_y = -0.15, x_axis_tick_prefix = '',
  y_axis_tick_format = 'string', y_axis_tick_prefix = '', shapes = list(), add_annotations = FALSE,
  annotation_text = c(), annotation_x = c(), annotation_y = c(), annotation_legend = FALSE,
  add_loess = FALSE, loess_frequency = 1, loess_mean_colour = '#c9c9c9', loess_ribbon_colour = 'rgba(96, 96, 96, 0.15)'
){
  moduleServer( id, function(input, output, session){
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
    
    # Get variable names ----
    x <- rlang::enquo(x)
    y <- rlang::enquo(y)
    
    # Create initial trace ----
    if (add_loess) {
      # Add LOESS smoothing ----
      min_ts <- dataset %>%
        top_n({{x}}, n = -1) %>%
        pull({{x}}) %>%
        lubridate::year()
      
      date_col <- dataset %>% pull({{x}})
      dataset <- dataset %>% pull({{y}})
      
      dataset_ts <- ts(dataset, start = c(min_ts, 1), frequency = loess_frequency)
      x.info <- attr(dataset_ts, "tsp")
      tt <- seq(from = x.info[1], to = x.info[2], by = 1/x.info[3])
      
      p <- plotly::plot_ly(
        x = date_col,
        y = dataset_ts,
        
        name = trace_name,
        type = chart_type,
        mode = 'lines',
        line = list(
          color = marker_color,
          shape = line_shape
        ),
        fill = line_fill,
        fillcolor = line_fillcolor
      ) %>% plotly::layout(
        xaxis = x_axis_format,
        yaxis = y_axis_format,
        margin = plotly_presets$margin_format,
        legend = plotly_presets$legend_format,
        shapes = shapes
      ) %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = plotly_presets$remove_buttons
        )
    } else {
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
        fill = line_fill,
        fillcolor = line_fillcolor
      ) %>% plotly::layout(
        xaxis = x_axis_format,
        yaxis = y_axis_format,
        margin = plotly_presets$margin_format,
        legend = plotly_presets$legend_format,
        shapes = shapes
      ) %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = plotly_presets$remove_buttons
        )
    }

    if (length(traces) > 0) {
      for (trace in traces) {
        p <- plotly::add_trace(
          p,
          y = trace$y,
          name = trace$trace_name,
          type = chart_type,
          mode = chart_mode,
          line = list(color = trace$color),
          fill = line_fill,
          fillcolor = trace$fillcolor
        )
      }
    }
    
    if (add_annotations) {
      p <- p %>%
        add_text(
          showlegend = annotation_legend,
          x = annotation_x,
          y = annotation_y,
          text = annotation_text,
          hovertext = ''
        )
    }
    
    if (add_loess) {
      ll.smooth = loess(dataset_ts ~ tt, span = 0.75)
      ll.pred = predict(ll.smooth, se = TRUE)
      ll.df = data.frame(
        # x = ll.smooth$x,
        x = date_col,
        fit = round(ll.pred$fit, 2),
        lb = round(ll.pred$fit - (1.96 * ll.pred$se), 2),
        ub = round(ll.pred$fit + (1.96 * ll.pred$se), 2)
      )
      # ll.df = ll.df[order(ll.df$tt), ]
      
      p <- p %>%
        plotly::add_lines(
          x = date_col,
          y = ll.pred$fit,
          name = "Mean",
          line = list(color = loess_mean_colour, width = 2, mode = 'lines')
        )
      p <- p %>%
        plotly::add_ribbons(
          x = date_col,
          ymin = ll.df$lb, ymax = ll.df$ub,
          name = "95% CI",
          line = list(opacity = 0.4, width = 0, color = loess_ribbon_colour,  mode = 'lines'),
          fillcolor = loess_ribbon_colour
        )
    }
    
    output$chart_line <- plotly::renderPlotly({
      p
    })
    
  })
}
    
## To be copied in the UI
# mod_mod_chart_line_ui("mod_chart_line_ui_1")
    
## To be copied in the server
# mod_mod_chart_line_server("mod_chart_line_ui_1")
