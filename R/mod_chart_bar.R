#' chart_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_bar_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("bar"), height = chart_height)
  )
}

#' chart_bar Server Functions
#'
#' @noRd
mod_chart_bar_server <- function(
  id, data, x, y, trace_name = '', traces = list(),
  label = NULL, flip_axis = FALSE, add_auto_text = TRUE, marker_color = '#F29B9B',
  bargap = 0.5, barmode = 'stack', legend_y = -0.1,
  tick_format = '', tick_format_x = '', tick_format_y = '',
  text_template = '', add_auto_text_stack = FALSE, exponentformat = 'B',
  text_position = 'none'
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Chart formatting
    fontFamily <- "Inconsolata, Karla, 'Open Sans', sans-serif"
    tickFont <- list(family = fontFamily, size = 14)
    textFont <- list(family = fontFamily, color = '#1F2933', size = 13)
    textTemplate <- '%{y:,}'
    if (text_template == 'currency') {
      textTemplate <- '%{y:$,.1f}'
    } else if (text_template == 'percentage') {
      textTemplate <- '%{y:.1%f}'
    }

    xAxisFormat <- yAxisFormat <- axisFormat <- list(
      title = "",
      showticklabels = TRUE,
      separatethousands = TRUE,
      tickfont = tickFont,
      color = '#3E4C59'
    )

    tickFormat <- tick_format
    if (tick_format == 'integer') {
      tickFormat <- ','
      exponentformat = 'B'
    } else if (tick_format == 'currency') {
      tickFormat <- '$,.1f'
      exponentformat = 'B'
    } else if (tick_format == 'percentage') {
      tickFormat <- '.1%f'
      exponentformat = 'B'
    } else if (tick_format == 'date') {
      tickFormat <- "%B<br>%Y"
      exponentformat = 'B'
    }

    # tickFormatX <- tick_format
    # if (tick_format_x == 'integer') {
    #   tickFormatX <- ','
    #   exponentformat = 'B'
    # } else if (tick_format_x == 'currency') {
    #   tickFormatX <- '$,.1f'
    #   exponentformat = 'B'
    # } else if (tick_format_x == 'percentage') {
    #   tickFormatX <- '.1%f'
    #   exponentformat = 'B'
    # } else if (tick_format_x == 'date') {
    #   tickFormatX <- "%B<br>%Y"
    #   exponentformat = 'B'
    # }
    #
    # tickFormatY <- tick_format
    # if (tick_format_y == 'integer') {
    #   tickFormatY <- ','
    #   exponentformat = 'B'
    # } else if (tick_format_y == 'currency') {
    #   tickFormatY <- '$,.1f'
    #   exponentformat = 'B'
    # } else if (tick_format_y == 'percentage') {
    #   tickFormatY <- '.1%f'
    #   exponentformat = 'B'
    # } else if (tick_format_y == 'date') {
    #   tickFormatY <- "%B<br>%Y"
    #   exponentformat = 'B'
    # }

    # xAxisFormat$tickformat <- tickFormatX
    xAxisFormat$tickformat <- tickFormat
    xAxisFormat$exponentformat <- exponentformat
    xAxisFormat$automargin <- TRUE
    # yAxisFormat$tickformat <- tickFormatY
    yAxisFormat$tickformat <- tickFormat
    yAxisFormat$exponentformat <- exponentformat
    yAxisFormat$automargin <- TRUE

    marginFormat <- list(
      l = 50,
      r = 50,
      b = ifelse(length(traces) > 0, 50, 10),
      t = 20,
      pad = 4
    )

    legendFormat <- list(
      font = list(
        family = fontFamily,
        size = 11,
        color = "#393939"
      ),
      bordercolor = "#e6e6e6",
      borderwidth = 1,
      bgcolor = "rgba(255, 255, 255, 0.5)",
      orientation = 'h',
      x = 0,
      y = legend_y,
      traceorder = 'normal'
      # xanchor = "center",  # use center of legend as anchor
    )

    x <- rlang::enquo(x)
    y <- rlang::enquo(y)

    z = ''
    # text template
    textTemplate <- '%{y:,}'
    if (text_template == 'currency') {
      textTemplate <- '%{y:$,.1f}'
    } else if (text_template == 'percentage') {
      textTemplate <- '%{y:.1%f}'
    }

    if (add_auto_text == TRUE) {

      z <- y

      if (flip_axis) {
        z <- x
        # text template
        textTemplate <- '%{x:,}'
        if (text_template == 'currency') {
          textTemplate <- '%{x:$,.1f}'
        } else if (text_template == 'percentage') {
          textTemplate <- '%{x:.1%f}'
        }
      }
      if (text_position == 'none') {
        text_position = 'auto'
      }
    }

    hoverTemplate <- paste0(trace_name, "<br>", "%{x}: <b>%{y}</b><extra></extra>")
    if (flip_axis) {
      hoverTemplate <- paste0(trace_name, "<br>", "%{y}: <b>%{x}</b><extra></extra>")
    }

    p <- plotly::plot_ly(
      data,
      x = x,
      y = y,
      name = trace_name,
      type = "bar",
      text = z,
      textposition = text_position,
      textfont = textFont,
      insidetextfont = textFont,
      outsidetextfont = textFont,
      cliponaxis = FALSE,
      texttemplate = textTemplate, # "%{x:.%}",
      hovertemplate = hoverTemplate,
      marker = list(color = marker_color),
      hoverinfo = "x+y+name",
      orientation = dplyr::if_else(flip_axis, 'h', 'v'),
      opacity = 0.75
    ) %>%
      plotly::layout(
        # title = PlotlyChartTitle(title_text = paste("Number of transactions in", ptGeoNameLabel())),
        xaxis = xAxisFormat,
        yaxis = yAxisFormat,
        margin = marginFormat,
        barmode = barmode,
        bargap = bargap,
        legend = legendFormat
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "select2d", "lasso2d", "hoverClosestCartesian", "zoomIn3d",
          "zoomOut3d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian"
        )
      )

    if (length(traces) > 0) {
      for (trace in traces) {
        # trace_x <- rlang::sym(trace$x)
        # trace_y <- rlang::sym(trace$y)

        hoverTemplate <- paste0(trace$trace_name, "<br>", "%{x}: <b>%{y}</b><extra></extra>")
        if (flip_axis) {
          hoverTemplate <- paste0(trace$trace_name, "<br>", "%{y}: <b>%{x}</b><extra></extra>")
        }

        p <- plotly::add_trace(
          p,
          y = trace$y,
          x = trace$x,
          name = trace$trace_name,
          text = trace$label,
          textposition = if_else(add_auto_text_stack == TRUE, text_position, 'none'),
          hovertemplate = hoverTemplate,
          marker = list(color = trace$color)
          #evaluate = TRUE
        )
      }
    }

    output$bar <- plotly::renderPlotly({
      p
    })

  })
}

## To be copied in the UI
# mod_chart_bar_ui("chart_bar_ui_1")

## To be copied in the server
# mod_chart_bar_server("chart_bar_ui_1")
