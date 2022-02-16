#' chart_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param chart_height Chart height in pixels.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_heatmap_ui <- function(id, chart_height = 450){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('heatmap'), height = chart_height)
  )
}

#' chart_heatmap Server Functions
#'
#' @noRd
mod_chart_heatmap_server <- function(
  id, dataset, col_x, col_y, col_z,
  palette = c('#C1EAC5', '#7BC47F', '#3F9142'),
  y_axis_labels = TRUE, hovertemplate = 'x: %{y}<br>y: %{x}<br>z: %{z}'
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    fontFamily <- "Inconsolata, Karla, 'Open Sans', sans-serif"
    tickFont <- list(family = fontFamily, size = 13)

    axisFormat <- list(title = "", showticklabels = TRUE, tickfont = tickFont, color = '#3E4C59')

    marginFormat <- list(
      l = 0,
      r = 0,
      b = 10,
      t = 20,
      pad = 4
    )

    fontFamily <- "Inconsolata, Karla, 'Open Sans', sans-serif"
    tickFont <- list(family = fontFamily, size = 14)
    textFont <- list(family = fontFamily, color = '#1F2933', size = 13)

    xAxisFormat <- axisFormat <- list(
      title = "",
      showticklabels = TRUE,
      separatethousands = TRUE,
      tickfont = tickFont,
      color = '#3E4C59'
    )

    yAxisFormat <- list(
      title = "",
      showticklabels = y_axis_labels,
      separatethousands = TRUE,
      tickfont = tickFont,
      color = '#3E4C59'
    )

    marginFormat <- list(
      l = 20,
      r = 20,
      b = 20,
      t = 10,
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
      y = 0,
      traceorder = 'normal'
      # xanchor = "center",  # use center of legend as anchor
    )

    col_x <- rlang::enquo(col_x)
    col_y <- rlang::enquo(col_y)
    col_z <- rlang::enquo(col_z)

    p <- dataset %>%
      plotly::plot_ly(
        type = 'heatmap',
        # opacity = 0.85,
        x = col_x,
        y = col_y,
        z = col_z,
        connectgaps = FALSE,
        xgap = 1,
        ygap = 1,
        colors = colorRamp(
          palette
        ),
        hovertemplate = hovertemplate,
        colorbar = list(
          bordercolor = '#969696',
          outlinecolor = '#969696',
          bordercolor = '#969696',
          thickness = 15,
          title = list(font = textFont)
        )
      )

    p <- p %>%
      plotly::layout(
        xaxis = xAxisFormat,
        yaxis = yAxisFormat,
        margin = marginFormat,
        legend = legendFormat,
        autosize = TRUE
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d", "select2d", "lasso2d", "hoverClosestCartesian", "zoomIn3d",
          "zoomOut3d", "resetScale2d", "toggleSpikelines", "hoverCompareCartesian"
        )
      )

    output$heatmap <- plotly::renderPlotly({
      p
    })

  })
}
