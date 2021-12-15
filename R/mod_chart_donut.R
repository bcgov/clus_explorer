#' mod_chart_donut UI Function
#'
#' @description A shiny Module for plotly donut chart.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_donut_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_donut'), height = chart_height)
  )
}

#' mod_chart_donut Server Function for plotly donut chart
#'
#' @noRd
mod_chart_donut_server <- function(
  id, dataset, labels, values, trace_name = '', marker_colors, traces = list(),
  label = NULL, add_auto_text = TRUE, sort = TRUE
){
  moduleServer( id, function(input, output, session){
    # browser()
    ns <- session$ns

    labels <- rlang::enquo(labels)
    values <- rlang::enquo(values)
    label <- rlang::enquo(label)

    p <- plotly::plot_ly(
      dataset,
      labels = labels,
      values = values,
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
