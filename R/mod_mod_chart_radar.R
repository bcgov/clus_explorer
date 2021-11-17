#' mod_chart_radar UI Function
#'
#' @description A shiny Module for plotly radar chart.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param chart_height Chart height in pixels
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_chart_radar_ui <- function(id, chart_height = '450px'){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('chart_radar'), height = chart_height)
  )
}

#' mod_chart_radar Server Functions
#'
#' @description A shiny Module for plotly radar chart.
#' @param id ID of the output element defined in the module UI function
#' @param data The dataset to plot
#' @param r Character vector of values that correspond to radial dimension of the chart
#' @param theta Character vector of categorical variables to plot the values against
#' @param traces List of traces to add
#'
#' @noRd
mod_mod_chart_radar_server <- function(id, data, r, theta, traces = list()){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    p <- plot_ly(
        type = 'scatterpolar',
        r = r,
        theta = theta,
        fill = 'toself'
      ) %>% layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,50)
          )
        ),
        showlegend = F
      ) %>%
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = plotly_presets$remove_buttons
      )

    if (length(traces) > 0) {
      for (trace in traces) {
        p <- p %>%
          plotly::add_trace(
            r = r,
            theta = theta,
            name = name
          )
      }
    }

    output$chart_radar <- plotly::renderPlotly({
      p
    })

  })
}
