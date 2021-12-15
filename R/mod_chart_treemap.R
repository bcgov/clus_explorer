#' chart_treemap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param chart_height Chart height (in pixels)
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_chart_treemap_ui <- function(id, chart_height = 400){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns('treemap'), height = chart_height)
  )
}

#' chart_treemap Server Function
#'
#' @noRd
mod_chart_treemap_server <- function(
  id, data, col_child, col_parent, col_value, col_label,
  chart_type = 'treemap',
  text_info = "label+text+value+percent root",
  colors = c("#A7D8F0", "#F29B9B", "#F8E3A3"),
  tiling_packing = 'dice-slice'
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

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

      col_parent <- rlang::enquo(col_parent)
      col_child <- rlang::enquo(col_child)
      col_value <- rlang::enquo(col_value)
      col_label <- rlang::enquo(col_label)
      p <- data %>%
        plotly::plot_ly(
          opacity = 0.9,
          parents = col_parent,
          values = col_value,
          labels = col_child,
          ids = col_child,
          type = chart_type,
          domain = list(column = 0),
          branchvalues = 'total',
          marker = list(
            pad = list(t = 5, l = 5, r = 5, b = 5)#,
            # depthfade = FALSE,
            # colors = colors
          ),
          textfont = list(
            family = fontFamily
          ),
          # insidetextorientation = 'radial',
          # text = c(
          #   'No cert', 'Post-secondary', 'Secondary', 'Apprenticeship', 'College', 'Uni cert below BSc',
          #   'Uni cert at BSc+', 'Cert', 'Trades cert', 'BSc', 'MD', 'PhD', 'MSc', 'Uni cert at BSc or +'
          # ),
          textinfo = text_info,
          hoverlabel = list(font = list(family = fontFamily)),
          hoverinfo = text_info,
          hovertemplate = paste0("%{label}<br><b>%{value:,}</b><extra></extra>"),
          tiling = list(
            pad = 5,
            packing = tiling_packing
            # flip = 'x+y'),
          ),
          pathbar = list(
            visible = TRUE,
            thickness = 20,
            textfont = list(family = fontFamily)
          )
        )

      p <- p %>%
        plotly::layout(
          uniformtext = list(minsize = 10, mode = 'hide'),
          colorway = colors,
          margin=list(l=0, r=0, b=0, t=0)
        ) %>%
        plotly::config(displaylogo = FALSE)

      output$treemap <- plotly::renderPlotly({
        p
      })
    })
}
