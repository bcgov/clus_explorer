#' Function that returns ggplot heatmap chart
#'
#' @param data Data to be plotted
#' @param x X coordinate
#' @param y Y coordinate
#' @param z Z coordinate
#' @param legend_position Legend position ('top', 'right', 'bottom', 'left')
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param caption Chart caption
#' @param n.dodge Dodge parameter
#' @param check.overlap Whether to check for overlapping labels
#'
#' @return
#'
#' @import ggplot2
#' @export
chart_heatmap <- function(
  data, x, y, z, legend_position = 'right', title = '', subtitle = '', caption = '', n.dodge = NULL,
  check.overlap = FALSE
) {
  p <- ggplot(data, aes({{x}}, {{y}}, fill = {{z}})) +
    geom_tile(width = 0.98, height = 0.97) +
    scale_fill_gradientn(colours = c('#C1EAC5', '#7BC47F', '#3F9142')) +
    # scale_fill_distiller(palette = 'Greens', direction = -1) +
    theme_minimal() +
    xlab("") +
    theme_void(base_size = 8) +
    theme(
      legend.position = legend_position,
      legend.title = element_blank(),
      plot.title = element_text(size = 8, colour = '#616E7C'),
      plot.subtitle = element_text(size = 7, colour = '#616E7C', margin = margin(b = 0.25, t = 0.15, unit = "cm")),
      plot.caption = element_text(face = 'italic', size = 6, colour = '#616E7C', hjust = 1, margin = margin(t = 0.5, unit = "cm")),
      # panel.grid.major = element_line(colour = "#E4E7EB", size = 0.2),
      # panel.grid.minor = element_line(colour = "#F5F7FA", size = 0.25),
      axis.text.x  = element_text(colour = "#52606D", size = 7),
      axis.text.y  = element_text(colour = "#52606D", hjust = 1, margin = margin(r = 0.25, unit = "cm"), size = 7)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  if (!is.null(n.dodge)) {
    p <- p +
      scale_x_discrete(guide = guide_axis(n.dodge = n.dodge))
  }

  if (check.overlap) {
    p <- p +
      scale_x_discrete(guide = guide_axis(check.overlap = check.overlap))
  }

  p
}


#' Reusable function to create a line chart
#'
#' @param data Data to be plotted
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param color_var Colour variable
#' @param facet_chart Should the chart be faceted
#' @param facet_vars Facet variables
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param is_plotly Should the chart object be converted to plotly object
#'
#' @return
#'
#' @import ggplot2
#' @import plotly
#' @export
chart_line_faceted <- function(
  data, x_var, y_var, color_var, facet_chart = FALSE, facet_vars = NULL, xlab = "",
  ylab = "", is_plotly = FALSE, legend_position = "bottom",
  add_x_intercept = FALSE, x_intercept = 0,
  add_y_intercept = FALSE, y_intercept = 0
) {

  p <-
    ggplot(data,
           aes (
             x = {{ x_var }},
             y = {{ y_var }},
             color = {{ color_var }},
             linetype = {{ color_var }}
           )) +
    geom_line() +
    xlab (xlab) +
    ylab (ylab) +
    scale_x_continuous(limits = c(0, 50),
                       breaks = seq(0, 50, by = 10)) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = legend_position
    )

  if (facet_chart) {
    p <- p +
      facet_wrap(facets = vars({{ facet_vars }}))
  }

  if (add_x_intercept) {
    p <- p +
      geom_vline(xintercept = x_intercept, linetype = "dashed", color = "#3E4C59")
  }

  if (add_y_intercept) {
    p <- p +
      geom_hline(yintercept = y_intercept, linetype = "dashed", color = "#3E4C59")
  }

  if (is_plotly) {
    p <- plotly::ggplotly(p, height = 600) %>%
      plotly::layout (
        legend = list (orientation = "h", y = -0.1),
        margin = list (
          l = 50,
          r = 40,
          b = 40,
          t = 40,
          pad = 0
        )
      )
  }

  p
}

