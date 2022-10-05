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
      axis.text.x  = element_text(colour = "#52606D", size = 7, angle =45 ),
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
#' @param facet_scales Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param facet_nrow	Number of facet rows
#' @param facet_ncol	Number columns.
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param is_plotly Should the chart object be converted to plotly object
#' @param add_x_intercept Whether to add vertical intercept line
#' @param x_intercept Position of vertical intercept line
#' @param add_y_intercept Whether to add horizontal intercept line
#' @param y_intercept Position of vertical horizontal line
#' @param legend_position Legend position
#' @param strip_position Facet strip position
#' @param labeller_data Facet labeller data
#' @param height Chart height
#'
#' @return
#'
#' @import ggplot2
#' @import plotly
#' @export
chart_line_faceted <- function(
  data, x_var, y_var, color_var, facet_chart = FALSE, facet_vars = NULL, facet_scales = 'free',
  facet_nrow = 3, facet_ncol = 3, xlab = "", ylab = "", is_plotly = FALSE,
  add_x_intercept = FALSE, x_intercept = 0,
  add_y_intercept = FALSE, y_intercept = 0,
  legend_position = "bottom", strip.position = "top", labeller_data = c(), height = 600
) {

  p <-
    ggplot(
      data,
      aes (
        x = {{ x_var }},
        y = {{ y_var }},
        color = {{ color_var }},
        linetype = {{ color_var }}
      )
    ) +
    geom_line() +
    xlab (xlab) +
    ylab (ylab) +
    scale_x_continuous(
      limits = c(0, 50),
      breaks = seq(0, 50, by = 10)
    ) +
    scale_y_continuous(
      labels = scales::comma_format(big.mark = ",", decimal.mark = ".")
    ) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = legend_position
    )

  if (facet_chart) {
    # browser()
    if (length(labeller_data) == 0) {
      labeller_data <- data %>%
        distinct({{ facet_vars }})
    }

    p <- p +
      facet_wrap(
        facets = vars({{ facet_vars }}),
        ncol = facet_ncol,
        nrow = facet_nrow,
        scales = facet_scales,
        strip.position = strip.position,
        labeller = as_labeller(labeller_data)
      ) +
      ggplot2::theme(panel.spacing = unit(0.5, "cm"))
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
    p <- plotly::ggplotly(p, height = height) %>%
      plotly::layout (
        legend = list (orientation = "h", y = -0.1),
        margin = list (
          l = 50,
          r = 20,
          b = 40,
          t = 40,
          pad = 0
        )
      )
  }

  p
}

#' Reusable function to create a bar chart
#'
#' @param data Data to be plotted
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param facet_chart Should the chart be faceted
#' @param facet_vars Facet variables
#' @param xlab X-axis label
#' @param facet_nrow	Number of facet rows
#' @param facet_ncol	Number columns.
#' @param ylab Y-axis label
#' @param is_plotly Should the chart object be converted to plotly object
#' @param legend_position Legend position
#' @param scale_x_continuous_limits Character vector of scale X limits
#' @param scale_x_continuous_breaks Character vector of scale X breaks
#' @param height Chart height
#'
#' @return
#' @export
#'
#' @examples
chart_bar_faceted <- function(
  data, x_var, y_var, facet_chart = FALSE, facet_vars = NULL, facet_nrow = 3,
  facet_ncol = 3, xlab = "", ylab = "", is_plotly = FALSE, scale_x_continuous_limits = c(),
  scale_x_continuous_breaks = c(), legend_position = "bottom", height = 600
){
  p <-
    ggplot(
      data,
      aes (
        x = {{ x_var }},
        y = {{ y_var }}
      )
    ) +
    geom_bar(stat = "identity", width = 1) +
    xlab (xlab) +
    ylab (ylab) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = legend_position
    )

  if (facet_chart) {
    p <- p +
      facet_wrap(
        facets = vars({{ facet_vars }}),
        nrow = facet_nrow,
        ncol = facet_ncol
      )
  }

  if (length(scale_x_continuous_limits) > 0 & length(scale_x_continuous_breaks) > 0) {
    p <- p +
      scale_x_continuous(
        limits = scale_x_continuous_limits,
        breaks = scale_x_continuous_breaks
      )
  }

  p <- p +
    scale_y_continuous(
      labels = scales::comma_format(big.mark = ",", decimal.mark = ".")
    )


  if (is_plotly) {
    p <- plotly::ggplotly(p, height = height) %>%
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

