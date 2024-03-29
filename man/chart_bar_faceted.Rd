% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clus_explorer_utils_reports.R
\name{chart_bar_faceted}
\alias{chart_bar_faceted}
\title{Reusable function to create a bar chart}
\usage{
chart_bar_faceted(
  data,
  x_var,
  y_var,
  facet_chart = FALSE,
  facet_vars = NULL,
  facet_nrow = 3,
  facet_ncol = 3,
  xlab = "",
  ylab = "",
  is_plotly = FALSE,
  scale_x_continuous_limits = c(),
  scale_x_continuous_breaks = c(),
  legend_position = "bottom",
  height = 600
)
}
\arguments{
\item{data}{Data to be plotted}

\item{x_var}{X-axis variable}

\item{y_var}{Y-axis variable}

\item{facet_chart}{Should the chart be faceted}

\item{facet_vars}{Facet variables}

\item{facet_nrow}{Number of facet rows}

\item{facet_ncol}{Number columns.}

\item{xlab}{X-axis label}

\item{ylab}{Y-axis label}

\item{is_plotly}{Should the chart object be converted to plotly object}

\item{scale_x_continuous_limits}{Character vector of scale X limits}

\item{scale_x_continuous_breaks}{Character vector of scale X breaks}

\item{legend_position}{Legend position}

\item{height}{Chart height}
}
\value{

}
\description{
Reusable function to create a bar chart
}
