#' page_dashboard_caribou UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_caribou_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

      tabPanel(
        "Disturbance",

        h4("Proportion Disturbed"),
        plotlyOutput(outputId = ns("propDisturbPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff'),

        h4("Proportion Disturbed with 500m Buffer"),
        plotlyOutput(outputId = ns("propDisturbBuffPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      ),

      tabPanel(
        "Forest Age",

        h4("Early Forest"),
        plotlyOutput(outputId = ns("propEarlyPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff'),

        h4("Mature Forest"),
        plotlyOutput(outputId = ns("propMaturePlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff'),

        h4("Old Forest"),
        plotlyOutput(outputId = ns("propOldPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      ),

      tabPanel(
        "Population",

        h4("Abundance (Southern Group of Southern Mountain Caribou Only)"),
        plotlyOutput(outputId = ns("abundancePlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff'),
        shiny::tags$small(
          '"Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest."'
        ),

        h4("Survival (Southern Group of Southern Mountain Caribou Only)"),
        plotlyOutput(outputId = ns("survivalPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      ),

      tabPanel(
        "Resource Selection",

        h4("Resource Selection"),
        plotlyOutput(outputId = ns("rsfPlot"), height = "600px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      )
    )
  )
}

#' page_dashboard_caribou Server Functions
#'
#' @noRd
mod_page_dashboard_caribou_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (nrow(reportList()$disturbance) > 0) {
      output$propDisturbPlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          data <- reportList()$disturbance
          chart_line_faceted(
            data = data,
            x_var = timeperiod,
            y_var = (dist_per * 100),
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = critical_hab,
            xlab = "Future year",
            ylab = "Percent Disturbed",
            is_plotly = TRUE
          )
        })
      })

      output$propDisturbBuffPlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          chart_line_faceted(
            data = reportList()$disturbance,
            x_var = timeperiod,
            y_var = (dist500_per * 100),
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = critical_hab,
            xlab = "Future year",
            ylab = "Percent Disturbed",
            is_plotly = TRUE
          )
        })
      })
    }

    if (nrow(reportList()$survival) > 0) {
      output$propEarlyPlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          chart_line_faceted(
            data = reportList()$survival,
            x_var = timeperiod,
            y_var = prop_age,
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            xlab = "Future year",
            ylab = "Proportion Age 0 to 40 years",
            is_plotly = TRUE
          )
        })
      })

      output$propMaturePlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          chart_line_faceted(
            data = reportList()$survival,
            x_var = timeperiod,
            y_var = prop_mature,
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            xlab = "Future year",
            ylab = "Proportion Age 80 to 120 years",
            is_plotly = TRUE
          )
        })
      })

      output$propOldPlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          chart_line_faceted(
            data = reportList()$survival,
            x_var = timeperiod,
            y_var = prop_old,
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            xlab = "Future year",
            ylab = "Proportion > 120 years",
            is_plotly = TRUE
          )
        })
      })

      output$survivalPlot <- renderPlotly ({
        withProgress(message = 'Making Plots', value = 0.1, {
          data <- reportList()$survival
          data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, herd_bounds)]  # replace first() with shift() to get difference with previous year value instead of first year value

          chart_line_faceted(
            data = data,
            x_var = timeperiod,
            y_var = survival_rate_change,
            color_var = scenario,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            xlab = "Future year",
            ylab = "Change in Annual Adult Female Survival Rate",
            is_plotly = TRUE
          )
        })
      })
    }

    if (!is.null(reportList()$abundance)) {
      if (nrow(reportList()$abundance) > 0) {
        output$abundancePlot <- renderPlotly ({
          withProgress(message = 'Making Plots', value = 0.1, {
            chart_line_faceted(
              data = reportList()$abundance,
              x_var = timeperiod,
              y_var = abundance_avg,
              color_var = scenario,
              facet_chart = TRUE,
              facet_vars = subpop_name,
              xlab = "Future year",
              ylab = "Abundance",
              is_plotly = TRUE
            )
        })
      })
      }
    }

    if (nrow(reportList()$rsf) > 0) {
      output$rsfPlot <- renderPlotly ({
        data <- as.data.table(reportList()$rsf)

        data[, rsf_perc_change := ((first(sum_rsf_hat) - sum_rsf_hat) / first(sum_rsf_hat) * 100), by = .(scenario, rsf_model)]  # replace first() with shift() to get difference with previous year value instead of first year value
        chart_line_faceted(
          data = data,
          x_var = timeperiod,
          y_var = rsf_perc_change,
          color_var = scenario,
          facet_chart = TRUE,
          facet_vars = rsf_model,
          add_y_intercept = TRUE,
          xlab = "Future year",
          ylab = "RSF Value Percent Change",
          is_plotly = TRUE
        )
      })
    }

  })

}
