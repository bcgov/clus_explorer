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
        uiOutput(ns("propDisturbPlotUI")),

        h4("Proportion Disturbed with 500m Buffer"),
        uiOutput(ns("propDisturbBuffPlotUI")),
      ),

      tabPanel(
        "Forest Age",

        h4("Early Forest"),
        uiOutput(ns("propEarlyPlotUI")),

        h4("Mature Forest"),
        uiOutput(ns("propMaturePlotUI")),

        h4("Old Forest"),
        uiOutput(ns("propOldPlotUI")),
      ),

      tabPanel(
        "Population",

        h4("Abundance (Southern Group of Southern Mountain Caribou Only)"),
        uiOutput(ns("abundancePlotUI")),


        h4("Survival (Southern Group of Southern Mountain Caribou Only)"),
        uiOutput(ns("survivalPlotUI"))
      ),

      tabPanel(
        "Resource Selection",

        h4("Resource Selection"),
        uiOutput(ns("rsfPlotUI"))
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

    if (!is.null(reportList()$disturbance)) {
      if (nrow(reportList()$disturbance) > 0) {

        indicator_count_disturbance <- length(
          reportList()$disturbance %>%
            distinct(critical_hab) %>%
            pull(critical_hab)
        )
        facet_ncol_disturbance <- 3
        facet_chart_height_disturbance <- 300 * indicator_count_disturbance / facet_ncol_disturbance

        output$propDisturbPlotUI <- renderUI ({
          tagList(
            plotlyOutput(
              outputId = ns("propDisturbPlot"),
              height = facet_chart_height_disturbance
            ) %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          )
        })

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
              facet_ncol = facet_ncol_disturbance,
              facet_nrow = ceiling(
                indicator_count_disturbance / facet_ncol_disturbance
              ),
              facet_scales = 'fixed',
              xlab = "Future year",
              ylab = "Percent Disturbed",
              is_plotly = TRUE,
              height = facet_chart_height_disturbance
            )
          })
        })

        output$propDisturbBuffPlotUI <- renderUI ({
          tagList(
            plotlyOutput(
              outputId = ns("propDisturbBuffPlot"),
              height = facet_chart_height_disturbance
            ) %>%
              withSpinner(
                color = '#ecf0f5',
                color.background = '#ffffff'
              )
          )
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
              facet_ncol = facet_ncol_disturbance,
              facet_nrow = ceiling(
                indicator_count_disturbance / facet_ncol_disturbance
              ),
              facet_scales = 'fixed',
              xlab = "Future year",
              ylab = "Percent Disturbed",
              is_plotly = TRUE,
              height = facet_chart_height_disturbance
            )
          })
        })
      } else {
        output$propDisturbPlotUI <- renderUI({
          mod_html_alert_ui('propDisturbPlot')
        })

        output$propDisturbBuffPlotUI <- renderUI({
          mod_html_alert_ui('propDisturbBuffPlot')
        })
      }
    } else {
      output$propDisturbPlotUI <- renderUI({
        mod_html_alert_ui('propDisturbPlot')
      })

      output$propDisturbBuffPlotUI <- renderUI({
        mod_html_alert_ui('propDisturbBuffPlot')
      })
    }

    if (!is.null(reportList()$survival)) {
      if (nrow(reportList()$survival) > 0) {
        output$propEarlyPlotUI <- renderUI ({
          tagList(
            plotlyOutput(outputId = ns("propEarlyPlot"), height = "600px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff'),
          )
        })

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

        output$propMaturePlotUI <- renderUI ({
          tagList(
            plotlyOutput(outputId = ns("propMaturePlot"), height = "600px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff'),
          )
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

        output$propOldPlotUI <- renderUI ({
          tagList(
            plotlyOutput(outputId = ns("propOldPlot"), height = "600px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          )
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

        output$survivalPlotUI <- renderUI ({
          tagList(
            plotlyOutput(outputId = ns("survivalPlot"), height = "600px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          )
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
      } else {
        output$propEarlyPlotUI <- renderUI({
          mod_html_alert_ui('propEarlyPlot')
        })

        output$propMaturePlotUI <- renderUI({
          mod_html_alert_ui('propMaturePlot')
        })

        output$propOldPlotUI <- renderUI({
          mod_html_alert_ui('propOldPlot')
        })

        output$survivalPlotUI <- renderUI({
          mod_html_alert_ui('survivalPlot')
        })
      }
    } else {
      output$propEarlyPlotUI <- renderUI({
        mod_html_alert_ui('propEarlyPlot')
      })

      output$propMaturePlotUI <- renderUI({
        mod_html_alert_ui('propMaturePlot')
      })

      output$propOldPlotUI <- renderUI({
        mod_html_alert_ui('propOldPlot')
      })

      output$survivalPlotUI <- renderUI({
        mod_html_alert_ui('survivalPlot')
      })
    }

    if (!is.null(reportList()$abundance)) {
      if (!is.null(reportList()$abundance)) {
        output$abundancePlotUI <- renderUI({
          plotlyOutput(outputId = ns("abundancePlot"), height = "600px") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')

          shiny::tags$small(
            '"Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest."'
          )
        })


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
      } else {
        output$abundancePlotUI <- renderUI({
          mod_html_alert_ui('abundancePlot')
        })
      }
    } else {
      output$abundancePlotUI <- renderUI({
        mod_html_alert_ui('abundancePlot')
      })
    }

    if (!is.null(reportList()$rsf)) {
      if (nrow(reportList()$rsf) > 0) {
        output$rsfPlotUI <- renderUI ({
          plotlyOutput(outputId = ns("rsfPlot"), height = "600px") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          p('TYhis is an rsf plot')
        })

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
      } else {
        output$rsfPlotUI <- renderUI({
          mod_html_alert_ui('rsfPlot')
        })
      }
    } else {
      output$rsfPlotUI <- renderUI({
        mod_html_alert_ui('rsfPlot')
      })
    }

  })

}
