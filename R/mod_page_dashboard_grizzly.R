#' page_dashboard_grizzly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_grizzly_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

      tabPanel(
        "Adult Female Survival",

        h4("Adult Female Survival"),
        uiOutput(ns("survival_grizzly_af_Plot_UI"))
      ),
      tabPanel(
        "Grizzly Bear Population Unit Road Density",
        h4("Grizzly Bear Population Unit Road Density"),
        uiOutput(ns("road_density_grizzly_Plot_UI"))
      )
    )
  )
}

#' page_dashboard_grizzly Server Functions
#'
#' @noRd
mod_page_dashboard_grizzly_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (!is.null(reportList()$grizzly_survival)) {
      if (nrow(reportList()$grizzly_survival) > 0) {


        output$survival_grizzly_af_Plot_UI <- renderUI({
          plotlyOutput(outputId = ns("road_density_grizzly_Plot"), height = "900px") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')
        })

        output$road_density_grizzly_Plot_UI <- renderUI({
          sliderInput(
            ns("grizzlyYear"),
            label = "Enter Year Range to Plot",
            0,
            200,
            value = c (0, 50),
            step = 5
          )
          plotlyOutput(outputId = ns("survival_grizzly_af_Plot"), height = "900px") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')
        })

        output$road_density_grizzly_Plot <- renderPlotly ({
          withProgress(message = 'Making Plots', value = 0.1, {
            data <- reportList()$grizzly_survival

            # if want to look at difference:
            # grizzly_data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, gbpu_name)]  # replace first() with shift() to get difference with previous year value instead of first year value

            p <- ggplot(data, aes (x = timeperiod, y = road_density, color = scenario)) +
              facet_grid (rows = vars(gbpu_name)) +
              geom_line() +
              xlab ("Future year") +
              ylab ("Grizzly Bear Population Unit Road Density (km/km2)") +
              theme_bw() +
              theme (legend.title = element_blank()) +
              scale_x_continuous(limits = c (input$grizzlyYear[1], input$grizzlyYear[2]))

            ggplotly(p, height = 900) %>%
              plotly::layout (
                legend = list (orientation = "h", y = -0.1),
                margin = list (
                  l = 50,
                  r = 40,
                  b = 50,
                  t = 40,
                  pad = 0
                )
              )
          })
        })

        output$survival_grizzly_af_Plot <- renderPlotly ({
          withProgress(message = 'Making Plots', value = 0.1, {
            data <- reportList()$grizzly_survival

            # if want to look at difference:
            # grizzly_data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, gbpu_name)]  # replace first() with shift() to get difference with previous year value instead of first year value

            p <-
              ggplot(data,
                     aes (x = timeperiod, y = survival_rate, color = scenario)) +
              facet_grid (rows = vars(gbpu_name)) +
              geom_line() +
              xlab ("Future year") +
              ylab ("Adult Female Survival Rate") +
              theme_bw() +
              theme (legend.title = element_blank()) +
              scale_x_continuous(limits = c (input$grizzlyYear[1], input$grizzlyYear[2]))

            ggplotly(p, height = 900) %>%
               plotly::layout (
                legend = list (orientation = "h", y = -0.1),
                margin = list (
                  l = 50,
                  r = 40,
                  b = 50,
                  t = 40,
                  pad = 0
                )
              )
          })
        })
      } else {
        output$survival_grizzly_af_Plot_UI <- renderUI({
          mod_html_alert_ui('propDisturbPlot')
        })

        output$road_density_grizzly_Plot_UI <- renderUI({
          mod_html_alert_ui('propDisturbBuffPlot')
        })
      }
    } else {
      output$survival_grizzly_af_Plot_UI <- renderUI({
        mod_html_alert_ui('propDisturbPlot')
      })

      output$road_density_grizzly_Plot_UI <- renderUI({
        mod_html_alert_ui('propDisturbBuffPlot')
      })
    }

  })
}
