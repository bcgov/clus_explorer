#' page_dashboard_fire UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_fire_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Summary of area burned",
        DT::dataTableOutput(ns("fireTable"), width = '100%')
      ),
      tabPanel(
        "Fire history 1919 - 2018",
        plotlyOutput(outputId = ns("fireByYearPlot"), height = "900px")
      ),
      tabPanel(
        "40 year cummulative area burned",
        plotlyOutput(outputId = ns("firecummulativePlot"), height = "900px")
      )
    )
  )
}

#' page_dashboard_fire Server Functions
#'
#' @noRd
mod_page_dashboard_fire_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (nrow(reportList()$fire) > 0) {
      output$fireByYearPlot <- renderPlotly ({
        withProgress(message = 'Making Plot', value = 0.1, {
          chart_bar_faceted(
            data = reportList()$fire,
            x_var = year,
            y_var = proportion.burn,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            facet_ncol = 1,
            xlab = "Year",
            ylab = "Proportion of area burned",
            scale_x_continuous_limits = c(1919, 2025),
            scale_x_continuous_breaks = seq(1925, 2025, by = 75),
            is_plotly = TRUE
          )
        })
      })
    }

    if (nrow(reportList()$fire) > 0) {
      output$firecummulativePlot <- renderPlotly ({
        withProgress(message = 'Making Plot', value = 0.1, {
          data <- reportList()$fire


          ##Calculating cummulative area burned over a 40 year moving window for each herd across each habitat type
          Years <- 1919:2018
          window_size <- 40

          Fire_cummulative <- data.frame (matrix (ncol = 3, nrow = 0))
          colnames (Fire_cummulative) <-
            c ("herd_bounds", "cummulative.area.burned", "year")

          for (i in 1:(length(Years) - window_size)) {
            fire.summary <-
              data %>% filter(year >= Years[i] & year <= (Years[i] + window_size)) %>%
              group_by (herd_bounds) %>%
              summarize(cummulative.area.burned = sum(proportion.burn))
            fire.summary$year <- Years[i] + window_size

            Fire_cummulative <-
              rbind(Fire_cummulative, as.data.frame(fire.summary))
          }

          chart_bar_faceted(
            data = Fire_cummulative,
            x_var = year,
            y_var = cummulative.area.burned,
            facet_chart = TRUE,
            facet_vars = herd_bounds,
            facet_ncol = 1,
            xlab = "Year",
            ylab = "Cummulative proportion of area burned < 40 years",
            scale_x_continuous_limits = c(1959, 2020),
            scale_x_continuous_breaks = seq(1960, 2020, by = 30),
            is_plotly = TRUE
          )
        })
      })
    }

    output$fireTable <- DT::renderDataTable(
      {
        dat <- reportList()$fire2
      },
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      filter = 'none',
      rownames = FALSE,
      selection = 'none'
    )

  })
}
