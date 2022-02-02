#' page_dashboard_forestry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_forestry_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Harvest Flow",
        plotlyOutput(outputId = ns("harvestAreaPlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),
        plotlyOutput(outputId = ns("harvestVolumePlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Transition Harvest",
        plotlyOutput(outputId = ns("managedAreaPlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),
        plotlyOutput(outputId = ns("managedVolumePlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Harvest Age",
        plotlyOutput(outputId = ns("harvestAgePlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Available THLB",
        plotlyOutput(outputId = ns("availableTHLBPlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Growingstock",
        plotlyOutput(outputId = ns("growingStockPlot"), height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      )
    )
  )
}

#' page_dashboard_forestry Server Functions
#'
#' @noRd
mod_page_dashboard_forestry_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$harvestAreaPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {

        data <-
          reportList()$harvest[, sum(area), by = c("scenario", "timeperiod")]
        data$scenario <-
          reorder(data$scenario, data$V1, function(x)
            - max(x))
        data[, timeperiod := as.integer(timeperiod)]
        p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
          geom_area(position = "identity", aes(alpha = scenario)) +
          xlab ("Future year") +
          ylab ("Area Harvested (ha)") +
          scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
          scale_alpha_discrete(range = c(0.4, 0.8)) +
          scale_fill_grey(start = 0.8, end = 0.2) +
          theme_bw()
        ggplotly(p)
      })
    })

    output$harvestAgePlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data <- reportList()$harvest
        data <-
          data[, lapply(.SD, FUN = weighted.mean, x = age), by = c("timeperiod", "scenario"), .SDcols =
                 'age']
        #data$scenario <- reorder(data$scenario, data$V1, function(x) -max(x) )
        data[, timeperiod := as.integer(timeperiod)]
        p <- ggplot (data, aes (x = timeperiod, y = age, fill = scenario)) +
          geom_area(position = "identity", aes(alpha = scenario)) +
          xlab ("Future year") +
          ylab ("Average Harvest Age (yrs)") +
          scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
          scale_alpha_discrete(range = c(0.4, 0.8)) +
          scale_fill_grey(start = 0.8, end = 0.2) +
          theme_bw()
        ggplotly(p)
      })
    })

    output$harvestVolumePlot <- renderPlotly ({
      data <-
        reportList()$harvest[, sum(volume), by = c("scenario", "timeperiod")]
      data$scenario <-
        reorder(data$scenario, data$V1, function(x)
          - max(x))
      data[, timeperiod := as.integer(timeperiod)]
      p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
        geom_area(position = "identity", aes(alpha = scenario)) +
        xlab ("Future year") +
        ylab ("Volume Harvested (m3)") +
        scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
        scale_alpha_discrete(range = c(0.4, 0.8)) +
        scale_fill_grey(start = 0.8, end = 0.2) +
        theme_bw()
      ggplotly(p)
    })

    output$managedAreaPlot <- renderPlotly ({
      data <-
        reportList()$harvest[, sum(transition_area), by = c("scenario", "timeperiod")]
      data$scenario <-
        reorder(data$scenario, data$V1, function(x)
          - max(x))
      data[, timeperiod := as.integer(timeperiod)]
      p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
        geom_area(position = "identity", aes(alpha = scenario)) +
        xlab ("Future year") +
        ylab ("Managed Area Harvested (ha)") +
        scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
        scale_alpha_discrete(range = c(0.4, 0.8)) +
        scale_fill_grey(start = 0.8, end = 0.2) +
        theme_bw()
      ggplotly(p)
    })

    output$managedVolumePlot <- renderPlotly ({
      data <-
        reportList()$harvest[, sum(transition_volume), by = c("scenario", "timeperiod")]
      data$scenario <-
        reorder(data$scenario, data$V1, function(x)
          - max(x))
      data[, timeperiod := as.integer(timeperiod)]
      p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
        geom_area(position = "identity", aes(alpha = scenario)) +
        xlab ("Future year") +
        ylab ("Managed Volume Harvested (m3)") +
        scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
        scale_alpha_discrete(range = c(0.4, 0.8)) +
        scale_fill_grey(start = 0.8, end = 0.2) +
        theme_bw()
      ggplotly(p)
    })

    output$availableTHLBPlot <- renderPlotly ({
      data <-
        reportList()$harvest[, sum(avail_thlb), by = c("scenario", "timeperiod")]
      data$scenario <-
        reorder(data$scenario, data$V1, function(x)
          - max(x))
      data[, timeperiod := as.integer(timeperiod)]
      p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
        geom_area(position = "identity", aes(alpha = scenario)) +
        xlab ("Future year") +
        ylab ("Available THLB (ha)") +
        scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
        scale_alpha_discrete(range = c(0.4, 0.8)) +
        scale_fill_grey(start = 0.8, end = 0.2) +
        theme_bw()
      ggplotly(p)
    })

    output$growingStockPlot <- renderPlotly ({
      data <- reportList()$growingstock
      data$scenario <-
        reorder(data$scenario, data$growingstock, function(x)
          - max(x))
      p <-
        ggplot(data, aes (x = timeperiod, y = growingstock, fill = scenario)) +
        geom_area(position = "identity", aes(alpha = scenario)) +
        xlab ("Future year") +
        ylab ("Growing Stock (m3)") +
        scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
        scale_alpha_discrete(range = c(0.4, 0.8)) +
        scale_fill_grey(start = 0.8, end = 0.2) +
        theme_bw()
      ggplotly(p)
    })

  })
}
