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
        sliderInput(
          ns("grizzlyYear"),
          label = "Enter Year Range to Plot",
          0,
          200,
          value = c (0, 50),
          step = 5
        ),
        plotlyOutput(outputId = ns("survival_grizzly_af_Plot"), height = "900px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      ),
      tabPanel(
        "Grizzly Bear Population Unit Road Density",
        h4("Grizzly Bear Population Unit Road Density"),
        plotlyOutput(outputId = ns("road_density_grizzly_Plot"), height = "900px") %>%
          withSpinner(color = '#ecf0f5', color.background = '#ffffff')
      )
    )
    # tags$div(
    #   "Click on the boxes below to obtain information on estimated road density and adult female  survival. Esitmates are provided for grizzly bear population units (GBPUs). Survival estimates are calculated by adapting a model developed by ",
    #   tags$a(href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0115535", "Boulanger and Stenhouse (2014).")
    # ),
    # br(),
    # line break
    # fluidRow(
    #   box(
    #     title = "Adult Female Survival",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     sliderInput(
    #       ns("grizzlyYear"),
    #       label = "Enter Year Range to Plot",
    #       0,
    #       200,
    #       value = c (0, 50),
    #       step = 5
    #     ),
    #     plotlyOutput(outputId = ns("survival_grizzly_af_Plot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Grizzly Bear Population Unit Road Density",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("road_density_grizzly_Plot"), height = "900px")
    #   )
    # )
  )
}

#' page_dashboard_grizzly Server Functions
#'
#' @noRd
mod_page_dashboard_grizzly_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
          layout (
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

  })
}
