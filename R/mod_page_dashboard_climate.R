#' page_dashboard_climate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_climate_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

      tabPanel(
        "Conceptual Path Diagram",

        h4("Conceptual Path Diagram"),
        plotOutput(outputId = ns("climatemap"), height = "800px")
      # ),
      # tabPanel(
      #   "Modelled Path Diagram",
      #
      #   h4("Modelled Path Diagram"),
      #   plotOutput(outputId = ns("climatemap"), height = "800px")
      )
    )
    # box(
    #   title = "Conceptual Path Diagram",
    #   width = 12,
    #   # background = "yellow",
    #   solidHeader = TRUE,
    #   collapsible = TRUE,
    #   plotOutput(outputId = ns("climatemap"), height = "800px")
    # ),
    # box(
    #   title = "Modelled Path Diagram",
    #   width = 12,
    #   # background = "yellow",
    #   solidHeader = TRUE,
    #   collapsible = TRUE,
    #   collapsed = TRUE
    # )
  )
}

#' page_dashboard_climate Server Functions
#'
#' @noRd
mod_page_dashboard_climate_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$climatemap <- renderPlot({
      plot.igraph(climateMap[[1]], layout = climateMap[[2]])
      legend(
        'topleft',
        legend = c(
          "Climate",
          "Anthropogenic",
          "Landscape Condition",
          "Predator-prey",
          "Energetics",
          "Health",
          "Population"
        ),
        col = c(
          "yellow",
          "orange",
          "purple",
          "lightblue",
          "pink",
          "red",
          "green"
        ),
        pch = 19,
        bty = 'n',
        cex = 1.7
      )
    })

  })
}
