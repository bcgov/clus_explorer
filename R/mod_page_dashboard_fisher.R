#' page_dashboard_fisher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_fisher_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Occupancy",
        collapsible = FALSE,
        collapsed = FALSE,
        solidHeader = TRUE,
        # background = "purple",
        width = 6,
        plotlyOutput(outputId = ns("fisherOccupancyPlot"), height = "300px")
      ),
      box(
        title = "Territory",
        collapsible = FALSE,
        collapsed = FALSE,
        solidHeader = TRUE,
        # background = "purple",
        width = 6,
        tags$style(
          " .irs-bar, .irs-bar-edge, .irs-single, .irs {max-height: 50px;}, .irs-grid-pol { background:blue; border-color: blue;}"
        ),
        sliderInput(
          ns("fisherTerritoryYear"),
          "Year",
          0,
          200,
          value = 0,
          step = 5,
          animate = TRUE
        ),
        plotOutput(outputId = ns("fisherTerritoryPlot"), height = "200px")
      )
    ),
    fluidRow(
      leafletOutput(ns("fishermapper"), height = 500, width = "100%"),
      absolutePanel(
        id = "fisher_map_control",
        class = "panel panel-default",
        top = 570,
        left = 245,
        fixed = FALSE,
        width = "15%",
        height = "30%",
        selectInput(
          ns("fisher_scenario_selected"),
          choices = NULL,
          label = 'Scenario',
          width = '100%',
          multiple = F
        ),
        bsTooltip(
          "fisher_scenario_selected",
          "Select a scenario to map.",
          "right"
        ),
        tags$style(
          " .irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol { background:black; border-color: black;}"
        ),
        sliderInput(
          ns("fisheryear"),
          "Year",
          0,
          200,
          value = 0,
          step = 5,
          animate = TRUE
        ),
        valueBoxOutput(ns("numberFisherTerritory"), width = 12),
        # @TODO: Replace the tooltip
        bsTooltip(
          "numberFisherTerritory",
          "Number of fisher territories with relative probability of occupancy > 0.55",
          "bottom"
        )
      )
    )
  )
}

#' page_dashboard_fisher Server Functions
#'
#' @noRd
mod_page_dashboard_fisher_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_fisher_ui("page_dashboard_fisher_ui_1")

## To be copied in the server
# mod_page_dashboard_fisher_server("page_dashboard_fisher_ui_1")
