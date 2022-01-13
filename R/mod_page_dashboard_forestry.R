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
    fluidRow(
      box(
        title = "Harvest Flow",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "green",
        width = 12,
        plotlyOutput(outputId = ns("harvestAreaPlot"), height = "400px"),
        plotlyOutput(outputId = ns("harvestVolumePlot"), height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Transition Harvest",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "green",
        width = 12,
        plotlyOutput(outputId = ns("managedAreaPlot"), height = "400px"),
        plotlyOutput(outputId = ns("managedVolumePlot"), height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Harvest Age",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "green",
        width = 12,
        plotlyOutput(outputId = ns("harvestAgePlot"), height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Available THLB",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "green",
        width = 12,
        plotlyOutput(outputId = ns("availableTHLBPlot"), height = "400px")
      )
    ),
    fluidRow(
      box(
        title = "Growingstock",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "green",
        width = 12,
        plotlyOutput(outputId = ns("growingStockPlot"), height = "400px")
      )
    )
  )
}

#' page_dashboard_forestry Server Functions
#'
#' @noRd
mod_page_dashboard_forestry_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_forestry_ui("page_dashboard_forestry_ui_1")

## To be copied in the server
# mod_page_dashboard_forestry_server("page_dashboard_forestry_ui_1")
