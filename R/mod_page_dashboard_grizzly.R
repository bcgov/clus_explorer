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
    tags$div(
      "Click on the boxes below to obtain information on estimated road density and adult female  survival. Esitmates are provided for grizzly bear population units (GBPUs). Survival estimates are calculated by adapting a model developed by ",
      tags$a(href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0115535", "Boulanger and Stenhouse (2014).")
    ),
    br(),
    # line break
    fluidRow(
      box(
        title = "Adult Female Survival",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "purple",
        width = 12,
        sliderInput(
          ns("grizzlyYear"),
          label = "Enter Year Range to Plot",
          0,
          200,
          value = c (0, 50),
          step = 5
        ),
        plotlyOutput(outputId = ns("survival_grizzly_af_Plot"), height = "900px")
      )
    ),
    fluidRow(
      box(
        title = "Grizzly Bear Population Unit Road Density",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "purple",
        width = 12,
        plotlyOutput(outputId = ns("road_density_grizzly_Plot"), height = "900px")
      )
    )
  )
}

#' page_dashboard_grizzly Server Functions
#'
#' @noRd
mod_page_dashboard_grizzly_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_grizzly_ui("page_dashboard_grizzly_ui_1")

## To be copied in the server
# mod_page_dashboard_grizzly_server("page_dashboard_grizzly_ui_1")
