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
    fluidRow(
      box(
        title = "Summary of area burned",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "red",
        width = 12   ,
        dataTableOutput(ns("fireTable"))
      )
    ),
    fluidRow(
      box(
        title = "Fire history 1919 - 2018",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "red",
        width = 12,
        plotlyOutput(outputId = ns("fireByYearPlot"), height = "900px")
      )
    ),
    fluidRow(
      box(
        title = "40 year cummulative area burned",
        collapsible = TRUE,
        collapsed = TRUE,
        solidHeader = TRUE,
        # background = "red",
        width = 12,
        plotlyOutput(outputId = ns("firecummulativePlot"), height = "900px")
      )
    )
  )
}

#' page_dashboard_fire Server Functions
#'
#' @noRd
mod_page_dashboard_fire_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_dashboard_fire_ui("page_dashboard_fire_ui_1")

## To be copied in the server
# mod_page_dashboard_fire_server("page_dashboard_fire_ui_1")
