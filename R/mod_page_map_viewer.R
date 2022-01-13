#' page_map_viewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_map_viewer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #Raster query
      box(
        title = "Map Query",
        collapsible = T,
        background = "black",
        solidHeader = TRUE,
        width = 10,
        selectInput(
          ns("maplayers"),
          label = "Available Layers",
          multiple = FALSE,
          choices = NULL
        ),
        actionButton(ns("getMapLayersButton"), "Load")
      )
    ),
    leafletOutput(ns("resultSetRaster"), height = 750, width = "83%")

  )
}

#' page_map_viewer Server Functions
#'
#' @noRd
mod_page_map_viewer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_map_viewer_ui("page_map_viewer_ui_1")

## To be copied in the server
# mod_page_map_viewer_server("page_map_viewer_ui_1")
