#' page_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Welcome to the CLUS Explorer App",
      width = 12,
      fluidRow(
        column(
          width = 10,
          p(
            "This app was designed to interactively compare outputs from the caribou
            and landuse simulator (CLUS) model. Outputs are formely organized by
            scenario; represnting a plausible future projection of the landscape."
          )
        ),
        column(
          width = 2,
          align = "center",
          img(src = "www/img/clus-logo.png", width = 100)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton("help", "Take a tour") %>%
          bsplus::bs_embed_tooltip(
            "Press for instructions",
            "right"
          ),
        align = "center",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      )
    )
  )
}

#' page_home Server Functions
#'
#' @noRd
mod_page_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # .. steps ----
    steps <- reactive(
      data.frame(
        element = c(
          ".sidebar-menu",
          ".settings",
          ".treeview",
          ".querybuilder",
          ".mapviewer"
        ),
        intro = c(
          "This is the navigation sidebar where you can select various features in the app.",
          "Step 1: This is where you select your area of interest and the various scenarios you wish to compare.",
          "Step 2: This is where you can view outputs of various indicators by scenario",
          "Advanced: This is the query builder where you can create output tables",
          "Advanced: This is where you can interatively view spatial outputs"
        ),
        position = c("right", "right", "right", "right", "right")
      )
    )

    observeEvent(
      input$help,
      introjs(
        session,
        options = list(
          steps = steps(),
          "nextLabel" = "Next",
          "prevLabel" = "Previous",
          "skipLabel" = "Skip"
        )
      )
    )

  })
}
