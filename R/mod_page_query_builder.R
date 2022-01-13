#' page_query_builder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_query_builder_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #Table query
      box(
        title = "Table Query",
        background = "black",
        solidHeader = TRUE,
        #em("SELECT",style="color:#090909;font-size:80%")
        selectInput(
          inputId = ns("queryTable"),
          label = "FROM TABLE:",
          choices = c(
            "scenarios",
            "harvest",
            "rsf",
            "growingstock",
            "survival" ,
            "yielduncertainty"
          ),
          selected = character(0)
        ),
        fluidRow(column(
          width = 6, textInput(inputId = ns("queryWHERE"), label = "Where")
        ),
        column(
          width = 6,
          selectInput(
            inputId = ns("queryColumns"),
            label = "Columns",
            choices = NULL
          )
        )),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("queryRows"),
              label = "Rows",
              choices = NULL
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns("queryValue"),
              label = "Values",
              choices = c("SUM", "AVG", "COUNT"),
              selected = character(0)
            )
          )
        )
      ),
      dataTableOutput(ns("resultSetTable"))
    )

  )
}

#' page_query_builder Server Functions
#'
#' @noRd
mod_page_query_builder_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_query_builder_ui("page_query_builder_ui_1")

## To be copied in the server
# mod_page_query_builder_server("page_query_builder_ui_1")
