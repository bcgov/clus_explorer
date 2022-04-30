#' page_map_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Generate report"),
    div(
      id = "report-options",
      p(
        "Press the button below to generate a PDF report for the selected area of interest and scenarios."
      ),

      shiny::selectizeInput(
        inputId = ns('details'),
        label = 'Include sections',
        choices = c(
          'Summary' = 'summary',
          'Caribou' = 'caribou',
          'Fire' = 'fire',
          'Fisher' = 'fisher',
          'Forestry' = 'forestry',
          'Grizzly' = 'grizzly'
        ),
        selected = NULL,
        multiple = TRUE,
        width = '50%'
      ),

      shiny::radioButtons(
        ns('format'),
        label = "Select file format",
        inline = FALSE,
        choiceNames = list(
          tagList(icon("file-pdf"), span(' PDF')),
          tagList(icon("file-word"), span(' Word')),
          tagList(icon("file-powerpoint"), span(' PowerPoint'))
        ),
        choiceValues = list(
          "pdf", "word", "powerpoint"
        )
      ),
      shiny::downloadButton(
        ns("generate_report"),
        "Download report",
        icon = shiny::icon("download"),
        class = "btn-clus disabled"
      ),
      icon('info-circle') %>%
        bsplus::bs_embed_tooltip(
          'To enable this function, you have to select Area of Interest and
          scenarios on the "Scenarios" page, as well as select a baseline scenario
          and click Apply button on the "Dashboard -> Summary" page',
          "top",
          delay = "5s"
        )

    )
  )
}

#' page_map_report Server Functions
#'
#' @noRd
#' @import rmarkdown
#' @import promises
#' @import future
mod_page_report_server <- function(
  id, schema = NULL, tsas = NULL, scenario_names = NULL, scenarios = NULL, data_seral_treemap = NULL,
  reportList = NULL, status_thlb = NULL, status_avg_vol = NULL, status_road = NULL,
  radar_list = NULL, radar_list_long = NULL, baseline_values = NULL, baseline_scenario = NULL,
  risk = NULL
) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    req(reportList)
    req(scenario_names)
    req(radar_list)

    # Disable Apply button if no scenarios are selected
    observe({
      shinyjs::toggleState(
        "generate_report",
        condition = (length(scenario_names()) > 0)
      )
    })

    # Download handler - report ----
    render_report_async <- function(file, filepath, output_format, output_dir, params){

      future({
        id <- shiny::showNotification(
          "Generating report...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(shiny::removeNotification(id), add = TRUE)

        out <- rmarkdown::render(
          input = 'inst/app/report.Rmd',
          output_format = output_format,
          output_file = filepath,
          output_dir = output_dir,
          params = params,
          run_pandoc = TRUE,
          envir = new.env(parent = globalenv()),
          clean = TRUE,
          quiet = TRUE
        )
        file.rename(out, file)
      })
    }

    # Get rid of reactive values
    schema <- data.table::copy(schema())
    tsas <- data.table::copy(tsas())
    scenario_names <- data.table::copy(scenario_names())
    scenarios <- data.table::copy(scenarios())
    data_seral_treemap <- data.table::copy(data_seral_treemap())
    status_thlb <- data.table::copy(status_thlb())
    status_avg_vol <- data.table::copy(status_avg_vol())
    status_road <- data.table::copy(status_road())
    reportList <- data.table::copy(reportList())

    output$generate_report <- downloadHandler(
      filename = function() {
        paste(
          'CLUS report',
          switch(input$format, pdf = 'pdf', word = 'docx', powerpoint = 'pptx'),
          sep = '.'
        )
      },
      content = function(file) {

        # Set up parameters to pass to Rmd document
        params <- list(
          details = input$details,
          schema = schema,
          tsas = tsas,
          scenario_names = scenario_names,
          scenarios = scenarios,
          data_seral_treemap = data_seral_treemap,
          reportList = reportList,
          status_thlb = status_thlb,
          status_avg_vol = status_avg_vol,
          status_road = status_road,
          radar_list = data.table::copy(radar_list()),
          radar_list_long = data.table::copy(radar_list_long()),
          baseline_values = data.table::copy(baseline_values()),
          baseline_scenario = data.table::copy(baseline_scenario),
          risk = data.table::copy(risk)
        )

        loc <- dirname(file)
        file_extension <- switch(
          input$format,
          pdf = '.pdf', word = '.docx', powerpoint = '.pptx'
        )
        filepath <- paste0(as.character(as.numeric(Sys.time()) * 100000), file_extension)

        output_format <- switch(
          input$format,
          pdf = pdf_document(),
          word = word_document(),
          powerpoint = powerpoint_presentation()
        )

        out <- render_report_async(file, filepath, output_format, loc, params)
      }
    )

  })
}
