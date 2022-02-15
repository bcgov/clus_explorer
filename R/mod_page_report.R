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
    # actionButton(inputId = ns("generate"), label = "Generate report")
    h2("Generate report"),
    p(
      "Press the button below to generate a PDF report for the selected area of interest and scenarios."
    ),
    shiny::downloadButton(ns("generate_report"), "Download report", icon = shiny::icon("download"))
  )
}

#' page_map_report Server Functions
#'
#' @noRd
#' @import rmarkdown
#' @import promises
#' @import future
mod_page_report_server <- function(
  id, schema = NULL, tsas = NULL, scenarios = NULL, data_seral_treemap = NULL,
  reportList = NULL, baseline_scenario = NULL, status_thlb = NULL, status_avg_vol = NULL,
  status_road = NULL, radar_list = NULL, radar_list_long = NULL
) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    req(reportList)

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

    # if (length(reportList()) == 0) {
    #   shinyjs::alert('No scenarios have been selected.')
    # } else {

    # Get rid of reactive values
    # browser()
    schema = data.table::copy(schema())
    tsas = data.table::copy(tsas())
    scenarios = data.table::copy(scenarios())
    data_seral_treemap = data.table::copy(data_seral_treemap())
    status_thlb = data.table::copy(status_thlb())
    status_avg_vol = data.table::copy(status_avg_vol())
    status_road = data.table::copy(status_road())
    reportList = data.table::copy(reportList())
    baseline_scenario = NA #baseline_scenario()
    radar_list = data.table::copy(radar_list())
    radar_list_long = data.table::copy(radar_list_long())

    output$generate_report <- downloadHandler(
      filename = 'clus_report.pdf',
      content = function(file) {

        # Set up parameters to pass to Rmd document
        params <- list(
          schema = schema,
          tsas = tsas,
          scenarios = scenarios,
          data_seral_treemap = data_seral_treemap,
          reportList = reportList,
          baseline_scenario = baseline_scenario,
          status_thlb = status_thlb,
          status_avg_vol = status_avg_vol,
          status_road = status_road,
          radar_list = radar_list,
          radar_list_long = radar_list_long
        )

        loc <- dirname(file)
        file_extension <- '.pdf'
        filepath <- paste0(as.character(as.numeric(Sys.time()) * 100000), file_extension)

        output_format <- pdf_document()
        out <- render_report_async(file, filepath, output_format, loc, params)
      }
    )
    # }

  })
}
