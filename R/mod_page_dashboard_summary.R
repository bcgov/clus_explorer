#' page_dashboard_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("heading")),
    fluidRow(
      column(
        width = 6,
        selectizeInput(ns("baseline_scenario"), label = "Baseline scenario", choices = NULL),
        div("Baseline chart here...")
      ),
      column(
        width = 6,
        plotlyOutput(outputId = ns("radar"), height = "750px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      )
    )
  )
}

#' page_dashboard_summary Server Functions
#'
#' @noRd
#' @importFrom stringr str_replace str_replace_all
mod_page_dashboard_summary_server <- function(id, schema_scenarios, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    schema_name <- schema_scenarios$schema()
    schema_label <- stringr::str_to_sentence(
      stringr::str_replace_all(schema_scenarios$schema(), pattern = '_', ' ')
    )

    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "baseline_scenario",
      choices = c(NULL, schema_scenarios$scenario())
    )

    output$heading <- renderUI(
      tagList(
        shiny::tags$h3(paste('Summary for', schema_label)),
        shiny::tags$h4('TSA selected:'),
        shiny::tags$ul(
          list_to_li(schema_scenarios$tsa_selected())
        )#,
        # shiny::tags$h4('Scenarios:'),
        # shiny::tags$ul(
        #   list_to_li(schema_scenarios$scenario())
        # )
      )
    )

    # .. radar list ----
    radarList <- reactive({
      req(reportList()$harvest)
      req(reportList()$survival)
      req(schema_scenarios$scenario())
      # browser()
      DT.h <-
        reportList()$harvest[, sum(volume) / sum(target), by = c("scenario", "timeperiod")][V1 > 0.9, .N, by = c("scenario")][, N :=
                                                                                                                                N / 100]
      setnames(DT.h, "N", "Timber")
      DT.s <-
        dcast(reportList()$survival[survival_rate > 0.75 &
                                      timeperiod > 0, .N / 100, by = c("scenario", "herd_bounds")], scenario ~
                herd_bounds, value.var =  "V1")
      DT.all <- base::merge(DT.h, DT.s, by = "scenario")
      DT.g <-
        data.table(getTableQuery(
          paste0(
            "select foo1.scenario, gs_100/gs_0 as GrowingStock  from (
	(select sum(m_gs) as gs_0, scenario from ",
	schema_name,
	".growingstock where timeperiod in (0)  and scenario IN ('",
	paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
	"')
group by scenario, timeperiod) foo1
JOIN (select sum(m_gs) as gs_100, scenario from ",
schema_name,
".growingstock where timeperiod in (100) and scenario IN ('",
paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
"')
group by scenario, timeperiod) foo2
ON (foo1.scenario = foo2.scenario) )"
          )
        ))
      base::merge(DT.all, DT.g, by = "scenario")
    })

    radar_plot <- reactive({
      renderPlotly ({
        # browser()
        a <- 2
        rl <- radarList()
        b <- 3
        radarLength <<- 1:nrow(rl)
        radarNames <-
          paste(c(names(rl)[2:length(rl)], names(rl)[2]), collapse = "', '")
        radarData <-
          rl[, data := paste(.SD, collapse = ',')  , .SDcols = c(names(rl)[2:length(rl)], names(rl)[2]), by = scenario]

        eval(parse(
          text = paste0(
            "plot_ly(
  type = 'scatterpolar',
  mode = 'lines+markers',
  fill = 'toself'
  ) %>%",
  paste(sapply(radarLength, function(x) {
    paste0(
      "add_trace(
      r = c(",
      radarData$data[x[]],
      "),
      theta = c('",
      radarNames,
      "'),
      name = '",
      rl$scenario[x[]],
      "'
    ) %>%"
    )
  }), collapse = ''),
  "
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,1.2)
      )
    ),
    legend = list (orientation = 'h'),
    margin = list(t = 50, r = 50, b = 50, l = 50)
  ) "
          )
        ))
      })
    })

    output$radar <- radar_plot()

  })
}
