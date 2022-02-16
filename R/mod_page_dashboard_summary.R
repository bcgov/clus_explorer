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
        selectizeInput(ns("discount_rate"), label = "Future importance", choices = list(
          "Neutral" = 0,
          "Low" = -0.01,
          "Medium" = 0.01,
          "High" = 0.05
        )),
                         # c(0, 0.05,0.01,-0.01)),
        # actionButton(ns("baseline_scenario_apply"), label = "Apply"),
        # mod_chart_bar_ui(ns("baseline_values"), ),
        # mod_chart_line_ui(ns("baseline_compare"))
        div("Baseline chart here...")
      ),
      column(
        width = 6,
        plotlyOutput(outputId = ns("radar"), height = "750px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),
        mod_chart_heatmap_ui(ns("heatmap"), chart_height = "400px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      )
    )
  )
}

#' page_dashboard_summary Server Functions
#'
#' @noRd
#' @importFrom dplyr filter mutate
#' @importFrom forcats as_factor fct_reorder
#' @importFrom tidyr pivot_longer
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
      choices = c(NULL, schema_scenarios$scenario_names())
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
        #   list_to_li(schema_scenarios$scenario_names())
        # )
      )
    )

    # .. radar list ----
    radarList <- reactive({

      # req(input$baseline_scenario_apply)
      req(reportList()$indicators)
      req(input$discount_rate)
      req((baseline_values))

#       if(FALSE){
#
#
#       req(reportList()$harvest)
#       req(reportList()$survival)
#       req(schema_scenarios$scenario_names())
#       # browser()
#       DT.h <-
#         reportList()$harvest[, sum(volume) / sum(target), by = c("scenario", "timeperiod")][V1 > 0.9, .N, by = c("scenario")][, N :=
#                                                                                                                                 N / 100]
#       setnames(DT.h, "N", "Timber")
#       DT.s <-
#         dcast(reportList()$survival[survival_rate > 0.75 &
#                                       timeperiod > 0, .N / 100, by = c("scenario", "herd_bounds")], scenario ~
#                 herd_bounds, value.var =  "V1")
#       DT.all <- base::merge(DT.h, DT.s, by = "scenario")
#       DT.g <-
#         data.table(getTableQuery(
#           paste0(
#             "select foo1.scenario, gs_100/gs_0 as GrowingStock  from (
# 	(select sum(m_gs) as gs_0, scenario from ",
# 	schema_name,
# 	".growingstock where timeperiod in (0)  and scenario IN ('",
# 	paste(schema_scenarios$scenario_names(), sep =  "' '", collapse = "', '"),
# 	"')
# group by scenario, timeperiod) foo1
# JOIN (select sum(m_gs) as gs_100, scenario from ",
# schema_name,
# ".growingstock where timeperiod in (100) and scenario IN ('",
# paste(schema_scenarios$scenario_names(), sep =  "' '", collapse = "', '"),
# "')
# group by scenario, timeperiod) foo2
# ON (foo1.scenario = foo2.scenario) )"
#           )
#         ))
#       }

      # Copy the reactive object, otherwise the new object is the reference to
      # the reactive object
      dt.baseline <- data.table::copy(baseline_values())
      dt.indicators <- data.table::copy(
        reportList()$indicators %>% dplyr::filter(!(`scenario` == input$baseline_scenario))
      )
      setnames(dt.baseline, "variable", "base_variable")
      dt.compare.indicators <- as.data.table(
        base::merge(
          dt.indicators,
          dt.baseline,
          by.x =c("compartment", "timeperiod", "ind_name"),
          by.y =c("compartment", "timeperiod", "ind_name")
        )
      )

      out <- dt.compare.indicators[
        ,
        list(
          scen = sum((variable) / (1 + as.numeric(input$discount_rate)) ** timeperiod),
          base = sum((base_variable) / (1 + as.numeric(input$discount_rate)) ** timeperiod)
        ),
        by = c("scenario.x", "ind_name")
      ][, ind := (scen - base) / base]
      #base::merge(DT.all, DT.g, by = "scenario")

      out %>% tidyr::pivot_wider(
        id_cols = 1,
        names_from = 'ind_name',
        values_from = 'ind'
      )
    })
    rl <- radarList
    rl_long <- reactive({

      radarList() %>%
        tidyr::pivot_longer(
          cols = -1,
          names_to = 'Herd',
          values_to = 'Ratio'
        )
    })

    baseline_values <- reactive({
      # req(input$baseline_scenario_apply)
      req(reportList()$indicators)
      reportList()$indicators %>% dplyr::filter(`scenario` == input$baseline_scenario)
    })

    radar_plot <- reactive({
      renderPlotly ({

        rl <- as.data.table(radarList())
        radarLength <<- 1:nrow(rl)
        radarNames <-
          paste(c(names(rl)[2:length(rl)], names(rl)[2]), collapse = "', '")
        radarData <-
          rl[, data := paste(.SD, collapse = ',')  , .SDcols = c(names(rl)[2:length(rl)], names(rl)[2]), by = scenario.x]

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
      rl$scenario.x[x[]],
      "'
    ) %>%"
    )
  }), collapse = ''),
  "
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(-2, 2)
      )
    ),
    legend = list (orientation = 'h'),
    margin = list(t = 75, r = 75, b = 75, l = 75)
  ) "
          )
        ))
      })
    })

    output$radar <- radar_plot()
    # output$baseline_values <- baseline_values()

    rll <- rl_long()
    mod_chart_heatmap_server('heatmap', rl_long(), col_x = Herd, col_y = scenario.x, col_z = Ratio)

    # Return reactive values ----
    # to be used in other modules
    return(
      list(
        radar_list = radarList,
        radar_list_long = rl_long,
        baseline_values = baseline_values,
        baseline_scenario = input$baseline_scenario,
        risk = input$discount_rate
      )
    )

  })
}