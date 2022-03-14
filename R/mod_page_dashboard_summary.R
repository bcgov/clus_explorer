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
      div(
        class = 'col-lg-5 col-md-5 col-sm-12 col-xs-12',

        h3('Scenario analysis'),
      )
    ),
    fluidRow(
      div(
        class = 'col-lg-3 col-md-3 col-sm-12 col-xs-12',
        p(
          'Select baseline scenario and compare other selected scenarios to the baseline values.
          Use the controls below to change baseline scenario and the importance of caribou and land use in future years.'
        ),
        wellPanel(
          selectizeInput(ns("baseline_scenario"), label = "Baseline scenario", choices = NULL),
          p(
            strong("Future importance"),
            icon('info-circle') %>%
              bsplus::bs_embed_tooltip(
                "Select how important future years are for your analysis",
                "right"
              )
          ),
          selectizeInput(
            ns("discount_rate"),
            label = NULL,
            choices = list(
              "Neutral" = 0,
              "Low" = -0.01,
              "Medium" = 0.01,
              "High" = 0.05
            )
          ),
          actionButton(
            ns("baseline_scenario_apply"),
            label = "Apply",
            class = "btn-clus btn-apply-scenarios",
            icon = icon("check-circle")
          )
        )
      ),
      div(
        class = 'col-lg-8 col-lg-offset-1 col-md-8 col-md-offset-1 col-sm-12 col-xs-12',
        h4('Baseline scenario values'),
        p(
          'Charts and table below show the values of baseline scenario.
          The facet variables are Growing Stock (m_gs), Volume Harvested (vol_h),
          and individual herd names.'
        ),
        div(
          class = 'chart-container',
          plotlyOutput(outputId = ns("baseline_scenario_charts"), height = "450px") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')
        )
      )
    ),
    uiOutput(ns('baseline_comparison'))
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

    # # .. introjs steps ----
    # steps <- reactive(
    #   data.frame(
    #     element = c(
    #       ".sidebar-menu",
    #       ".settings",
    #       ".treeview",
    #       ".report"
    #     ),
    #     intro = c(
    #       "This is the navigation sidebar where you can select various features in the app.",
    #       "Step 1: This is where you select your area of interest and the various scenarios you wish to compare.",
    #       "Step 2: This is where you can view outputs of various indicators by scenario.
    #
    #       You must have selected an area of interest and at least two scnearios in Step 1.",
    #       "Step 3: Use this tab to generate and download summary or detailed report in PDF/Word/PowerPoint format.<br>You must have selected an area of interest and at least two scnearios in Step 1."
    #     ),
    #     position = c("right", "right", "right", "right")
    #   )
    # )
    #
    # observeEvent(
    #   input$help,
    #   introjs(
    #     session,
    #     options = list(
    #       steps = steps(),
    #       "nextLabel" = "Next",
    #       "prevLabel" = "Previous",
    #       "skipLabel" = "Skip"
    #     )
    #   )
    # )

    radarList = reactive(NULL)
    rl_long = reactive(NULL)

    schema_name <- schema_scenarios$schema()
    schema_label <- stringr::str_to_sentence(
      stringr::str_replace_all(schema_scenarios$schema(), pattern = '_', ' ')
    )

    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "baseline_scenario",
      choices = schema_scenarios$scenario_names()#,
      # selected = ''
    )

    observeEvent(
      schema_scenarios$apply_scenario(),
      ignoreInit = TRUE,
      {
        # browser()
        isolate(schema_scenarios$scenarios())
        isolate(schema_scenarios$scenario_names())
        output$heading <- renderUI(
          tagList(
            fluidRow(
              div(
                class = 'col-lg-10 col-md-10 col-sm-12',
                shiny::tags$h3(paste('Summary for', schema_label)),
                shiny::tags$h4('Selected scenarios:'),
                # Generate table rows
                {
                  scenarios <- as.data.frame(schema_scenarios$scenarios())
                  scenarios <- scenarios%>%
                    dplyr::filter(scenario %in% schema_scenarios$scenario_names())

                  trows <- unlist(
                    lapply(1:nrow(scenarios), function(i) {
                      paste0('
                        <tr class="">
                          <td>', scenarios[i, 1], '</td>
                          <td>', round(scenarios[i, 3], 2), '</td>
                          <td class="description">', scenarios[i, 2], '</td>
                        </tr>'
                      )
                    })
                  )

                  # Table header
                  scenarios_table <- paste0('
                    <table id="scenario-list" class="clus-explorer-table">
                      <thead>
                        <tr>
                    		  <th>Scenario</th>
                    		  <th>Rank</th>
                    		  <th>Description</th>
                    	  </tr>
                      </thead>' ,
                      paste(trows, collapse=" "), '
                    </table>'
          	      )

                	div(
                	  id = "scenario-list-container",
                		HTML(scenarios_table)
                	)
                }
              )
            )
          )
        )

        if (length(schema_scenarios$scenario_names()) > 1) {
          output$baseline_comparison <- renderUI({
            tagList(
              fluidRow(

                div(
                  class = 'col-lg-6 col-md-12 col-sm-12 col-xs-12',
                  h4('Other scenarios compared to baseline'),
                  p(
                    'Charts below show how other selected scenarios compare
              to the baseline scenario. Dimensions in those multi-dimensional plots are
              individual scenarios, Growing Stock (m_gs), Volume Harvested (vol_h),
              and individual herd names.'
                  )
                )
              ),
              fluidRow(
                div(
                  class = 'col-lg-6 col-md-12 col-sm-12 col-xs-12',
                  div(
                    class = 'chart-container',
                    plotlyOutput(outputId = ns("radar"), height = "750px") %>%
                      withSpinner(color = '#ecf0f5', color.background = '#ffffff'),
                  )
                ),
                div(
                  class = 'col-lg-6 col-md-12 col-sm-12 col-xs-12',
                  div(
                    class = 'chart-container',
                    mod_chart_heatmap_ui(ns("heatmap"), chart_height = "400px") %>%
                      withSpinner(color = '#ecf0f5', color.background = '#ffffff')
                  )
                )
              )
            )
          })

          # .. radar list ----
          radarList <- reactive({

            input$baseline_scenario_apply

            # browser()
            # req(schema_scenarios$apply_scenario())
            # req(reportList()$indicators)
            # req(isolate(input$discount_rate))
            # req((baseline_values))

            baseline_scenario <- isolate(input$baseline_scenario)
            discount_rate <- isolate(input$discount_rate)

            # Copy the reactive object, otherwise the new object is the reference to
            # the reactive object
            dt.baseline <- data.table::copy(baseline_values())
            dt.indicators <- data.table::copy(
              reportList()$indicators %>% dplyr::filter(!(`scenario` == baseline_scenario))
            )
            setnames(dt.baseline, "variable", "base_variable")
            dt.compare.indicators <- as.data.table(
              base::merge(
                dt.indicators,
                dt.baseline,
                by.x = c("compartment", "timeperiod", "ind_name"),
                by.y = c("compartment", "timeperiod", "ind_name")
              )
            )

            out <- dt.compare.indicators[
              ,
              list(
                scen = sum((variable) / (1 + as.numeric(discount_rate)) ** timeperiod),
                base = sum((base_variable) / (1 + as.numeric(discount_rate)) ** timeperiod)
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
          if (nrow(radarList()) > 0) {
            rl_long <- reactive({
              radarList() %>%
                tidyr::pivot_longer(
                  cols = -1,
                  names_to = 'Herd',
                  values_to = 'Ratio'
                )
            })
          }

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
            range = c(-1, 1)
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

          if(!is.null(rl_long())) {
            if(nrow(rl_long()) > 0) {
              mod_chart_heatmap_server('heatmap', rl_long(), col_x = Herd, col_y = scenario.x, col_z = Ratio)
            }
          }
        }
      }
    )

    baseline_values <- reactive({

      input$baseline_scenario_apply

      # req(input$baseline_scenario_apply)
      baseline_scenario <- isolate(input$baseline_scenario)

      # req(reportList()$indicators)
      # browser()
      bv <- isolate(
        reportList()$indicators %>%
          dplyr::filter(`scenario` == baseline_scenario) %>%
          arrange(compartment, ind_name, timeperiod)
      )
      bv
    })

    output$baseline_scenario_charts <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {

        isolate(baseline_values())
        # labeller_data <- c()
        baseline_values_annotated <- baseline_values() %>%
          mutate(
            ind_name = ifelse(
              ind_name == 'm_gs',
              'Growing Stock (m3)',
              ifelse(
                ind_name == 'vol_h',
                'Harvested volume (m3)',
                paste(ind_name, '(ha disturbed)')
              )
            )
          )

        chart_line_faceted(
          data = baseline_values_annotated,
          x_var = timeperiod,
          y_var = variable,
          color_var = scenario,
          facet_chart = TRUE,
          facet_vars = ind_name,
          facet_scales = 'free_y',
          facet_ncol = 2,
          xlab = "Future year",
          ylab = "Proportion Age 0 to 40 years",
          is_plotly = TRUE,
          # strip.position = "left",
          # labeller_data = labeller_data,
          height = 450
        )
      })
    })

    # Return reactive values ----
    # to be used in other modules
    return({

      input$baseline_scenario_apply

      baseline_scenario <- isolate(input$baseline_scenario)
      discount_rate <- isolate(input$discount_rate)

      list(
        radar_list = radarList,
        radar_list_long = rl_long,
        baseline_values = baseline_values,
        baseline_scenario = baseline_scenario,
        risk = discount_rate
      )
    })
  })
}
