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
            strong("Future Importance Adjustment"),
            actionLink(ns('discount_rate_help'), label = '', icon = icon('info-circle'))
            # icon('info-circle') %>%
            #   bsplus::bs_embed_tooltip(
            #     "Select how important future years are for your analysis",
            #     "right"
            #   )
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
      uiOutput(ns('baseline_scenario_charts_container'))
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

    # Description of Future Importance Adjustment modal ----
    observeEvent(
      input$discount_rate_help,
      ignoreInit = TRUE,
      {
        showModal(
          modalDialog(
            title = "Future Importance Adjustment Description",
            tagList(
              p(
                "The radar plot and heat maps are based on the total indicator value
                across the planning horizon of the simulation (e.g., over a 200 years period).
                However, there may be interest in weighting the indicator scores
                according to whether they are more or less important to occur
                in the short or long-term."
              ),
              hr(),
              div(
                id = 'discount-rate-description-example',
                class = 'discount-rate-description-example',
                strong(em("Example:")),
                "The ‘future importance’ adjustment allows you to weight the importance
                of the indicators towards the short or long term.
                A ‘High’ future importance
                results in greater weight on the long term then the short term;
                A ‘Neutral’ results in all time periods being equally weighted;
                A ‘Medium’ future importance results in greater weight on the short term;
                A ‘Low’ future importance results in even greater weight on the short term.
                As an example, if you want to prioritize the value of the indicators
                in the long-term, select the “High” value."
              )
            ),
            size = 'm',
            easyClose = TRUE,
            footer = modalButton("Dismiss")
          )
        )
      }
    )

    # Description of Plots modal ----
    observeEvent(
      input$plot_descriptions_help,
      ignoreInit = TRUE,
      {
        showModal(
          modalDialog(
            title = "Description of Plots",
            tagList(
              p(
                "The radar plot and heat map are designed to help you quickly compare
                the relative score of several landscape indicators
                (see “Description of Indicators”) between selected scenarios."
              ),
              p(
                "The indicator scores, which are scaled between -1 and 1,
                represent a percent change relative to the selected baseline scenario
                (which is scaled to have a value of 0 for each indicator).
                Scenarios with scores greater than 0 for a given indicator
                have more of that value than the selected baseline scenario,
                and scenarios  with scores less than 0 have less of that value
                than the selected baseline scenario. Thus, the indicator values
                depicted in the plots are not absolute and do not represent
                a specific measurement scale, but are intended to quickly show
                how scenarios compare to each other."
              ),
              p(
                "The heat map shows this with colours, where darker colours represent
                larger scores and lighter colours represent smaller scores."
              ),
              p(
                "Note that some indicator scores may be greater than 1 or less than -1,
                and will look as if they are extending outside of the range of the plot.
                These should be considered as very different scenarios from the selected baseline,
                and more detailed review of the indicators is recommended."
              ),
              hr(),
              div(
                id = 'plot-description-example',
                class = 'plot-description-example',
                strong(em("Example:")),
                "a scenario with a score of 1 for a given indicator would equate to
                doubling (100%) that indicator compared to the baseline,
                a score of -0.5 has half (50%) of the baseline,
                and a score of zero would equate to the same indicator value as the baseline."
              )
            ),
            size = 'm',
            easyClose = TRUE,
            footer = modalButton("Dismiss")
          )
        )
      }
    )

    # Description of Indicators example modal ----
    observeEvent(
      input$indicators_descriptions_help,
      ignoreInit = TRUE,
      {
        showModal(
          modalDialog(
            title = "Indicators Example",
            tagList(
              p(
                "Let's say our baseline scenario results in:
                1,000 ha of disturbance in the Tweedsmuir subpopulation range;
                100,000 m3 harvested and 1,000,000 m3 of merchantable growing stock.
                As this is the selected baseline scenario, the indicator values
                for this scenario are scaled to 0 in the radar plot."
              ),
              p(
                "An alternative scenario that results in:
                250 ha disturbance in the Tweedsmuir subpopulation;
                75,000 m3 harvested, and 1,500,000 m3 of merchantable growing stock
                would have indicator scores of -0.75, -0.25, and 0.5, respectively,
                indicating that the alternative scenario produces 75% less disturbance,
                25% less timber volume and 50% more growing stock than the baseline scenario."
              )
            ),
            size = 'm',
            easyClose = TRUE,
            footer = modalButton("Dismiss")
          )
        )
      }
    )

    observe({
      shinyjs::toggleState(
        "baseline_scenario_apply",
        condition = (input$baseline_scenario != '')
      )
    })

    radarList = reactive(NULL)
    rl_long = reactive(NULL)
    baseline_values = reactive(NULL)

    schema_name <- schema_scenarios$schema()
    schema_label <- stringr::str_to_sentence(
      stringr::str_replace_all(schema_scenarios$schema(), pattern = '_', ' ')
    )

    updateSelectizeInput(
      session = getDefaultReactiveDomain(),
      inputId = "baseline_scenario",
      choices = schema_scenarios$scenario_names(),
      selected = input$baseline_scenario
    )

    # Apply scenario observer ----
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
      }
    )

    baseline_values <- eventReactive(

      input$baseline_scenario_apply,
      {
        bv <- isolate(
          reportList()$indicators %>%
            dplyr::filter(`scenario` == isolate(input$baseline_scenario)) %>%
            arrange(compartment, ind_name, timeperiod)
        )
        indicator_count <- length(
          bv %>%
            distinct(ind_name) %>%
            pull(ind_name)
        )

        list(
          data = bv,
          indicator_count = indicator_count
        )
      }
    )

    radarList <- eventReactive(
      input$baseline_scenario_apply,
      {
        baseline_scenario <- isolate(input$baseline_scenario)
        discount_rate <- isolate(input$discount_rate)

        # Copy the reactive object, otherwise the new object is the reference to
        # the reactive object
        dt.baseline <- data.table::copy(baseline_values()$data)
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
      }
    )

    rl <- radarList
    rl_long <- eventReactive(
      radarList(),
      {
        if (nrow(radarList()) > 0) {
          radarList() %>%
            tidyr::pivot_longer(
              cols = -1,
              names_to = 'Herd',
              values_to = 'Ratio'
            )
        }
      }
    )

    # Apply baseline scenario observer ----
    observeEvent(
      input$baseline_scenario_apply,
      ignoreInit = TRUE,
      {
        isolate({
          output$baseline_scenario_charts_container <- renderUI({
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
                plotlyOutput(
                  outputId = ns("baseline_scenario_charts"),
                  height = 250 * baseline_values()$indicator_count / 2
                ) %>%
                  withSpinner(
                    color = '#ecf0f5',
                    color.background = '#ffffff'
                  )
              )
            )
          })

          if (length(schema_scenarios$scenario_names()) > 1) {
            output$baseline_comparison <- renderUI({
              tagList(
                fluidRow(
                  div(
                    class = 'col-lg-6 col-md-12 col-sm-12 col-xs-12',
                    h4(
                      'Other scenarios compared to baseline',
                      actionLink(ns('plot_descriptions_help'), label = '', icon = icon('info-circle'))
                    )
                  )
                ),
                fluidRow(
                  div(
                    class = 'col-lg-6 col-md-12 col-sm-12 col-xs-12',
                    p(
                      'In both the radar plot and heat map, the caribou indicators
                      represent the relative proportion disturbed
                      (i.e., cutblocks less than 40 years old and roads buffered by 50m)
                      of a caribou subpopulation range (as named on the plot)
                      in the area of interest. The forestry indicators show
                      the total merchantable growing stock (m_gs) and total volume
                      of timber harvested (vol_h).'
                    ),
                    actionButton(ns('indicators_descriptions_help'), label = 'Click here for an example'),
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

            # baseline_values <- reactive({
            #
            #   # input$baseline_scenario_apply
            #
            #   # baseline_scenario <- isolate(input$baseline_scenario)
            #
            #   # req(reportList()$indicators)
            #   # browser()
            #   bv <- isolate(
            #     reportList()$indicators %>%
            #       dplyr::filter(`scenario` == isolate(input$baseline_scenario)) %>%
            #       arrange(compartment, ind_name, timeperiod)
            #   )
            #   bv
            # })

            # .. radar list ----
            # radarList <- reactive({
            #
            #   # input$baseline_scenario_apply
            #
            #   # browser()
            #   # req(schema_scenarios$apply_scenario())
            #   # req(reportList()$indicators)
            #   # req(isolate(input$discount_rate))
            #   # req((baseline_values))
            #
            #   baseline_scenario <- isolate(input$baseline_scenario)
            #   discount_rate <- isolate(input$discount_rate)
            #
            #   # Copy the reactive object, otherwise the new object is the reference to
            #   # the reactive object
            #   dt.baseline <- data.table::copy(baseline_values())
            #   dt.indicators <- data.table::copy(
            #     reportList()$indicators %>% dplyr::filter(!(`scenario` == baseline_scenario))
            #   )
            #   setnames(dt.baseline, "variable", "base_variable")
            #   dt.compare.indicators <- as.data.table(
            #     base::merge(
            #       dt.indicators,
            #       dt.baseline,
            #       by.x = c("compartment", "timeperiod", "ind_name"),
            #       by.y = c("compartment", "timeperiod", "ind_name")
            #     )
            #   )
            #
            #   out <- dt.compare.indicators[
            #     ,
            #     list(
            #       scen = sum((variable) / (1 + as.numeric(discount_rate)) ** timeperiod),
            #       base = sum((base_variable) / (1 + as.numeric(discount_rate)) ** timeperiod)
            #     ),
            #     by = c("scenario.x", "ind_name")
            #   ][, ind := (scen - base) / base]
            #   #base::merge(DT.all, DT.g, by = "scenario")
            #
            #   out %>% tidyr::pivot_wider(
            #     id_cols = 1,
            #     names_from = 'ind_name',
            #     values_from = 'ind'
            #   )
            # })
            #
            # rl <- radarList
            # if (nrow(radarList()) > 0) {
            #   rl_long <- reactive({
            #     radarList() %>%
            #       tidyr::pivot_longer(
            #         cols = -1,
            #         names_to = 'Herd',
            #         values_to = 'Ratio'
            #       )
            #   })
            # }

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
          margin = list(t = 75, r = 125, b = 75, l = 125)
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
          } else {
            output$baseline_comparison <- renderUI({
              tagList()
            })
          }

          output$baseline_scenario_charts <- renderPlotly ({
            withProgress(message = 'Making Plots', value = 0.1, {

              isolate(baseline_values()$data)
              # labeller_data <- c()
              baseline_values_annotated <- baseline_values()$data %>%
                group_by (scenario, ind_name, timeperiod) %>%
                summarise (variable = sum(variable)) %>%
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
                facet_nrow = ceiling(
                  baseline_values()$indicator_count / 2
                ),
                xlab = "Future year",
                ylab = "",
                is_plotly = TRUE,
                # strip.position = "left",
                # labeller_data = labeller_data,
                # 2 columns in each row, 225px for each row
                height = 250 * baseline_values()$indicator_count / 2 #450
              )
            })
          })
        })
      }
    )

    # Return reactive values ----
    # to be used in other modules
    if (input$baseline_scenario_apply == 0) {
      return(NULL)
    } else {
      return({
        isolate({
          list(
            radar_list = radarList,
            radar_list_long = rl_long,
            baseline_values = baseline_values,
            baseline_scenario = input$baseline_scenario,
            risk = input$discount_rate
          )
        })
      })
    }

  })
}
