#' page_scenarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_scenarios_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          title = tagList(
            span("Area of interest"),
            icon('info-circle') %>%
              bsplus::bs_embed_tooltip(
                "Select an area of interest to see the list of available scenarios",
                "right"
              )
          ),
          solidHeader = FALSE,
          selectizeInput(
            inputId = ns("schema"),
            label = NULL,
            selected = "" ,
            choices = c(
              list("Select an area" = ""),
              data_global$available_study_areas
            )
          )
        ),
        conditionalPanel(
          condition = paste0("$('#", ns("schema"), "').val() != ''"),
          box(
            width = 12,
            title = "Current State",
            solidHeader = FALSE,
            fluidRow(
              box(
                title = "Landbase",
                solidHeader = TRUE,
                # background = "green",
                width = 12,
                fluidRow(
                  width = 12,
                  column(
                    width = 3,
                    div(
                      infoBoxOutput(ns("statusTHLB")) %>%
                        withSpinner(color.background = '#fff', color = '#ffffff')
                    ) %>%
                      bsplus::bs_embed_tooltip(
                        "Percentage of timber harvesting landbase in the area of interest",
                        "top"
                      )
                  ),
                  column(
                    width = 3,
                    offset = 1,
                    div(
                      infoBoxOutput(ns("statusRoad")) %>%
                        withSpinner(color.background = '#fff')
                    ) %>%
                      bsplus::bs_embed_tooltip(
                        "Percentage of the area of interest within 100m of a road",
                        "top"
                      )
                  ),
                  column(
                    width = 3,
                    offset = 1,
                    div(
                      infoBoxOutput(ns("statusAvgVol")) %>%
                        withSpinner(color.background = '#fff', color = '#ffffff')
                    ) %>%
                      bsplus::bs_embed_tooltip(
                        "Average volume (m3) per ha in THLB",
                        "top"
                      )
                  )
                )
              ),
              box(
                title = "Seral stage",
                solidHeader = FALSE,
                # background = "green",
                width = 12,
                fluidRow(
                  width = 12,
                  column(
                    width = 12,
                    tags$p(
                      class = 'chart-title',
                      'Proportion of forest by age group'
                    ),
                    tags$p(
                      class = 'chart-subtitle',
                      'Early (<40 yrs), mature (60 - 120 yrs) and old (> 120 yrs).'
                    ),
                    mod_chart_treemap_ui(ns('statusPlot'), chart_height = '350px') %>%
                      withSpinner(color.background = '#fff')
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                tags$h4("Timber Supply Area(s):"),
                selectInput(
                  ns("tsa_selected"),
                  choices = NULL,
                  label = '',
                  width = '100%',
                  multiple = T
                ),
                bsTooltip(
                  "tsa_selected",
                  "Select timber supply area(s).",
                  "bottom",
                  options = list(container = "body")
                )
              )
            )
          )#end of current state box
        )
      ),
      column(
        width = 6,
        box(
          title = tagList(

            span("Scenarios"),
            icon('info-circle') %>%
              bsplus::bs_embed_tooltip(
                "Select the scenarios you wish to compare, then see Dashboard tab for indicators.",
                "right"#,
                # delay = seconds(5)
              )
          ),
          width = 12,
          solidHeader = TRUE,
          p(
            "Scenarios are ordered by ",
            code("rank"),
            ". The values of ",
            code("rank"),
            " correspond to the cubic meter of harvested volume per area of caribou
              critical habitat disturbed. Scenarios with larger rank values are potentially better."
          ),
          # uiOutput(ns('selected_scenarios_list')),
          # DT::dataTableOutput(ns('scenarios')),
          # uiOutput(ns("scenarios"))
          checkboxGroupInput(
            inputId = ns("scenario"), label = NULL, selected = NULL, choiceNames = NULL
          ),
          actionButton(ns("apply_scenario"), label = "Apply", class = "btn-clus")
        )
      )
    )
  )
}

#' page_scenarios Server Functions
#'
#' @noRd
mod_page_scenarios_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive values ----

    # .. available scenarios ----
    scenariosList <- reactive({
      req(input$schema)

      conn = getDbConnection()

      scenario_list <- data.table(
        getTableQuery(
          sql = glue::glue_sql(
            "SELECT scenario, description, COALESCE(rank, 0) AS rank
            FROM {`input$schema`}.scenarios
            ORDER BY rank DESC",
            .con = conn
          ),
          conn = conn
        )
      )

      dbDisconnect(conn)

      scenario_list
    })

    # .. status data ----
    statusData <- reactive({
      req(input$schema)

      conn = getDbConnection()

      status_data <- data.table(
        getTableQuery(
          sql = glue::glue_sql(
            "SELECT
              a.compartment as compartment, gs, (gs/thlb) as avg_m3_ha, aoi, total,
              thlb, early, mature, old, road, c40r500, c40r50, total_area
            FROM (
              SELECT compartment, MAX(m_gs) as gs
              FROM {`input$schema`}.growingstock
              WHERE timeperiod = 0
              GROUP BY compartment
            ) a
            LEFT JOIN (
              SELECT *
              FROM {`input$schema`}.state
            ) b ON b.compartment = a.compartment
            LEFT JOIN (
              SELECT SUM(c40r500) as c40r500, SUM(c40r50) as c40r50,
                SUM(total_area) as total_area, compartment
  		        FROM {`input$schema`}.disturbance
              WHERE timeperiod = 0
              AND scenario = (
                SELECT scenario
                FROM {`input$schema`}.disturbance LIMIT 1)
                GROUP BY compartment
            ) c ON c.compartment = a.compartment;",
            .con = conn
          ),
          conn = conn
        )
      )

      dbDisconnect(conn)

      status_data
    })

    # .. data for selected schema ----
    data <- reactive(statusData()[compartment %in% input$tsa_selected, ])

    # .... treemap data ----
    data_seral_treemap <- reactive({
      data_seral_treemap <- data.table(
        reshape2::melt(
          data()[, c("compartment", "early", "mature", "old")],
          id.vars = "compartment",
          measure.vars = c("early", "mature", "old")
        )
      )
      data_seral_treemap <- data_seral_treemap[, sum(value), by = list(variable)]

      data_seral_treemap <- data_seral_treemap %>% mutate(
        parent_col = 'Total area'
      )
      data_total <- data_seral_treemap %>%
        group_by(parent_col) %>%
        summarise(V1 = sum(V1)) %>%
        ungroup()

      data_seral_treemap <- bind_rows(data_total, data_seral_treemap)
    })

    # .... landbase ----
    status_thlb <- reactive({
      round(
        (sum(data()$thlb) / sum(data()$total)) * 100, 0
      )
    })

    status_road <- reactive({
      round(
        (sum(data()$road) / sum(data()$total)) * 100, 0
      )
    })

    status_avg_vol <- reactive({
      round(
        (sum(data()$gs) / sum(data()$thlb)), 0
      )
    })

    # Observers ----
    observe({
      updateSelectInput(
        session,
        "tsa_selected",
        choices = statusData()$compartment,
        selected = statusData()$compartment
      )
    })

    # Render data table ----
#    observe({
#     sl <- scenariosList()
#     sl <- sl %>%
#       mutate(
#         selection = sprintf('<input type="checkbox" name="scenario_cbg" value="%s"/>', scenario),
#         rank = round(rank, 2),
#         tooltip = sprintf('
# <i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon" title="" data-toggle="tooltip" data-placement="top" data-delay="5s" data-original-title="%s"></i>
#         ', description)
#       ) %>%
#       select(selection, scenario, rank, tooltip)
#
#     output$scenarios = DT::renderDataTable(
#         sl, escape = FALSE, selection = 'none', server = FALSE,
#         colnames = c('', 'Scenario', 'Rank', 'Description'),
#         options = list(dom = 't', paging = FALSE, ordering = FALSE, rownames = FALSE),
#         callback = JS("table.rows().every(function(i, tab, row) {
#             var $this = $(this.node());
#             $this.attr('id', this.data()[0]);
#             $this.addClass('shiny-input-checkboxgroup');
#             $this.addClass('shiny-options-group');
#           });")
#           # Shiny.unbindAll(table.table().node());
#           # Shiny.bindAll(table.table().node());
#       )
#   })

    # .. render scenarios table ----
#     observe({
#       output$scenarios <- renderUI({
#         scenarios <- scenariosList()
#
#         # Generate table rows
#         trows <- unlist(
#           lapply(1:nrow(scenarios), function(i) {
#             paste0('
#               <tr class="shiny-input-checkboxgroup shiny-bound-input">
#                 <td class="checkbox-container">', tags$input(type = "checkbox", name = ns("scenario"), value = scenarios[i, 1]), '</td>
#                 <td>', scenarios[i, 1], '</td>
#                 <td>', round(scenarios[i, 3], 2), '</td>
#                 <td class="description">',
#                   icon('info-circle') %>%
#                     bsplus::bs_embed_tooltip(scenarios[i, 2], "top", delay = "5s"),
#                 '</td>
#               </tr>'
#             )
#           })
#         )
#
#         # Table header
#         scenarios_table <- paste0('
#           <table id="scenario-list" class="clus-explorer-table">
#             <thead>
#               <tr>
#           		  <th></th>
#           		  <th>Scenario</th>
#           		  <th>Rank</th>
#           		  <th>Description</th>
#           	  </tr>
#             </thead>' ,
#             paste(trows, collapse=" "), '
#           </table>'
# 	      )
#
#       	div(
#       	  id = "scenario-list-container",
#       	  class="form-group shiny-input-checkboxgroup shiny-input-container",
#       		HTML(scenarios_table)
#       	)
#       })
#     })


   # Update Checkbox Group ----
    observe({ #Scenarios based on the area of interest selected
      updateCheckboxGroupInput(
        session, "scenario",
        # label = sprintf(
        #   '%s (Rank %s) $s',
        #   scenariosList()$scenario,
        #   round(scenariosList()$rank, 2),
        #   '<i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon" title="" data-toggle="tooltip" data-placement="top" data-delay="5s" data-original-title="%s"></i>'
        # ),
        # choices = scenariosList()$scenario,
        choiceValues = scenariosList()$scenario,
        choiceNames = sprintf(
          '%s (Rank %s)',
          scenariosList()$scenario,
          round(scenariosList()$rank, 2)
            # ),
         #  ),
         # icon('info-circle') %>%
         #   bsplus::bs_embed_tooltip(scenariosList()$description, "top", delay = "5s")
        ),
        selected = character(0)
      )
    })

    # .. render treemap chart ----
    observeEvent(
      input$tsa_selected,
      ignoreInit = TRUE,
      {
        # data_seral_treemap(),
        # data_seral_treemap <- data.table(
        #   reshape2::melt(
        #     data()[, c("compartment", "early", "mature", "old")],
        #     id.vars = "compartment",
        #     measure.vars = c("early", "mature", "old")
        #   )
        # )
        # data_seral_treemap <- data_seral_treemap[, sum(value), by = list(variable)]
        #
        # data_seral_treemap <- data_seral_treemap %>% mutate(
        #   parent_col = 'Total area'
        # )
        # data_total <- data_seral_treemap %>%
        #   group_by(parent_col) %>%
        #   summarise(V1 = sum(V1)) %>%
        #   ungroup()
        #
        # data_seral_treemap <- bind_rows(data_total, data_seral_treemap)

        mod_chart_treemap_server(
          'statusPlot',
          data = data_seral_treemap(),
          # data = data_seral_treemap,
          col_parent = parent_col,
          col_child = variable,
          col_value = V1,
          chart_type = 'treemap',
          text_info = "label+text+percent root",
          colors = c('#A3D9A5', '#3F9142', '#0E5814'),
          tiling_packing = 'dice-slce'
        )
      }
    )

    # .. render info boxes ----
    output$statusTHLB <- renderInfoBox({
      infoBox(
        title = NULL,
        subtitle = "THLB",
        value = tags$p(
          scales::percent(status_thlb(), accuracy = 0.1, scale = 1), style = "font-size: 160%;"
        ),
        icon = icon("images"),
        color = "green"
      )
    })

    output$statusAvgVol <- renderInfoBox({
      infoBox(
        title = NULL,
        subtitle = "m3/ha",
        value = tags$p(
          status_avg_vol(), style = "font-size: 160%;"
        ),
        icon = icon("seedling"),
        color = "green"
      )
    })

    output$statusRoad <- renderInfoBox({
      infoBox(
        title = NULL,
        subtitle = "Road",
        value = tags$p(
          scales::percent(status_road(), accuracy = 0.1, scale = 1),  style = "font-size: 160%;"
        ),
        icon = icon("road"),
        color = "green"
      )
    })

    # Return reactive values ----
    # to be used in other modules
    return(
      list(
        schema = reactive({input$schema}),
        tsa_selected = reactive({input$tsa_selected}),
        scenario_names = reactive({input$scenario}),
        scenarios = scenariosList,
        apply_scenario = reactive({input$apply_scenario}),
        data_seral_treemap = data_seral_treemap,
        status_thlb = status_thlb,
        status_avg_vol = status_avg_vol,
        status_road = status_road
      )
    )

  })
}
