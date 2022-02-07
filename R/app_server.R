#' The application server-side
#'
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import plotly
#' @import leaflet
#' @import DBI
#' @import RPostgreSQL
#' @import rpostgis
#' @import data.table
#' @import bsplus
#' @import igraph
#' @import sf
#' @import shinyBS
#' @import DT
#' @import dplyr
#' @import rintrojs
#' @import purrr
#' @import config
#' @import ggplot2
#' @importFrom glue glue glue_sql
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
app_server <- function(input, output, session) {
  # Your application server logic

  options(warn = -1)
  options(spinner.color = "#F0F4F8")
  options(spinner.size = 1)
  options(spinner.type = 3)

  config <- config::get()

  # Modules' server functions ----
  mod_page_home_server('page_home')
  schema_scenarios <- mod_page_scenarios_server('page_scenarios')

  # Reactives ----
  # .. column names ----
  queryColumnNames <- reactive({
    # req(schema_scenarios$schema()())
    # data.table(getTableQuery(
    #   paste0(
    #     "SELECT column_name FROM information_schema.columns
    #       WHERE table_schema = '",
    #     schema_scenarios$schema()() ,
    #     "' ",
    #     "
    #       AND table_name   = '",
    #     input$queryTable,
    #     "'"
    #   )
    # ))
  })

  # .. available column names ----
  availableMapLayers <- reactive({
    # req(schema_scenarios$schema()())
    # req(schema_scenarios$scenario()())
    # #print(paste0("SELECT r_table_name FROM public.raster_columns WHERE r_table_schema = '", schema_scenarios$schema() , "' ;"))
    # #print(getSpatialQuery(paste0("SELECT r_table_name FROM public.raster_columns WHERE r_table_schema = '", schema_scenarios$schema() , "' ;")))
    # getTableQuery(
    #   paste0(
    #     "SELECT r_table_name FROM public.raster_columns WHERE r_table_schema = '",
    #     schema_scenarios$schema()() ,
    #     "' ;"
    #   )
    # )$r_table_name
  })


  # .. fisher points filter ----
  fisherPointsFilter <- reactive({
    req(reportList())
    req(input$fisheryear)
    req(input$fisher_scenario_selected)
    merge(
      reportList()$fisherPts[, c('zone', 'reference_zone', 'size', 'x', 'y')],
      reportList()$fisher[timeperiod == input$fisheryear &
                            scenario == input$fisher_scenario_selected, c('zone', 'reference_zone', 'rel_prob_occup')],
      by.x = c('zone', 'reference_zone'),
      by.y = c('zone', 'reference_zone'),
      all.y = TRUE
    )
  })

  # Observers ----
  observeEvent(input$getMapLayersButton, {
    withProgress(message = 'Loading layers', value = 0.1, {
      mapLayersStack <-
        getRasterQuery(c(schema_scenarios$schema(), tolower(input$maplayers)))
      mapLayersStack[mapLayersStack[] == 0] <- NA
    })
    cb <-
      colorBin("Spectral", domain = 1:200,  na.color = "#00000000")
    leafletProxy("resultSetRaster", session) %>%
      clearTiles() %>%
      clearImages() %>%
      clearControls() %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
      addProviderTiles("Esri.DeLorme", group = "DeLorme") %>%
      addRasterImage(
        mapLayersStack,
        colors = cb,
        opacity = 0.8,
        project = TRUE,
        group = "Selected"
      ) %>%
      addLegend(pal = cb, values = 1:200) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "WorldImagery", "DeLorme"),
        overlayGroups = "Selected"
      ) %>%
      addScaleBar(position = "bottomleft")
  })

  observeEvent(input$fisheryear, {
    pal <- colorNumeric(palette = 'Blues',
                        domain = reportList()$fisherPts$rel_prob_occup)
    leafletProxy("fishermapper", data = fisherPointsFilter(), session) %>%
      clearShapes() %>%
      addCircles(
        lat = ~ y,
        lng = ~ x,
        fillColor = ~ pal(fisherPointsFilter()$rel_prob_occup),
        color =  ~ pal(fisherPointsFilter()$rel_prob_occup),
        radius = fisherPointsFilter()$size * 100,
        popup = ~ paste0(
          "ref:",
          reference_zone,
          " zone:",
          zone,
          " occupancy:",
          rel_prob_occup
        )
      )
  })

  observe({
    updateSelectInput(
      session,
      "queryColumns",
      choices = queryColumnNames()$column_name,
      selected = character(0)
    )
    updateSelectInput(
      session,
      "queryRows",
      choices = queryColumnNames()$column_name,
      selected = character(0)
    )
  })

  observe({
    #print(availableMapLayers())
    updateSelectInput(session,
                      "maplayers",
                      choices = availableMapLayers(),
                      selected = character(0))
  })

  # observe({
  #   # req(schema_scenarios$scenario())
  #   updateSelectInput(
  #     session,
  #     "fisher_scenario_selected",
  #     choices = schema_scenarios$scenario(),
  #     selected = character(0)
  #   )
  # })

  # Outputs ----


  # # Query Builder
  # output$resultSetTable <- renderDataTable({
  #   #print(paste0("SELECT ", paste(c(input$queryRows,input$queryColumns), sep="' '", collapse=", "), " FROM ", schema_scenarios$schema(), ".", input$queryTable, " WHERE scenario IN ('", paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"), "') GROUP BY ", input$queryColumns))
  #   data.table(getTableQuery(
  #     paste0(
  #       "SELECT scenario, ",
  #       paste(
  #         c(
  #           paste0(input$queryValue, "(", input$queryRows, ")"),
  #           input$queryColumns
  #         ),
  #         sep = "' '",
  #         collapse = ", "
  #       ),
  #       " FROM ",
  #       schema_scenarios$schema(),
  #       ".",
  #       input$queryTable,
  #       " WHERE scenario IN ('",
  #       paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
  #       "') GROUP BY scenario, ",
  #       input$queryColumns,
  #       " ORDER BY ",
  #       input$queryColumns
  #     )
  #   ))
  # })
  #
  # # Mapviewer
  # output$resultSetRaster <- renderLeaflet({
  #   leaflet(options = leafletOptions(doubleClickZoom = TRUE)) %>%
  #     setView(-124.87, 54.29, zoom = 5) %>%
  #     addTiles() %>%
  #     addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  #     addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
  #     addProviderTiles("Esri.DeLorme", group = "DeLorme") %>%
  #     addScaleBar(position = "bottomright") %>%
  #     addLayersControl(baseGroups = c("OpenStreetMap", "WorldImagery", "DeLorme"))
  #
  # })
  #
  #

  reportList <- reactive({

    # .. report list ----
    req(schema_scenarios$schema())
    req(schema_scenarios$scenario())
    req(schema_scenarios$apply_scenario())

    # .... abundance ----
    withProgress(
      message = "Getting abundance data...", min = 0, max = 9, {

        # Reusable db connection
        conn = getDbConnection()

        if (nrow(getTableQuery(
          sql = glue_sql(
            "SELECT * FROM information_schema.tables WHERE table_schema =
            {schema_scenarios$schema()}
            AND table_name = 'caribou_abundance'",
            .con = conn
          ),
          conn = conn
        )) > 0) {
          if (nrow(getTableQuery(
            sql = glue_sql(
              "SELECT scenario, subpop_name, timeperiod,  area, core, matrix, abundance_r50, abundance_c80r50, abundance_c80, abundance_avg
              FROM {`schema_scenarios$schema()`}.caribou_abundance where scenario IN ({schema_scenarios$scenario()*}) limit 1"
            ),
            conn = conn
          )) > 0) {
            data.abundance <-
              data.table(getTableQuery(
                sql = glue::glue_sql(
                  "SELECT scenario, subpop_name, timeperiod, area, core, matrix,
                  abundance_r50, abundance_c80r50, abundance_c80, abundance_avg
                  FROM {`schema_scenarios$schema()`}.caribou_abundance
                  WHERE scenario IN ({schema_scenarios$scenario()*})
                  ORDER BY scenario, subpop_name, timeperiod;",
                  .con = conn
                ),
                conn = conn
              ))
            data.abundance <-
              data.abundance [, lapply(.SD, weighted.mean, w = area), by = c("scenario",  "subpop_name", "timeperiod"), .SDcols = c(
                "core",
                "matrix",
                "abundance_r50",
                "abundance_c80r50",
                "abundance_c80",
                "abundance_avg"
              )]
          } else{
            data.abundance <- NULL
          }
        } else{
          data.abundance <- NULL
        }

        incProgress(amount = 1, message = "Getting survival data...")

        # .... survival ----
        if (nrow(getTableQuery(
          sql = glue_sql(
            "SELECT * FROM information_schema.tables
          WHERE table_schema = {schema_scenarios$schema()}
          AND table_name = 'survival'",
          .con = conn
          ),
          conn = conn
        )) > 0) {
          if (nrow(getTableQuery(
            sql = glue_sql(
              "SELECT * FROM {`schema_scenarios$schema()`}.survival
            WHERE scenario IN ({schema_scenarios$scenario()*}) limit 1",
            .con = conn
            ),
            conn = conn
          )) > 0) {
            data.survival <-
              data.table(getTableQuery(
                sql = glue_sql(
                  "SELECT * FROM {`schema_scenarios$schema()`}.survival
                WHERE scenario IN ({schema_scenarios$scenario()*})
                ORDER BY scenario, herd_bounds, timeperiod;",
                .con = conn
                ),
                conn = conn
              ))
            data.survival <-
              data.survival[, lapply(.SD, weighted.mean, w = area),
                            by = c("scenario",  "herd_bounds", "timeperiod"),
                            .SDcols = c("prop_age", "prop_mature", "prop_old", "survival_rate")]
          } else{
            data.survival <- NULL
          }
        } else{
          data.survival <- NULL
        }

        incProgress(amount = 1, message = "Getting disturbance data...")

        # .... disturbance ----
        if (nrow(getTableQuery(
          sql = glue::glue_sql(
            "SELECT * FROM information_schema.tables
            WHERE table_schema = {schema_scenarios$schema()}
            AND table_name = 'disturbance'",
            .con = conn
          ),
          conn = conn
        )
        ) > 0) {
          if (nrow(getTableQuery(
            sql = glue::glue_sql(
              "SELECT * FROM {`schema_scenarios$schema()`}.disturbance
                 WHERE scenario IN ({schema_scenarios$scenario()*})
                 LIMIT 1",
              .con = conn
            ),
            conn = conn
          )) > 0) {
            data.disturbance <-
              data.table (getTableQuery(
                sql = glue::glue_sql(
                  "SELECT scenario, timeperiod, critical_hab, sum(c40r500) as c40r500,
                      sum(c40r50) as c40r50, sum(total_area) as total_area
                     FROM {`schema_scenarios$schema()`}.disturbance
                     WHERE scenario IN ({schema_scenarios$scenario()*})
                    GROUP BY scenario, critical_hab, timeperiod
                    ORDER BY scenario, critical_hab, timeperiod",
                  .con = conn
                ),
                conn = conn
              ))
            # c40r50 = dist; c40r500 = dist500 }
            data.disturbance <-
              data.disturbance[, dist_per := c40r50 / total_area][, dist500_per := c40r500 /
                                                                    total_area]
          } else{
            data.disturbance <- NULL
          }
        } else{
          data.disturbance <- NULL
        }

        incProgress(amount = 1, message = "Getting grizzly survival data...")

        # .... grizzly survival ----
        if (nrow(getTableQuery(
          sql = glue::glue_sql(
            "SELECT * FROM information_schema.tables
            WHERE table_schema = {schema_scenarios$schema()}
            AND table_name = 'grizzly_survival'",
            .con = conn
          ),
          conn = conn
        )) > 0) {
          if (nrow(getTableQuery(
            sql = glue::glue_sql(
              "SELECT * FROM {`schema_scenarios$schema()`}.grizzly_survival
           WHERE scenario IN ({schema_scenarios$scenario()*}) limit 1",
           .con = conn
            ),
           conn = conn
          )) > 0) {
            data.grizzly_survival <-
              data.table(getTableQuery(
                sql = glue::glue_sql(
                  "SELECT * FROM {`schema_scenarios$schema()`}.grizzly_survival
               WHERE scenario IN ({schema_scenarios$scenario()*})
               ORDER BY scenario, gbpu_name, timeperiod;",
               .con = conn
                ),
               conn = conn
              )
              )
            data.grizzly_survival <-
              data.grizzly_survival[, lapply(.SD, weighted.mean, w = total_area), by =
                                      c("scenario",  "gbpu_name", "timeperiod"), .SDcols = c("road_density", "survival_rate")]
          } else{
            data.grizzly_survival <- NULL
          }
        } else{
          data.grizzly_survival <- NULL
        }

        incProgress(amount = 1, message = "Getting fire data...")

        # .... fire ----
        data.fire <-
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT * FROM fire where herd_bounds IN ({unique(data.survival$herd_bounds)*});",
              .con = conn
            ),
            conn = conn
          )
        data.fire2 <-
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT
              herd_name, habitat, round(cast(mean_ha2 as numeric), 1) as mean,
              round(cast(mean_area_percent as numeric), 1) as percent, round(cast(max_ha2 as numeric), 1) as max,
              round(cast(min_ha2 as numeric), 1) as min,
              round(cast(cummulative_area_ha2 as numeric), 1) as cummulative,
              round(cast(cummulative_area_percent as numeric), 1) as cummul_percent
           FROM firesummary where herd_bounds IN (
      {unique(data.survival$herd_bounds)*});",
      .con = conn
            ),
      conn = conn
          )

        incProgress(amount = 1, message = "Getting fisher data...")

        # .... fisher ----
        if (nrow(getTableQuery(
          sql = glue::glue_sql(
            "SELECT * FROM information_schema.tables
      WHERE table_schema = {schema_scenarios$schema()}
      AND table_name = 'fisher'",
      .con = conn
          ),
      conn = conn
        )) > 0) {
          if (nrow(getTableQuery(
            sql = glue::glue_sql(
              "SELECT * FROM {`schema_scenarios$schema()`}.fisher
           WHERE scenario IN ({schema_scenarios$scenario()*})
           LIMIT 1",
           .con = conn
            ),
           conn = conn
          )) > 0) {
            data.fisherOccupancy <-
              data.table(getTableQuery(
                sql = glue::glue_sql(
                  "SELECT rel_prob_occup, zone, reference_zone, timeperiod, scenario
               FROM {`schema_scenarios$schema()`}.fisher
               WHERE scenario IN ({schema_scenarios$scenario()*})
               ORDER BY scenario, timeperiod;",
               .con = conn
                ),
               conn = conn
              ))
            data.fisher.hexa <-
              data.table(
                getTableQuery(
                  sql = "SELECT x,y, size, ogc_fid as zone, reference_zone FROM public.fisher_territory_pts",
                  conn = conn
                )
              )
            data.fisherPoints <-
              merge(
                data.fisher.hexa,
                data.fisherOccupancy[timeperiod == 0 &
                                       scenario == schema_scenarios$scenario()[1], c('zone', 'reference_zone', 'rel_prob_occup')],
                by.x = c('zone', 'reference_zone'),
                by.y = c('zone', 'reference_zone'),
                all.y = TRUE
              )
          } else{
            data.fisherPoints <- NULL
            data.fisherOccupancy <- NULL
          }
        } else{
          data.fisherPoints <- NULL
          data.fisherOccupancy <- NULL
        }

        # reportList <- reactive({
        # req(schema_scenarios$schema())
        # req(schema_scenarios$scenario())
        #

        incProgress(amount = 1, message = "Getting harvest data...")

        # .... harvest ----
        data.harvest <- data.table(
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT *
               FROM {`schema_scenarios$schema()`}.harvest
               WHERE scenario IN ({schema_scenarios$scenario()*});",
              .con = conn
            ),
            conn = conn
          )
        )

        incProgress(amount = 1, message = "Getting growing stock data...")

        # .... growing stock ----
        data.growingstock <- data.table(
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT scenario, timeperiod, sum(m_gs) as growingstock
              FROM {`schema_scenarios$schema()`}.growingstock
              WHERE scenario IN ({schema_scenarios$scenario()*})
              GROUP BY scenario, timeperiod;",
              .con = conn
            ),
            conn = conn
          )
        )

        incProgress(amount = 1, message = "Getting rsf data...")

        # .... rsf ----
        data.rsf <- data.table(
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT * FROM {`schema_scenarios$schema()`}.rsf
              WHERE scenario IN ({schema_scenarios$scenario()*})
              ORDER BY scenario, rsf_model, timeperiod;",
              .con = conn
            ),
            conn = conn
          )
        )

        list(
          harvest = data.harvest,
          growingstock = data.growingstock,
          rsf = data.rsf,
          survival = data.survival,
          disturbance = data.disturbance,
          fire = data.fire,
          fire2 = data.fire2,
          fisher = data.fisherOccupancy,
          fisherPts = data.fisherPoints,
          grizzly_survival = data.grizzly_survival,
          abundance = data.abundance
        )

        # })

      })
  })

  observe({
    mod_page_dashboard_summary_server("page_dashboard_summary", schema_scenarios, reportList)
    mod_page_dashboard_caribou_server("page_dashboard_caribou", reportList)
    mod_page_dashboard_grizzly_server("page_dashboard_grizzly", reportList)
    mod_page_dashboard_climate_server("page_dashboard_climate", reportList)
    mod_page_dashboard_forestry_server("page_dashboard_forestry", reportList)
    mod_page_dashboard_fire_server("page_dashboard_fire", reportList)
  })

}
