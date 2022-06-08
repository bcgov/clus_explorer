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

  reportList <- reactive({

    validate(
      need(schema_scenarios$apply_scenario(), "Forbidden Access ! ")
    )

    isolate({

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
              FROM {`schema_scenarios$schema()`}.caribou_abundance where scenario IN ({schema_scenarios$scenario_names()*}) limit 1"
            ),
            conn = conn
          )) > 0) {
            data.abundance <-
              data.table(getTableQuery(
                sql = glue::glue_sql(
                  "SELECT scenario, subpop_name, timeperiod, area, core, matrix,
                  abundance_r50, abundance_c80r50, abundance_c80, abundance_avg
                  FROM {`schema_scenarios$schema()`}.caribou_abundance
                  WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
            WHERE scenario IN ({schema_scenarios$scenario_names()*}) limit 1",
            .con = conn
            ),
            conn = conn
          )) > 0) {
            data.survival <-
              data.table(getTableQuery(
                sql = glue_sql(
                  "SELECT * FROM {`schema_scenarios$schema()`}.survival
                WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
                 WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
                     WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
           WHERE scenario IN ({schema_scenarios$scenario_names()*}) limit 1",
           .con = conn
            ),
           conn = conn
          )) > 0) {
            data.grizzly_survival <-
              data.table(getTableQuery(
                sql = glue::glue_sql(
                  "SELECT * FROM {`schema_scenarios$schema()`}.grizzly_survival
               WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
           WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
               WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
            # data.fisherPoints <- base::merge(data.fisher.hexa, data.fisherOccupancy[timeperiod == 0 & scenario == schema_scenarios$scenario_names()[1], c('zone', 'reference_zone', 'rel_prob_occup')], by.x =c('zone', 'reference_zone'), by.y = c('zone', 'reference_zone'), all.y=TRUE )
            data.fisherPoints <-
              base::merge(
                data.fisher.hexa,
                data.fisherOccupancy[timeperiod == 0 &
                                       scenario == schema_scenarios$scenario_names()[1], c('zone', 'reference_zone', 'rel_prob_occup')],
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

        incProgress(amount = 1, message = "Getting harvest data...")

        # .... harvest ----
        data.harvest <- data.table(
          getTableQuery(
            sql = glue::glue_sql(
              "SELECT *
               FROM {`schema_scenarios$schema()`}.harvest
               WHERE scenario IN ({schema_scenarios$scenario_names()*});",
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
              WHERE scenario IN ({schema_scenarios$scenario_names()*})
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
              WHERE scenario IN ({schema_scenarios$scenario_names()*})
              ORDER BY scenario, rsf_model, timeperiod;",
              .con = conn
            ),
            conn = conn
          )
        )

        incProgress(amount = 1, message = "Getting summary data...")

        data.indicators <- getTableQuery(
          sql = glue_sql("WITH view1 AS (select scenario, compartment, timeperiod, m_gs as variable, 'm_gs' as ind_name from {`schema_scenarios$schema()`}.growingstock
  			where scenario in ({schema_scenarios$scenario_names()*})
  			Union All
   SELECT scenario, compartment, timeperiod, volume as variable, 'vol_h' as ind_name
  	FROM {`schema_scenarios$schema()`}.harvest where scenario in ({schema_scenarios$scenario_names()*})
  Union all
  SELECT scenario, compartment, timeperiod, sum(c40r50) as variable, split_part(critical_hab, ' ', 1) AS ind_name
  	FROM {`schema_scenarios$schema()`}.disturbance where scenario in ({schema_scenarios$scenario_names()*})
  		group by scenario, compartment, timeperiod, ind_name)
  select scenario, compartment, timeperiod, COALESCE(variable, 0) as variable, ind_name from view1
  where ind_name is not null;", .con = conn),
          conn = conn
        )

        shiny::showNotification(
          "Data has been prepared, please go to pages under Dashboard tab for further analysis.",
          duration = 5, closeButton = TRUE, type = "message"
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
          abundance = data.abundance,
          indicators = data.indicators
        )

      })
    })
  })

  observe({
    summary_data <- mod_page_dashboard_summary_server("page_dashboard_summary", schema_scenarios, reportList)
    mod_page_dashboard_caribou_server("page_dashboard_caribou", reportList)
    mod_page_dashboard_grizzly_server("page_dashboard_grizzly", reportList)
    mod_page_dashboard_forestry_server("page_dashboard_forestry", reportList)
    mod_page_dashboard_fisher_server("page_dashboard_fisher", reportList)
    mod_page_dashboard_fire_server("page_dashboard_fire", reportList)
    mod_page_report_server(
      "page_report",
      schema = schema_scenarios$schema,
      tsas = schema_scenarios$tsa_selected,
      scenario_names = schema_scenarios$scenario_names,
      scenarios = schema_scenarios$scenarios,
      data_seral_treemap = schema_scenarios$data_seral_treemap,
      reportList = reportList,
      status_thlb = schema_scenarios$status_thlb,
      status_avg_vol = schema_scenarios$status_avg_vol,
      status_road = schema_scenarios$status_road,
      radar_list = data.table::copy(summary_data$radar_list),
      radar_list_long = data.table::copy(summary_data$radar_list_long),
      baseline_values = summary_data$baseline_values,
      baseline_scenario = isolate(summary_data$baseline_scenario),
      risk = summary_data$risk
    )
  })

}
