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


  # output$numberFisherTerritory <- renderValueBox({
  #   valueBoxSpark(
  #     value = paste0(as.integer(nrow(
  #       reportList()$fisher[timeperiod == input$fisheryear &
  #                             scenario == input$fisher_scenario_selected &
  #                             rel_prob_occup > 0.55, "zone"]
  #     ))),
  #     title = toupper("Territories"),
  #     subtitle = NULL,
  #     icon = icon("times-circle"),
  #     width = 4,
  #     color = "blue"
  #   )
  # })
  #
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
  # output$fishermapper <- renderLeaflet({
  #   pal <- colorNumeric(palette = 'Blues',
  #                       domain = reportList()$fisherPts$rel_prob_occup)
  #   leaflet(reportList()$fisherPts) %>%
  #     addTiles() %>%
  #     fitBounds( ~ min(x), ~ min(y), ~ max(x), ~ max(y)) %>%
  #     addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  #     addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
  #     addProviderTiles("Esri.DeLorme", group = "DeLorme") %>%
  #     addScaleBar(position = "bottomright") %>%
  #     addLayersControl(baseGroups = c("OpenStreetMap", "WorldImagery", "DeLorme")) %>%
  #     addCircles(
  #       lat = ~ y,
  #       lng = ~ x,
  #       fillColor = ~ pal(reportList()$fisherPts$rel_prob_occup),
  #       color =  ~ pal(reportList()$fisherPts$rel_prob_occup),
  #       radius = reportList()$fisherPts$size * 100,
  #       popup = ~ paste0(
  #         "ref:",
  #         reference_zone,
  #         " zone:",
  #         zone,
  #         " occupancy:",
  #         rel_prob_occup
  #       )
  #     ) %>%
  #     addLegend(
  #       position = "topright",
  #       pal = pal,
  #       values = ~ reportList()$fisherPts$rel_prob_occup,
  #       title = "Prob"
  #     )
  # })
  #
  # output$climatemap <- renderPlot({
  #   plot.igraph(climateMap[[1]], layout = climateMap[[2]])
  #   legend(
  #     'topleft',
  #     legend = c(
  #       "Climate",
  #       "Anthropogenic",
  #       "Landscape Condition",
  #       "Predator-prey",
  #       "Energetics",
  #       "Health",
  #       "Population"
  #     ),
  #     col = c(
  #       "yellow",
  #       "orange",
  #       "purple",
  #       "lightblue",
  #       "pink",
  #       "red",
  #       "green"
  #     ),
  #     pch = 19,
  #     bty = 'n',
  #     cex = 1.7
  #   )
  # })
  #
  # output$harvestAreaPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <-
  #       reportList()$harvest[, sum(area), by = c("scenario", "timeperiod")]
  #     data$scenario <-
  #       reorder(data$scenario, data$V1, function(x)
  #         - max(x))
  #     data[, timeperiod := as.integer(timeperiod)]
  #     p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
  #       geom_area(position = "identity", aes(alpha = scenario)) +
  #       xlab ("Future year") +
  #       ylab ("Area Harvested (ha)") +
  #       scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #       scale_alpha_discrete(range = c(0.4, 0.8)) +
  #       scale_fill_grey(start = 0.8, end = 0.2) +
  #       theme_bw()
  #     ggplotly(p)
  #   })
  # })
  #
  # output$harvestAgePlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <- reportList()$harvest
  #     data <-
  #       data[, lapply(.SD, FUN = weighted.mean, x = age), by = c("timeperiod", "scenario"), .SDcols =
  #              'age']
  #     #data$scenario <- reorder(data$scenario, data$V1, function(x) -max(x) )
  #     data[, timeperiod := as.integer(timeperiod)]
  #     p <- ggplot (data, aes (x = timeperiod, y = age, fill = scenario)) +
  #       geom_area(position = "identity", aes(alpha = scenario)) +
  #       xlab ("Future year") +
  #       ylab ("Average Harvest Age (yrs)") +
  #       scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #       scale_alpha_discrete(range = c(0.4, 0.8)) +
  #       scale_fill_grey(start = 0.8, end = 0.2) +
  #       theme_bw()
  #     ggplotly(p)
  #   })
  # })
  #
  # output$harvestVolumePlot <- renderPlotly ({
  #   data <-
  #     reportList()$harvest[, sum(volume), by = c("scenario", "timeperiod")]
  #   data$scenario <-
  #     reorder(data$scenario, data$V1, function(x)
  #       - max(x))
  #   data[, timeperiod := as.integer(timeperiod)]
  #   p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
  #     geom_area(position = "identity", aes(alpha = scenario)) +
  #     xlab ("Future year") +
  #     ylab ("Volume Harvested (m3)") +
  #     scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #     scale_alpha_discrete(range = c(0.4, 0.8)) +
  #     scale_fill_grey(start = 0.8, end = 0.2) +
  #     theme_bw()
  #   ggplotly(p)
  # })
  #
  # output$managedAreaPlot <- renderPlotly ({
  #   data <-
  #     reportList()$harvest[, sum(transition_area), by = c("scenario", "timeperiod")]
  #   data$scenario <-
  #     reorder(data$scenario, data$V1, function(x)
  #       - max(x))
  #   data[, timeperiod := as.integer(timeperiod)]
  #   p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
  #     geom_area(position = "identity", aes(alpha = scenario)) +
  #     xlab ("Future year") +
  #     ylab ("Managed Area Harvested (ha)") +
  #     scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #     scale_alpha_discrete(range = c(0.4, 0.8)) +
  #     scale_fill_grey(start = 0.8, end = 0.2) +
  #     theme_bw()
  #   ggplotly(p)
  # })
  #
  # output$managedVolumePlot <- renderPlotly ({
  #   data <-
  #     reportList()$harvest[, sum(transition_volume), by = c("scenario", "timeperiod")]
  #   data$scenario <-
  #     reorder(data$scenario, data$V1, function(x)
  #       - max(x))
  #   data[, timeperiod := as.integer(timeperiod)]
  #   p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
  #     geom_area(position = "identity", aes(alpha = scenario)) +
  #     xlab ("Future year") +
  #     ylab ("Managed Volume Harvested (m3)") +
  #     scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #     scale_alpha_discrete(range = c(0.4, 0.8)) +
  #     scale_fill_grey(start = 0.8, end = 0.2) +
  #     theme_bw()
  #   ggplotly(p)
  # })
  #
  # output$availableTHLBPlot <- renderPlotly ({
  #   data <-
  #     reportList()$harvest[, sum(avail_thlb), by = c("scenario", "timeperiod")]
  #   data$scenario <-
  #     reorder(data$scenario, data$V1, function(x)
  #       - max(x))
  #   data[, timeperiod := as.integer(timeperiod)]
  #   p <- ggplot (data, aes (x = timeperiod, y = V1, fill = scenario)) +
  #     geom_area(position = "identity", aes(alpha = scenario)) +
  #     xlab ("Future year") +
  #     ylab ("Available THLB (ha)") +
  #     scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #     scale_alpha_discrete(range = c(0.4, 0.8)) +
  #     scale_fill_grey(start = 0.8, end = 0.2) +
  #     theme_bw()
  #   ggplotly(p)
  # })
  #
  # output$growingStockPlot <- renderPlotly ({
  #   data <- reportList()$growingstock
  #   data$scenario <-
  #     reorder(data$scenario, data$growingstock, function(x)
  #       - max(x))
  #   p <-
  #     ggplot(data, aes (x = timeperiod, y = growingstock, fill = scenario)) +
  #     geom_area(position = "identity", aes(alpha = scenario)) +
  #     xlab ("Future year") +
  #     ylab ("Growing Stock (m3)") +
  #     scale_x_continuous(breaks = seq(0, max(data$timeperiod), by = 10)) +
  #     scale_alpha_discrete(range = c(0.4, 0.8)) +
  #     scale_fill_grey(start = 0.8, end = 0.2) +
  #     theme_bw()
  #   ggplotly(p)
  # })
  #
  # output$abundancePlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <- reportList()$abundance
  #
  #     p <-
  #       ggplot(data,
  #              aes (x = timeperiod, y = abundance_avg, color = scenario)) +
  #       facet_grid (rows = vars(subpop_name)) +
  #       geom_line () +
  #       xlab ("Future year") +
  #       ylab ("Abundance") +
  #       scale_x_continuous (limits = c(0, 50),
  #                           breaks = seq (0, 50, by = 10)) +
  #       theme_bw () +
  #       theme (legend.title = element_blank(),
  #              plot.caption = element_text (hjust = 0))
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #
  #   })
  # })
  #
  # output$survivalPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <- reportList()$survival
  #     data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, herd_bounds)]  # replace first() with shift() to get difference with previous year value instead of first year value
  #
  #     p <-
  #       ggplot(data,
  #              aes (x = timeperiod, y = survival_rate_change, color = scenario)) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       geom_line() +
  #       geom_hline(yintercept = 0,
  #                  linetype = "dashed",
  #                  color = "black") +
  #       xlab ("Future year") +
  #       ylab ("Change in Annual Adult Female Survival Rate") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #   })
  # })
  #
  # output$survival_grizzly_af_Plot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <- reportList()$grizzly_survival
  #
  #     # if want to look at difference:
  #     # grizzly_data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, gbpu_name)]  # replace first() with shift() to get difference with previous year value instead of first year value
  #
  #     p <-
  #       ggplot(data,
  #              aes (x = timeperiod, y = survival_rate, color = scenario)) +
  #       facet_grid (rows = vars(gbpu_name)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Adult Female Survival Rate") +
  #       theme_bw() +
  #       theme (legend.title = element_blank()) +
  #       scale_x_continuous(limits = c (input$grizzlyYear[1], input$grizzlyYear[2]))
  #
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #   })
  # })
  #
  # output$road_density_grizzly_Plot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data <- reportList()$grizzly_survival
  #
  #     # if want to look at difference:
  #     # grizzly_data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, gbpu_name)]  # replace first() with shift() to get difference with previous year value instead of first year value
  #
  #     p <-
  #       ggplot(data, aes (x = timeperiod, y = road_density, color = scenario)) +
  #       facet_grid (rows = vars(gbpu_name)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Grizzly Bear Population Unit Road Density (km/km2)") +
  #       theme_bw() +
  #       theme (legend.title = element_blank()) +
  #       scale_x_continuous(limits = c (input$grizzlyYear[1], input$grizzlyYear[2]))
  #
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #   })
  # })

  # output$propDisturbPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data1 <- reportList()$disturbance
  #     p <-
  #       ggplot(data1,
  #              aes (
  #                x = timeperiod,
  #                y = (dist_per * 100),
  #                color = scenario,
  #                linetype = scenario
  #              )) +
  #       facet_wrap (facets = vars (critical_hab)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Percent Disturbed") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 40,
  #           t = 40,
  #           pad = 0
  #         )
  #         #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #       )# change seasonal values
  #   })
  # })
  #
  # output$propDisturbBuffPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data1 <- reportList()$disturbance
  #     p <-
  #       ggplot(data1,
  #              aes (
  #                x = timeperiod,
  #                y = (dist500_per * 100),
  #                color = scenario,
  #                linetype = scenario
  #              )) +
  #       facet_wrap (facets = vars (critical_hab)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Percent Disturbed") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       scale_y_continuous(limits = c(0, 100),
  #                          breaks = seq(0, 100, by = 10)) +
  #       # scale_alpha_discrete(range=c(0.4,0.8))+
  #       # scale_color_grey(start=0.8, end=0.2) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 40,
  #           t = 40,
  #           pad = 0
  #         )
  #         #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #       )# change seasonal values
  #   })
  # })
  #
  # output$propEarlyPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data1 <- reportList()$survival
  #     p <-
  #       ggplot(data1,
  #              aes (
  #                x = timeperiod,
  #                y = prop_age,
  #                color = scenario,
  #                type = scenario
  #              )) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Proportion Age 0 to 40 years") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       # scale_alpha_discrete(range=c(0.4,0.8))+
  #       # scale_color_grey(start=0.8, end=0.2) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 40,
  #           t = 40,
  #           pad = 0
  #         )
  #         #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #       )# change seasonal values
  #   })
  # })
  #
  # output$propMaturePlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data1 <- reportList()$survival
  #     p <-
  #       ggplot(data1,
  #              aes (
  #                x = timeperiod,
  #                y = prop_mature,
  #                color = scenario,
  #                type = scenario
  #              )) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Proportion Age 80 to 120 years") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       # scale_alpha_discrete(range=c(0.4,0.8))+
  #       # scale_color_grey(start=0.8, end=0.2) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 40,
  #           t = 40,
  #           pad = 0
  #         )
  #         #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #       )# change seasonal values
  #   })
  # })
  #
  # output$propOldPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plots', value = 0.1, {
  #     data1 <- reportList()$survival
  #     p <-
  #       ggplot(data1,
  #              aes (
  #                x = timeperiod,
  #                y = prop_old,
  #                color = scenario,
  #                type = scenario
  #              )) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       geom_line() +
  #       xlab ("Future year") +
  #       ylab ("Proportion > 120 years") +
  #       scale_x_continuous(limits = c(0, 50),
  #                          breaks = seq(0, 50, by = 10)) +
  #       # scale_alpha_discrete(range=c(0.4,0.8))+
  #       # scale_color_grey(start=0.8, end=0.2) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 40,
  #           t = 40,
  #           pad = 0
  #         )
  #         #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #       )# change seasonal values
  #   })
  # })

  # output$fisherOccupancyPlot <- renderPlotly({
  #   data <-
  #     reportList()$fisher[, sum(rel_prob_occup), by = c('scenario', 'timeperiod')]
  #   p <-
  #     ggplot(data,
  #            aes (
  #              x = timeperiod,
  #              y = V1,
  #              group = scenario,
  #              color = scenario
  #            )) +
  #     geom_line() +
  #     xlab ("Future year") +
  #     ylab ("Sum relative probability of occupancy") +
  #     theme_bw() +
  #     theme (legend.title = element_blank())
  #   ggplotly(p) %>%
  #     layout (legend = list (orientation = "h", y = -0.1))
  # })
  #
  # output$fisherTerritoryPlot <- renderPlot({
  #   data <- reportList()$fisher[timeperiod == input$fisherTerritoryYear]
  #   ggplot(data, aes(rel_prob_occup, color = scenario, fill = scenario)) +
  #     facet_grid(. ~ reference_zone) +
  #     geom_density(aes(y = ..scaled..), alpha = 0.1) +
  #     xlab ("Relative probability of occupancy") +
  #     ylab ("Frequency") +
  #     theme_bw() +
  #     theme (legend.title = element_blank(), legend.position = 'bottom')
  #
  # })
  #
  # output$rsfPlot <- renderPlotly ({
  #   data <- reportList()$rsf
  #   # data$scenario <- reorder(data$scenario, data$sum_rsf_hat, function(x) -max(x) )
  #   data[, rsf_perc_change := ((first(sum_rsf_hat) - sum_rsf_hat) / first(sum_rsf_hat) * 100), by = .(scenario, rsf_model)]  # replace first() with shift() to get difference with previous year value instead of first year value
  #   p <-
  #     ggplot(data, aes (x = timeperiod, y = rsf_perc_change, fill = scenario)) +
  #     facet_grid (rows = vars(rsf_model)) +
  #     geom_bar(stat = "identity", position = "dodge") +
  #     geom_hline(yintercept = 0,
  #                linetype = "dashed",
  #                color = "black") +
  #     xlab ("Future year") +
  #     ylab ("RSF Value Percent Change") +
  #     scale_x_continuous(limits = c(0, 55), breaks = seq(0, 50, by = 10)) +
  #     theme_bw() +
  #     theme (legend.title = element_blank())
  #   ggplotly(p, height = 900)  %>%
  #     layout (
  #       legend = list (orientation = "h", y = -0.1),
  #       margin = list (
  #         l = 50,
  #         r = 40,
  #         b = 40,
  #         t = 10,
  #         pad = 0
  #       )
  #       #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
  #     )# change seasonal values
  # })
  #
  # output$fireByYearPlot <- renderPlotly ({
  #   withProgress(message = 'Making Plot', value = 0.1, {
  #     data <- reportList()$fire
  #     # data$scenario <- reorder(data$scenario, data$sum_rsf_hat, function(x) -max(x) )
  #     #print(data)
  #
  #     p <- ggplot(data, aes (x = year, y = proportion.burn)) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       geom_bar(stat = "identity", width = 1) +
  #       #geom_line(col="grey")+
  #       #geom_bar(stat="identity", width=0.7) +
  #       xlab ("Year") +
  #       ylab ("Proportion of area burned") +
  #       scale_x_continuous(limits = c(1919, 2025),
  #                          breaks = seq(1925, 2025, by = 75)) +
  #       scale_y_continuous(limits = c(0, 45),
  #                          breaks = seq(0, 45, by = 20)) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #   })
  # })
  #
  # output$firecummulativePlot <- renderPlotly ({
  #   withProgress(message = 'Making Plot', value = 0.1, {
  #     data <- reportList()$fire
  #
  #
  #     ##Calculating cummulative area burned over a 40 year moving window for each herd across each habitat type
  #     Years <- 1919:2018
  #     window_size <- 40
  #
  #     Fire_cummulative <- data.frame (matrix (ncol = 3, nrow = 0))
  #     colnames (Fire_cummulative) <-
  #       c ("herd_bounds", "cummulative.area.burned", "year")
  #
  #     for (i in 1:(length(Years) - window_size)) {
  #       fire.summary <-
  #         data %>% filter(year >= Years[i] & year <= (Years[i] + window_size)) %>%
  #         group_by (herd_bounds) %>%
  #         summarize(cummulative.area.burned = sum(proportion.burn))
  #       fire.summary$year <- Years[i] + window_size
  #
  #       Fire_cummulative <-
  #         rbind(Fire_cummulative, as.data.frame(fire.summary))
  #     }
  #     #print(Fire_cummulative)
  #
  #     p <-
  #       ggplot(Fire_cummulative,
  #              aes (x = year, y = cummulative.area.burned)) +
  #       facet_grid (rows = vars(herd_bounds)) +
  #       #geom_line (col="grey") +
  #       #geom_point()+
  #       geom_bar(stat = "identity", width = 1) +
  #       xlab ("Year") +
  #       ylab ("Cummulative proportion of area burned < 40 years") +
  #       scale_x_continuous(limits = c(1959, 2020),
  #                          breaks = seq(1960, 2020, by = 30)) +
  #       scale_y_continuous(limits = c(0, 70),
  #                          breaks = seq(0, 70, by = 20)) +
  #       theme_bw() +
  #       theme (legend.title = element_blank())
  #
  #     ggplotly(p, height = 900) %>%
  #       layout (
  #         legend = list (orientation = "h", y = -0.1),
  #         margin = list (
  #           l = 50,
  #           r = 40,
  #           b = 50,
  #           t = 40,
  #           pad = 0
  #         )
  #       )
  #   })
  # })
  #
  # output$fireTable <- DT::renderDataTable({
  #   dat <- reportList()$fire2
  #   names_col <- names(dat)
  #   dat <- dat %>%
  #     datatable(extensions = 'Buttons',
  #               options = list(
  #                 dom = 'Bfrtip',
  #                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  #               )) %>%
  #     formatStyle(names_col,  color = 'black', fontWeight = 'bold')
  #   return(dat)
  # })

  # observe({
  reportList <- reactive({

    # .. report list ----
      # browser()
    req(schema_scenarios$schema())
    req(schema_scenarios$scenario())


    if (nrow(getTableQuery(
      paste0(
        "SELECT * FROM information_schema.tables
     WHERE table_schema = '",
     schema_scenarios$schema() ,
     "' and table_name = 'caribou_abundance'"
      )
    )) > 0) {
      if (nrow(getTableQuery(
        paste0(
          "SELECT scenario, subpop_name, timeperiod,  area, core, matrix, abundance_r50, abundance_c80r50, abundance_c80, abundance_avg FROM ",
          schema_scenarios$schema(),
          ".caribou_abundance where scenario IN ('",
          paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
          "') limit 1"
        )
      )) > 0) {
        data.abundance <-
          data.table(getTableQuery(
            paste0(
              "SELECT scenario, subpop_name, timeperiod, area, core, matrix, abundance_r50, abundance_c80r50, abundance_c80, abundance_avg FROM ",
              schema_scenarios$schema(),
              ".caribou_abundance where scenario IN ('",
              paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
              "') order by scenario, subpop_name, timeperiod;"
            )
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

    if (nrow(getTableQuery(
      paste0(
        "SELECT * FROM information_schema.tables
     WHERE table_schema = '",
     schema_scenarios$schema() ,
     "' and table_name = 'survival'"
      )
    )) > 0) {
      if (nrow(getTableQuery(
        paste0(
          "SELECT * FROM ",
          schema_scenarios$schema(),
          ".survival where scenario IN ('",
          paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
          "') limit 1"
        )
      )) > 0) {
        data.survival <-
          data.table(getTableQuery(
            paste0(
              "SELECT * FROM ",
              schema_scenarios$schema(),
              ".survival where scenario IN ('",
              paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
              "') order by scenario, herd_bounds, timeperiod;"
            )
          ))
        data.survival <-
          data.survival[, lapply(.SD, weighted.mean, w = area), by = c("scenario",  "herd_bounds", "timeperiod"), .SDcols = c("prop_age",
                                                                                                                              "prop_mature",
                                                                                                                              "prop_old",
                                                                                                                              "survival_rate")]
      } else{
        data.survival <- NULL
      }
    } else{
      data.survival <- NULL
    }

         if (nrow(getTableQuery(
           paste0(
             "SELECT * FROM information_schema.tables
          WHERE table_schema = '",
          schema_scenarios$schema() ,
          "' and table_name = 'disturbance'"
           )
         )) > 0) {
           if (nrow(getTableQuery(
             paste0(
               "SELECT * FROM ",
               schema_scenarios$schema(),
               ".disturbance where scenario IN ('",
               paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
               "') limit 1"
             )
           )) > 0) {
             data.disturbance <-
               data.table (getTableQuery(
                 paste0(
                   "SELECT scenario,timeperiod,critical_hab,
       sum(c40r500) as c40r500, sum(c40r50) as c40r50, sum(total_area) as total_area FROM ",
       schema_scenarios$schema(),
       ".disturbance where scenario IN ('",
       paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
       "') group by scenario, critical_hab, timeperiod order by scenario, critical_hab, timeperiod;"
                 )
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

         if (nrow(getTableQuery(
           paste0(
             "SELECT * FROM information_schema.tables
          WHERE table_schema = '",
          schema_scenarios$schema() ,
          "' and table_name = 'grizzly_survival'"
           )
         )) > 0) {
           if (nrow(getTableQuery(
             paste0(
               "SELECT * FROM ",
               schema_scenarios$schema(),
               ".grizzly_survival where scenario IN ('",
               paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
               "') limit 1"
             )
           )) > 0) {
             data.grizzly_survival <-
               data.table(getTableQuery(
                 paste0(
                   "SELECT * FROM ",
                   schema_scenarios$schema(),
                   ".grizzly_survival where scenario IN ('",
                   paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
                   "') order by scenario, gbpu_name, timeperiod;"
                 )
               ))
             data.grizzly_survival <-
               data.grizzly_survival[, lapply(.SD, weighted.mean, w = total_area), by =
                                       c("scenario",  "gbpu_name", "timeperiod"), .SDcols = c("road_density", "survival_rate")]
           } else{
             data.grizzly_survival <- NULL
           }
         } else{
           data.grizzly_survival <- NULL
         }

         data.fire <-
           getTableQuery(paste0(
             "SELECT * FROM fire where herd_bounds IN ('",
             paste(
               unique(data.survival$herd_bounds),
               sep =  "' '",
               collapse = "', '"
             ),
             "');"
           ))
         data.fire2 <-
           getTableQuery(
             paste0(
               "SELECT herd_name, habitat,  round(cast(mean_ha2 as numeric),1) as mean,  round(cast(mean_area_percent as numeric),1) as percent,
    round(cast(max_ha2 as numeric),1) as max,  round(cast(min_ha2 as numeric),1) as min, round(cast(cummulative_area_ha2 as numeric),1) as cummulative, round(cast(cummulative_area_percent as numeric),1) as cummul_percent FROM firesummary where herd_bounds IN ('",
    paste(
      unique(data.survival$herd_bounds),
      sep =  "' '",
      collapse = "', '"
    ),
    "');"
             )
           )

         if (nrow(getTableQuery(
           paste0(
             "SELECT * FROM information_schema.tables
          WHERE table_schema = '",
          schema_scenarios$schema() ,
          "' and table_name = 'fisher'"
           )
         )) > 0) {
           if (nrow(getTableQuery(
             paste0(
               "SELECT * FROM ",
               schema_scenarios$schema(),
               ".fisher where scenario IN ('",
               paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
               "') limit 1"
             )
           )) > 0) {
             data.fisherOccupancy <-
               data.table(getTableQuery(
                 paste0(
                   "SELECT rel_prob_occup, zone, reference_zone, timeperiod, scenario  FROM ",
                   schema_scenarios$schema(),
                   ".fisher where scenario IN ('",
                   paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"),
                   "') order by scenario, timeperiod;"
                 )
               ))
             data.fisher.hexa <-
               data.table(
                 getTableQuery(
                   "SELECT x,y, size, ogc_fid as zone, reference_zone FROM public.fisher_territory_pts "
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

      list(
        harvest = data.table(getTableQuery(paste0("SELECT * FROM ", schema_scenarios$schema(), ".harvest where scenario IN ('", paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"), "');"))),
        growingstock = data.table(getTableQuery(paste0("SELECT scenario, timeperiod, sum(m_gs) as growingstock FROM ", schema_scenarios$schema(), ".growingstock where scenario IN ('", paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"), "') group by scenario, timeperiod;"))),
        rsf = data.table(getTableQuery(paste0("SELECT * FROM ", schema_scenarios$schema(), ".rsf where scenario IN ('", paste(schema_scenarios$scenario(), sep =  "' '", collapse = "', '"), "') order by scenario, rsf_model, timeperiod;"))),
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

  observe({
    mod_page_dashboard_summary_server("page_dashboard_summary", schema_scenarios, reportList)
    mod_page_dashboard_caribou_server("page_dashboard_caribou", reportList)
  })

}
