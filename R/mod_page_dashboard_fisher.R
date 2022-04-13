#' page_dashboard_fisher UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_fisher_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Occupancy",
        collapsible = FALSE,
        collapsed = FALSE,
        solidHeader = TRUE,
        # background = "purple",
        width = 6,
        # h4("Fire history 1919 - 2018"),
        uiOutput(ns("fisherOccupancyPlotUI"))
      ),
      box(
        title = "Territory",
        collapsible = FALSE,
        collapsed = FALSE,
        solidHeader = TRUE,
        # background = "purple",
        width = 6,
        tags$style(
          " .irs-bar, .irs-bar-edge, .irs-single, .irs {max-height: 50px;}, .irs-grid-pol { background:blue; border-color: blue;}"
        ),
        uiOutput(ns("fisherTerritoryPlotUI"))
      )
    ),
    fluidRow(
      uiOutput(ns("fisherMapUI"))
    )
  )
}

#' page_dashboard_fisher Server Functions
#'
#' @noRd
mod_page_dashboard_fisher_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (exists('reportList()$fisher')) {

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

      output$numberFisherTerritory <- renderValueBox({
        valueBoxSpark(
          value = paste0(as.integer(nrow(
            reportList()$fisher[timeperiod == input$fisheryear &
                                  scenario == input$fisher_scenario_selected &
                                  rel_prob_occup > 0.55, "zone"]
          ))),
          title = toupper("Territories"),
          subtitle = NULL,
          icon = icon("times-circle"),
          width = 4,
          color = "blue"
        )
      })

      output$fishermapper <- renderLeaflet({
        pal <- colorNumeric(palette = 'Blues',
                            domain = reportList()$fisherPts$rel_prob_occup)
        leaflet(reportList()$fisherPts) %>%
          addTiles() %>%
          fitBounds( ~ min(x), ~ min(y), ~ max(x), ~ max(y)) %>%
          addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>%
          addProviderTiles("Esri.DeLorme", group = "DeLorme") %>%
          addScaleBar(position = "bottomright") %>%
          addLayersControl(baseGroups = c("OpenStreetMap", "WorldImagery", "DeLorme")) %>%
          addCircles(
            lat = ~ y,
            lng = ~ x,
            fillColor = ~ pal(reportList()$fisherPts$rel_prob_occup),
            color =  ~ pal(reportList()$fisherPts$rel_prob_occup),
            radius = reportList()$fisherPts$size * 100,
            popup = ~ paste0(
              "ref:",
              reference_zone,
              " zone:",
              zone,
              " occupancy:",
              rel_prob_occup
            )
          ) %>%
          addLegend(
            position = "topright",
            pal = pal,
            values = ~ reportList()$fisherPts$rel_prob_occup,
            title = "Prob"
          )
      })

      if (nrow(reportList()$fisher) > 0) {
        output$fisherMapUI <- renderUI({
          leafletOutput(ns("fishermapper"), height = 500, width = "100%") %>%
            withSpinner(color = '#ecf0f5', color.background = '#ffffff')

          absolutePanel(
            id = "fisher_map_control",
            class = "panel panel-default",
            top = 570,
            left = 245,
            fixed = FALSE,
            width = "15%",
            height = "30%",
            selectInput(
              ns("fisher_scenario_selected"),
              choices = NULL,
              label = 'Scenario',
              width = '100%',
              multiple = F
            ),
            icon('info-circle') %>%
              bsplus::bs_embed_tooltip(
                "Select a scenario to map.",
                "right"
              ),
            tags$style(
              " .irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol { background:black; border-color: black;}"
            ),
            sliderInput(
              ns("fisheryear"),
              "Year",
              0,
              200,
              value = 0,
              step = 5,
              animate = TRUE
            ),
            valueBoxOutput(ns("numberFisherTerritory"), width = 12),
            icon('info-circle') %>%
              bsplus::bs_embed_tooltip(
                "Number of fisher territories with relative probability of occupancy > 0.55",
                "bottom"
              )
          )
        })

        output$fisherTerritoryPlotUI <- renderUI({
          tagList(
            sliderInput(
              ns("fisherTerritoryYear"),
              "Year",
              0,
              200,
              value = 0,
              step = 5,
              animate = TRUE
            ),
            plotOutput(outputId = ns("fisherTerritoryPlot"), height = "200px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          )
        })

        output$fisherTerritoryPlot <- renderPlot({
          data <- reportList()$fisher[timeperiod == input$fisherTerritoryYear]
          ggplot(data, aes(rel_prob_occup, color = scenario, fill = scenario)) +
            facet_grid(. ~ reference_zone) +
            geom_density(aes(y = ..scaled..), alpha = 0.1) +
            xlab ("Relative probability of occupancy") +
            ylab ("Frequency") +
            theme_bw() +
            theme (legend.title = element_blank(), legend.position = 'bottom')
        })

        output$fisherOccupancyPlotUI <- renderUI({
          tagList(
            plotlyOutput(outputId = ns("fisherOccupancyPlot"), height = "300px") %>%
              withSpinner(color = '#ecf0f5', color.background = '#ffffff')
          )
        })

        output$fisherOccupancyPlot <- renderPlotly({
          data <-
            reportList()$fisher[, sum(rel_prob_occup), by = c('scenario', 'timeperiod')]
          p <-
            ggplot(data,
                   aes (
                     x = timeperiod,
                     y = V1,
                     group = scenario,
                     color = scenario
                   )) +
            geom_line() +
            xlab ("Future year") +
            ylab ("Sum relative probability of occupancy") +
            theme_bw() +
            theme (legend.title = element_blank())
          ggplotly(p) %>%
            plotly::layout (legend = list (orientation = "h", y = -0.1))
        })
      } else {
        output$fisherTerritoryPlotUI <- renderUI({
          mod_html_alert_ui('fisherTerritoryPlotUI')
        })
        output$fisherOccupancyPlotUI <- renderUI({
          mod_html_alert_ui('fisherOccupancyPlot')
        })
        output$fisherMaptUI <- renderUI({
          mod_html_alert_ui('fisherMap')
        })
      }
    } else {
      output$fisherTerritoryPlotUI <- renderUI({
        mod_html_alert_ui('fisherTerritoryPlotUI')
      })
      output$fisherOccupancyPlotUI <- renderUI({
        mod_html_alert_ui('fisherOccupancyPlot')
      })
      output$fisherMaptUI <- renderUI({
        mod_html_alert_ui('fisherMap')
      })
    }
  })
}
