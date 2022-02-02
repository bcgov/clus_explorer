#' page_dashboard_caribou UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_caribou_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(

      tabPanel(
        "Disturbance",

        h4("Proportion Disturbed"),
        plotlyOutput(outputId = ns("propDisturbPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),

        h4("Proportion Disturbed with 500m Buffer"),
        plotlyOutput(outputId = ns("propDisturbBuffPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),

      tabPanel(
        "Forest Age",

        h4("Early Forest"),
        plotlyOutput(outputId = ns("propEarlyPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),

        h4("Mature Forest"),
        plotlyOutput(outputId = ns("propMaturePlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),

        h4("Old Forest"),
        plotlyOutput(outputId = ns("propOldPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),

      tabPanel(
        "Population",

        h4("Abundance (Southern Group of Southern Mountain Caribou Only)"),
        plotlyOutput(outputId = ns("abundancePlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),
        shiny::tags$small(
          '"Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest."'
        ),

        h4("Survival (Southern Group of Southern Mountain Caribou Only)"),
        plotlyOutput(outputId = ns("survivalPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),

      tabPanel(
        "Resource Selection",

        h4("Resource Selection"),
        plotlyOutput(outputId = ns("rsfPlot"), height = "600px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      )
    )
  )
}

#' page_dashboard_caribou Server Functions
#'
#' @noRd
mod_page_dashboard_caribou_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$propDisturbPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data1 <- reportList()$disturbance
        p <-
          ggplot(data1,
                 aes (
                   x = timeperiod,
                   y = (dist_per * 100),
                   color = scenario,
                   linetype = scenario
                 )) +
          facet_wrap (facets = vars (critical_hab)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Percent Disturbed") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          theme_bw() +
          theme (legend.title = element_blank())

        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 40,
              t = 40,
              pad = 0
            )
            #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
          )# change seasonal values
      })
    })

    output$propDisturbBuffPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data1 <- reportList()$disturbance
        p <-
          ggplot(data1,
                 aes (
                   x = timeperiod,
                   y = (dist500_per * 100),
                   color = scenario,
                   linetype = scenario
                 )) +
          facet_wrap (facets = vars (critical_hab)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Percent Disturbed") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          scale_y_continuous(limits = c(0, 100),
                             breaks = seq(0, 100, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 40,
              t = 40,
              pad = 0
            )
            #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
          )# change seasonal values
      })
    })

    output$propEarlyPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data1 <- reportList()$survival
        p <-
          ggplot(data1,
                 aes (
                   x = timeperiod,
                   y = prop_age,
                   color = scenario,
                   type = scenario
                 )) +
          facet_wrap (facets = vars (herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion Age 0 to 40 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 40,
              t = 40,
              pad = 0
            )
            #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
          )# change seasonal values
      })
    })

    output$propMaturePlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data1 <- reportList()$survival
        p <-
          ggplot(data1,
                 aes (
                   x = timeperiod,
                   y = prop_mature,
                   color = scenario,
                   type = scenario
                 )) +
          facet_wrap (facets = vars (herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion Age 80 to 120 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 40,
              t = 40,
              pad = 0
            )
            #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
          )# change seasonal values
      })
    })

    output$propOldPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data1 <- reportList()$survival
        p <-
          ggplot(data1,
                 aes (
                   x = timeperiod,
                   y = prop_old,
                   color = scenario,
                   type = scenario
                 )) +
          facet_wrap (facets = vars (herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion > 120 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 40,
              t = 40,
              pad = 0
            )
            #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
          )# change seasonal values
      })
    })

    output$abundancePlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data <- reportList()$abundance

        p <-
          ggplot(data,
                 aes (x = timeperiod, y = abundance_avg, color = scenario)) +
          facet_wrap (facets = vars (subpop_name)) +
          geom_line () +
          xlab ("Future year") +
          ylab ("Abundance") +
          scale_x_continuous (limits = c(0, 50),
                              breaks = seq (0, 50, by = 10)) +
          theme_bw () +
          theme (legend.title = element_blank(),
                 plot.caption = element_text (hjust = 0))
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 50,
              t = 40,
              pad = 0
            )
          )

      })
    })

    output$survivalPlot <- renderPlotly ({
      withProgress(message = 'Making Plots', value = 0.1, {
        data <- reportList()$survival
        data[, survival_rate_change := survival_rate - first(survival_rate), by = .(scenario, herd_bounds)]  # replace first() with shift() to get difference with previous year value instead of first year value

        p <-
          ggplot(data,
                 aes (x = timeperiod, y = survival_rate_change, color = scenario)) +
          facet_wrap (facets = vars (herd_bounds)) +
          geom_line() +
          geom_hline(yintercept = 0,
                     linetype = "dashed",
                     color = "black") +
          xlab ("Future year") +
          ylab ("Change in Annual Adult Female Survival Rate") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 600) %>%
          plotly::layout (
            legend = list (orientation = "h", y = -0.1),
            margin = list (
              l = 50,
              r = 40,
              b = 50,
              t = 40,
              pad = 0
            )
          )
      })
    })

    output$rsfPlot <- renderPlotly ({
      # browser()
      data <- as.data.table(reportList()$rsf)

      if (nrow(data)) {

      # data$scenario <- reorder(data$scenario, data$sum_rsf_hat, function(x) -max(x) )
      data[, rsf_perc_change := ((first(sum_rsf_hat) - sum_rsf_hat) / first(sum_rsf_hat) * 100), by = .(scenario, rsf_model)]  # replace first() with shift() to get difference with previous year value instead of first year value
      p <-
        ggplot(data, aes (x = timeperiod, y = rsf_perc_change, fill = scenario)) +
        facet_grid (rows = vars(rsf_model)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_hline(yintercept = 0,
                   linetype = "dashed",
                   color = "black") +
        xlab ("Future year") +
        ylab ("RSF Value Percent Change") +
        scale_x_continuous(limits = c(0, 55), breaks = seq(0, 50, by = 10)) +
        theme_bw() +
        theme (legend.title = element_blank())
      ggplotly(p, height = 600)  %>%
        plotly::layout (
          legend = list (orientation = "h", y = -0.1),
          margin = list (
            l = 50,
            r = 40,
            b = 40,
            t = 10,
            pad = 0
          )
          #yaxis = list (title=paste0(c(rep("&nbsp;", 10),"RSF Value Percent Change", rep("&nbsp;", 200), rep("&nbsp;", 3))
        )# change seasonal values
      }
    })


  })


}
