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
        plotlyOutput(outputId = ns("propDisturbPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Disturbance (500m Buffer)",
        plotlyOutput(outputId = ns("propDisturbBuffPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Seral (Early)",
        plotlyOutput(outputId = ns("propEarlyPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Seral (Mature)",
        plotlyOutput(outputId = ns("propMaturePlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Seral (Old)",
        plotlyOutput(outputId = ns("propOldPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Abundance",
        plotlyOutput(outputId = ns("abundancePlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff'),
        shiny::tags$small(
          '"Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest."'
        )
      ),
      tabPanel(
        "Survival",
        plotlyOutput(outputId = ns("survivalPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      ),
      tabPanel(
        "Resource Selection",
        plotlyOutput(outputId = ns("rsfPlot"), height = "900px") %>%
          withSpinner(color.background = '#ecf0f5', color = '#ffffff')
      )
    )#,
    # fluidRow(
    #   box(
    #     title = "Proportion Disturbed",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("propDisturbPlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Proportion Disturbed with 500m Buffer",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("propDisturbBuffPlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Proportion Early",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("propEarlyPlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Proportion Mature",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("propMaturePlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Proportion Old",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("propOldPlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Abundance (Southern Group of Southern Mountain Caribou Only)",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("abundancePlot"), height = "900px"),
    #     "Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest."
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Survival (Southern Group of Southern Mountain Caribou Only)",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("survivalPlot"), height = "900px")
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Resource Selection",
    #     collapsible = TRUE,
    #     collapsed = TRUE,
    #     solidHeader = TRUE,
    #     # background = "purple",
    #     width = 12,
    #     plotlyOutput(outputId = ns("rsfPlot"), height = "900px")
    #   )
    # )
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
        ggplotly(p, height = 900) %>%
          layout (
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
        ggplotly(p, height = 900) %>%
          layout (
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
          facet_grid (rows = vars(herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion Age 0 to 40 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 900) %>%
          layout (
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
          facet_grid (rows = vars(herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion Age 80 to 120 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 900) %>%
          layout (
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
          facet_grid (rows = vars(herd_bounds)) +
          geom_line() +
          xlab ("Future year") +
          ylab ("Proportion > 120 years") +
          scale_x_continuous(limits = c(0, 50),
                             breaks = seq(0, 50, by = 10)) +
          # scale_alpha_discrete(range=c(0.4,0.8))+
          # scale_color_grey(start=0.8, end=0.2) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 900) %>%
          layout (
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

  })
}
