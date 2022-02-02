#' page_dashboard_fire UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_dashboard_fire_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Summary of area burned",
        dataTableOutput(ns("fireTable"))
      ),
      tabPanel(
        "Fire history 1919 - 2018",
        plotlyOutput(outputId = ns("fireByYearPlot"), height = "900px")
      ),
      tabPanel(
        "40 year cummulative area burned",
        plotlyOutput(outputId = ns("firecummulativePlot"), height = "900px")
      )
    )
  )
}

#' page_dashboard_fire Server Functions
#'
#' @noRd
mod_page_dashboard_fire_server <- function(id, reportList){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$fireByYearPlot <- renderPlotly ({
      withProgress(message = 'Making Plot', value = 0.1, {
        data <- reportList()$fire
        # data$scenario <- reorder(data$scenario, data$sum_rsf_hat, function(x) -max(x) )
        #print(data)

        p <- ggplot(data, aes (x = year, y = proportion.burn)) +
          facet_grid (rows = vars(herd_bounds)) +
          geom_bar(stat = "identity", width = 1) +
          #geom_line(col="grey")+
          #geom_bar(stat="identity", width=0.7) +
          xlab ("Year") +
          ylab ("Proportion of area burned") +
          scale_x_continuous(limits = c(1919, 2025),
                             breaks = seq(1925, 2025, by = 75)) +
          scale_y_continuous(limits = c(0, 45),
                             breaks = seq(0, 45, by = 20)) +
          theme_bw() +
          theme (legend.title = element_blank())
        ggplotly(p, height = 900) %>%
          layout (
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

    output$firecummulativePlot <- renderPlotly ({
      withProgress(message = 'Making Plot', value = 0.1, {
        data <- reportList()$fire


        ##Calculating cummulative area burned over a 40 year moving window for each herd across each habitat type
        Years <- 1919:2018
        window_size <- 40

        Fire_cummulative <- data.frame (matrix (ncol = 3, nrow = 0))
        colnames (Fire_cummulative) <-
          c ("herd_bounds", "cummulative.area.burned", "year")

        for (i in 1:(length(Years) - window_size)) {
          fire.summary <-
            data %>% filter(year >= Years[i] & year <= (Years[i] + window_size)) %>%
            group_by (herd_bounds) %>%
            summarize(cummulative.area.burned = sum(proportion.burn))
          fire.summary$year <- Years[i] + window_size

          Fire_cummulative <-
            rbind(Fire_cummulative, as.data.frame(fire.summary))
        }
        #print(Fire_cummulative)

        p <-
          ggplot(Fire_cummulative,
                 aes (x = year, y = cummulative.area.burned)) +
          facet_grid (rows = vars(herd_bounds)) +
          #geom_line (col="grey") +
          #geom_point()+
          geom_bar(stat = "identity", width = 1) +
          xlab ("Year") +
          ylab ("Cummulative proportion of area burned < 40 years") +
          scale_x_continuous(limits = c(1959, 2020),
                             breaks = seq(1960, 2020, by = 30)) +
          scale_y_continuous(limits = c(0, 70),
                             breaks = seq(0, 70, by = 20)) +
          theme_bw() +
          theme (legend.title = element_blank())

        ggplotly(p, height = 900) %>%
          layout (
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

    output$fireTable <- DT::renderDataTable({
      dat <- reportList()$fire2
      names_col <- names(dat)
      dat <- dat %>%
        datatable(extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  )) %>%
        formatStyle(names_col,  color = 'black', fontWeight = 'bold')
      return(dat)
    })

  })
}
