#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#' @import shiny
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
app_ui <- function(request) {

  add_class <- function(x, class) {
    x$attribs <- append(x$attribs, list(class = class))
    x
  }

  availStudyAreas<-unlist(getTableQuery("Select nspname from pg_catalog.pg_namespace WHERE nspname NOT IN ('pg_toast', 'pg_temp_1', 'pg_toast_temp_1', 'pg_catalog','information_schema', 'topology', 'public')"), use.names = FALSE)

  valueBoxSpark <- function(value = NULL, title = NULL, sparkobj = NULL, subtitle =NULL, info = NULL,
                            icon = NULL, color = "aqua", width = 4, href = NULL){

    shinydashboard:::validateColor(color)

    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")

    info_icon <- tags$small(
      tags$i(
        class = "fa fa-info-circle fa-lg",
        title = info,
        `data-toggle` = "tooltip",
        style = "color: rgba(255, 255, 255, 0.75);"
      ),
      # bs3 pull-right
      # bs4 float-right
      class = "pull-right float-right"
    )

    boxContent <- div(
      class = paste0("small-box bg-", color),
      div(
        class = "inner",
        tags$small(title),
        if (!is.null(sparkobj)) info_icon,
        h3(value),
        if (!is.null(sparkobj)) sparkobj,
        p(subtitle)
      ),
      # bs3 icon-large
      # bs4 icon
      if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )

    if (!is.null(href))
      boxContent <- a(href = href, boxContent)

    div(
      class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
    )
  }

  title <- tags$span(
    tags$img(src = 'www/img/gov3_bc_logo.png'),
    'CLUS: Explorer Tool'
  )
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(skin = "black",

                  dashboardHeader(title = title),
                  dashboardSidebar(
                    tags$style("@import url(https://use.fontawesome.com/releases/v5.15.3/css/all.css);"),
                    introjsUI(),
                    sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("home")),
                      add_class(menuItem("Scenarios", tabName = "settings", icon = icon("gears")), "settings" ),
                      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                               menuSubItem("Summary", tabName = "summary", icon = icon("balance-scale")),
                               menuSubItem("Caribou", tabName = "caribou", icon = icon("paw")),
                               menuSubItem("Climate", tabName = "climate", icon = icon("thermometer-half")),
                               menuSubItem("Fire", tabName = "fire", icon = icon("fire")),
                               menuSubItem("Fisher", tabName = "fisher", icon = icon("otter",lib = "font-awesome")),
                               menuSubItem("Forestry", tabName = "forestry", icon = icon("tree")),
                               menuSubItem("Grizzly Bear", tabName = "grizzly_bear", icon = icon("leaf")),
                               menuSubItem("Insects", tabName = "insects", icon = icon("bug")),
                               menuSubItem("Mining", tabName = "mining", icon = icon("gem")),
                               menuSubItem("Oil and Gas", tabName = "oilandgas", icon = icon("bolt")),
                               menuSubItem("Recreation", tabName = "recreation", icon = icon("shoe-prints"))
                      ),
                      add_class(menuItem("Query Builder", tabName = "querybuilder", icon = icon("search")), "querybuilder"),
                      add_class(menuItem("Map Viewer", tabName = "mapviewer", icon = icon("layer-group")), "mapviewer")
                    )
                  ),
                  dashboardBody(
                    tags$head(tags$style(
                      HTML('.small-box.bg-blue {background-color: rgba(192,192,192,0.2) !important; color: #000000 !important; } .small_icon_test { font-size: 50px; } .info-box {min-height: 75px;} .info-box-icon {height: 75px; line-height: 75px;} .info-box-content {padding-top: 0px; padding-bottom: 0px; font-size: 110%;}
                           #fisher_map_control {background-color: rgba(192,192,192,0.2);}'))),

                    tabItems(

                      tabItem(tabName = "home",
                              box(title="Welcome to the CLUS Explorer App", width =12,
                                  fluidRow(
                                    column(width = 10,
                                           p("This app was designed to interactively compare outputs from the caribou and landuse simulator (CLUS) model. Outputs are formely organized by scenario; represnting a plausible future projection of the landscape.")),
                                    column(width = 2, align = "center",
                                           img(src="clus-logo.png", width =100))
                                  )
                              ),
                              fluidRow(
                                column(
                                  12,
                                  actionButton("help", "Take a tour")
                                  , align = "center"
                                  , style = "margin-bottom: 10px;"
                                  , style = "margin-top: -10px;"
                                ),
                                bsTooltip("help", "Press for instructions",
                                          "right", options = list(container = "body"))
                              )
                      ),
                      tabItem(tabName = "settings",
                              sidebarLayout(
                                sidebarPanel( width = 6,
                                              fluidRow(

                                                column(width =12,
                                                       box(title = "Area of interest", width = 12, background = "black", solidHeader = TRUE,

                                                           selectInput(inputId = "schema", label = NULL,
                                                                       selected = "" ,
                                                                       choices = c("",availStudyAreas), selectize=FALSE
                                                           ),
                                                           bsTooltip("schema", "Select an area of interest",
                                                                     "right", options = list(container = "body")
                                                           )
                                                       )
                                                )
                                              ),
                                              fluidRow(
                                                column(width =12,
                                                       box(title = "Scenarios", width = 12, background = "black", solidHeader = TRUE,
                                                           checkboxGroupInput(inputId ="scenario", label = NULL, selected = NULL, choiceNames = NULL ),
                                                           bsTooltip("scenario", "Select the scenarios you wish to compare. See Dashboard for indicators.",
                                                                     "right", options = list(container = "body"))
                                                           #tableOutput("scenarioDescription")
                                                       )
                                                       # checkboxGroupTooltip(id = "scenario", placement = "right", trigger = "hover"),
                                                )
                                              )
                                )
                                ,
                                mainPanel( width = 6,
                                           conditionalPanel(condition = "!input.schema == ''",
                                                            box(title = "Current State", width = 12, background = "black", solidHeader = FALSE,
                                                                fluidRow(
                                                                  box(title = "Landbase", solidHeader = TRUE,background = "green", width =12,
                                                                      fluidRow(width = 12,
                                                                               column(width = 4,
                                                                                      fluidRow(width = 12,infoBoxOutput("statusTHLB")),
                                                                                      fluidRow(width = 12,infoBoxOutput("statusRoad")),
                                                                                      fluidRow(width = 12,infoBoxOutput("statusAvgVol")),
                                                                                      bsTooltip("statusAvgVol", "Average volume (m3) per ha in THLB ",
                                                                                                "top", options = list(container = "body")),
                                                                                      bsTooltip("statusTHLB", "Percentage of timber harvesting landbase in the area of interest",
                                                                                                "top", options = list(container = "body")),
                                                                                      bsTooltip("statusRoad", "Percentage of the area of interest within 100m of a road ",
                                                                                                "top", options = list(container = "body"))
                                                                               ),
                                                                               column(width = 8,
                                                                                      plotlyOutput(outputId = "statusPlot", height = "250px"),
                                                                                      bsTooltip("statusPlot", "Proportion of seral as early (<40 yrs), mature (60 - 120 yrs) and old (> 120 yrs).",
                                                                                                "top", options = list(container = "body"))
                                                                               )
                                                                      )
                                                                  )
                                                                ),
                                                                fluidRow(

                                                                ),
                                                                fluidRow(
                                                                  column(12,
                                                                         tags$h4("Timber Supply Area(s):"),
                                                                         selectInput("tsa_selected", choices = NULL, label = '', width = '100%',
                                                                                     multiple = T),
                                                                         bsTooltip("tsa_selected", "Select timber supply area(s).",
                                                                                   "bottom", options = list(container = "body"))
                                                                  ),
                                                                  column(12,
                                                                         tags$h4("Scenario Description"),
                                                                         textOutput("scenario_description")),
                                                                  bsTooltip("scenario_description", "Description of the last scenario selected",
                                                                            "bottom", options = list(container = "body"))
                                                                )
                                                            )#end of current state box
                                           )
                                )
                              )
                      ),
                      tabItem(tabName = "querybuilder",
                              fluidRow( #Table query
                                box(title = "Table Query", background = "black", solidHeader = TRUE,
                                    #em("SELECT",style="color:#090909;font-size:80%")
                                    selectInput(inputId = "queryTable", label = "FROM TABLE:", choices = c("scenarios", "harvest", "rsf",  "growingstock", "survival" , "yielduncertainty"), selected = character(0)),
                                    fluidRow(
                                      column(width = 6, textInput(inputId= "queryWHERE", label = "Where")),
                                      column(width = 6, selectInput(inputId= "queryColumns", label = "Columns", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(width = 6, selectInput(inputId= "queryRows", label = "Rows", choices = NULL)),
                                      column(width = 6, selectInput(inputId= "queryValue", label = "Values", choices =c("SUM", "AVG", "COUNT"), selected = character(0)))
                                    )
                                ),
                                dataTableOutput("resultSetTable")
                              )
                      ),
                      tabItem(tabName = "mapviewer",
                              fluidRow(#Raster query
                                box(title = "Map Query",  collapsible = T, background = "black", solidHeader = TRUE, width = 10,
                                    selectInput("maplayers", label = "Available Layers", multiple = FALSE, choices = NULL),
                                    actionButton("getMapLayersButton", "Load")
                                )
                              ),
                              leafletOutput("resultSetRaster", height = 750, width = "83%")

                      ),
                      tabItem(tabName = "summary",
                              fluidRow(#Raster query
                                plotlyOutput(outputId = "radar", height = "400px")
                              )
                      ),
                      tabItem(tabName = "caribou",
                              fluidRow(
                                box(title = "Proportion Disturbed", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "propDisturbPlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Proportion Disturbed with 500m Buffer", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "propDisturbBuffPlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Proportion Early", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "propEarlyPlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Proportion Mature", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "propMaturePlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Proportion Old", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "propOldPlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Abundance (Southern Group of Southern Mountain Caribou Only)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "abundancePlot", height = "900px"),
                                    "Use abundance estimates with caution. The estimates assume the entire herd is in the area of interest, or forestry development is similar outside the area of interest.")
                              ),
                              fluidRow(
                                box(title = "Survival (Southern Group of Southern Mountain Caribou Only)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "survivalPlot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Resource Selection", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "rsfPlot", height = "900px"))
                              )
                      ),
                      tabItem(tabName = "forestry",
                              fluidRow(
                                box(title = "Harvest Flow", collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "green", width =12,
                                    plotlyOutput(outputId = "harvestAreaPlot", height = "400px"),
                                    plotlyOutput(outputId = "harvestVolumePlot", height = "400px")
                                )
                              ),
                              fluidRow(
                                box(title = "Transition Harvest", collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "green", width =12,
                                    plotlyOutput(outputId = "managedAreaPlot", height = "400px"),
                                    plotlyOutput(outputId = "managedVolumePlot", height = "400px")
                                )
                              ),
                              fluidRow(
                                box(title = "Harvest Age", collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "green", width =12,
                                    plotlyOutput(outputId = "harvestAgePlot", height = "400px")
                                )
                              ),
                              fluidRow(
                                box(title = "Available THLB", collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "green", width =12,
                                    plotlyOutput(outputId = "availableTHLBPlot", height = "400px")
                                )
                              ),
                              fluidRow(
                                box(title = "Growingstock", collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "green", width =12,
                                    plotlyOutput(outputId = "growingStockPlot", height = "400px")
                                )
                              )
                      ),
                      tabItem(tabName = "fire",
                              fluidRow(
                                box(title="Summary of area burned",collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "red", width =12   ,
                                    dataTableOutput("fireTable")
                                )
                              ),
                              fluidRow(
                                box(title="Fire history 1919 - 2018",collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "red", width =12,
                                    plotlyOutput(outputId = "fireByYearPlot", height = "900px")
                                )
                              ),
                              fluidRow(
                                box(title="40 year cummulative area burned",collapsible = TRUE,  collapsed = TRUE, solidHeader = TRUE,background = "red", width =12,
                                    plotlyOutput(outputId = "firecummulativePlot", height = "900px")
                                )
                              )
                      ),
                      tabItem(tabName = "fisher",
                              fluidRow(
                                box(title = "Occupancy", collapsible = FALSE,  collapsed = FALSE, solidHeader = TRUE,background = "purple", width =6,
                                    plotlyOutput(outputId = "fisherOccupancyPlot", height = "300px")
                                ),
                                box(title = "Territory", collapsible = FALSE,  collapsed = FALSE, solidHeader = TRUE,background = "purple", width =6,
                                    tags$style(" .irs-bar, .irs-bar-edge, .irs-single, .irs {max-height: 50px;}, .irs-grid-pol { background:blue; border-color: blue;}"),
                                    sliderInput("fisherTerritoryYear", "Year", 0, 200, value = 0, step = 5, animate = TRUE),
                                    plotOutput(outputId = "fisherTerritoryPlot", height = "200px")
                                )
                              ),
                              fluidRow(
                                leafletOutput("fishermapper", height = 500, width = "100%"),
                                absolutePanel(id = "fisher_map_control", class = "panel panel-default",  top = 570, left = 245, fixed = FALSE, width = "15%", height = "30%",
                                              selectInput("fisher_scenario_selected", choices = NULL, label = 'Scenario', width = '100%', multiple = F),
                                              bsTooltip("fisher_scenario_selected", "Select a scenario to map.", "right"),
                                              tags$style(" .irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol { background:black; border-color: black;}"),
                                              sliderInput("fisheryear", "Year", 0, 200,value = 0, step = 5, animate = TRUE),
                                              valueBoxOutput("numberFisherTerritory", width = 12),
                                              bsTooltip("numberFisherTerritory", "Number of fisher territories with relative probability of occupancy > 0.55", "bottom")
                                )
                              )
                      ),
                      tabItem(tabName = "grizzly_bear",
                              tags$div("Click on the boxes below to obtain information on estimated road density and adult female  survival. Esitmates are provided for grizzly bear population units (GBPUs). Survival estimates are calculated by adapting a model developed by ",
                                       tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0115535", "Boulanger and Stenhouse (2014).")
                              ),
                              br(), # line break
                              fluidRow(
                                box(title = "Adult Female Survival", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    sliderInput("grizzlyYear", label = "Enter Year Range to Plot",
                                                0, 200, value = c (0, 50), step = 5),
                                    plotlyOutput(outputId = "survival_grizzly_af_Plot", height = "900px"))
                              ),
                              fluidRow(
                                box(title = "Grizzly Bear Population Unit Road Density", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, background = "purple", width =12,
                                    plotlyOutput(outputId = "road_density_grizzly_Plot", height = "900px"))
                              )
                      ),
                      tabItem(tabName = "insects",
                              fluidRow(
                                valueBox("0", "Outbreaks", icon = icon("bug"), color = "teal"),
                                valueBox("0", "Area spread", icon = icon("bug"), color = "teal"),
                                valueBox("0", "Volume Lost", icon = icon("bug"), color = "teal")
                              )
                      ),
                      tabItem(tabName = "climate",
                              box(title = "Conceptual Path Diagram", width=12, background = "yellow", solidHeader = TRUE, collapsible = TRUE,
                                  plotOutput(outputId = "climatemap", height = "800px")
                              ),
                              box(title = "Modelled Path Diagram", width=12, background = "yellow", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE
                              )
                      ),
                      tabItem(tabName = "oilandgas",
                              fluidRow(
                                valueBox("BAU", "Footprint", icon = icon("bolt"), color = "navy"),
                                valueBox("0", "Jobs", icon = icon("bolt"), color = "navy"),
                                valueBox("0", "GDP", icon = icon("bolt"), color = "navy")
                              )
                      ),
                      tabItem(tabName = "mining",
                              fluidRow(
                                valueBox("BAU", "Footprint", icon = icon("industry"), color = "navy"),
                                valueBox("0", "Jobs", icon = icon("industry"), color = "navy"),
                                valueBox("0", "GDP", icon = icon("industry"), color = "navy")
                              )
                      ),
                      tabItem(tabName = "recreation",
                              fluidRow(
                                valueBox("BAU", "Footprint", icon = icon("shoe-prints"), color = "aqua"),
                                valueBox("0", "Jobs", icon = icon("shoe-prints"), color = "aqua"),
                                valueBox("0", "GDP", icon = icon("shoe-prints"), color = "aqua")
                              )
                      )
                    )
                      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import golem
#' @noRd
golem_add_external_resources <- function(){

  golem::add_resource_path(
    'www', app_sys('app/www')
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'clus_explorer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
