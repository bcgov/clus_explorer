#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
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
#' @import shinycssloaders
#' @import shinyjs
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),

    # Leave this function for adding external resources
    dashboardPage(
      skin = "black",
      dashboardHeader(
        title = tags$span(
          tags$img(src = 'www/img/gov3_bc_logo.png'),
          'CLUS Explorer Tool'
        ),
        titleWidth = 400
      ),
      dashboardSidebar(
        tags$style(
          "@import url(https://use.fontawesome.com/releases/v5.15.3/css/all.css);"
        ),
        introjsUI(),
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          add_class(
            menuItem("Scenarios", tabName = "settings", icon = icon("gears")),
            "settings"
          ),
          menuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("dashboard"),
            menuSubItem("Summary", tabName = "summary", icon = icon("balance-scale")),
            menuSubItem("Caribou", tabName = "caribou", icon = icon("paw")),
            menuSubItem("Climate", tabName = "climate", icon = icon("thermometer-half")),
            menuSubItem("Fire", tabName = "fire", icon = icon("fire")),
            menuSubItem("Fisher", tabName = "fisher", icon = icon("otter", lib = "font-awesome")),
            menuSubItem("Forestry", tabName = "forestry", icon = icon("tree")),
            menuSubItem("Grizzly Bear", tabName = "grizzly_bear", icon = icon("leaf")),
            menuSubItem("Insects", tabName = "insects", icon = icon("bug")),
            menuSubItem("Mining", tabName = "mining", icon = icon("gem")),
            menuSubItem("Oil and Gas", tabName = "oilandgas", icon = icon("bolt")),
            menuSubItem("Recreation", tabName = "recreation", icon = icon("shoe-prints"))
          ),
          add_class(
            menuItem(
              "Generate Report",
              tabName = "report",
              icon = icon("file-pdf")
            ),
            "report"
          ),
          add_class(
            menuItem(
              "Query Builder",
              tabName = "querybuilder",
              icon = icon("search")
            ),
            "querybuilder"
          ),
          add_class(
            menuItem(
              "Map Viewer",
              tabName = "mapviewer",
              icon = icon("layer-group")
            ),
            "mapviewer"
          )
        )
      ),
      dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(tags$style(
          HTML(
            '.small-box.bg-blue {background-color: rgba(192,192,192,0.2) !important; color: #000000 !important; } .small_icon_test { font-size: 50px; } .info-box {min-height: 75px;} .info-box-icon {height: 75px; line-height: 75px;} .info-box-content {padding-top: 0px; padding-bottom: 0px; font-size: 110%;}
                           #fisher_map_control {background-color: rgba(192,192,192,0.2);}'
          )
        )),
        tabItems(

          tabItem(
            tabName = "home",
            mod_page_home_ui("page_home")
          ),

          tabItem(
            tabName = "settings",
            mod_page_scenarios_ui("page_scenarios")
          ),

          tabItem(
            tabName = "summary",
            mod_page_dashboard_summary_ui("page_dashboard_summary")
          ),

          tabItem(
            tabName = "caribou",
            mod_page_dashboard_caribou_ui("page_dashboard_caribou")
          ),

          tabItem(
            tabName = "forestry",
            mod_page_dashboard_forestry_ui("page_dashboard_forestry")
          ),

          tabItem(
            tabName = "fire",
            mod_page_dashboard_fire_ui("page_dashboard_fire")
          ),

          tabItem(
            tabName = "fisher",
            mod_page_dashboard_fisher_ui("page_dashboard_fisher")
          ),

          tabItem(
            tabName = "grizzly_bear",
            mod_page_dashboard_grizzly_ui("page_dashboard_grizzly")
          ),

          tabItem(
            tabName = "insects",
            mod_page_dashboard_insects_ui("page_dashboard_insects")
          ),

          tabItem(
            tabName = "climate",
            mod_page_dashboard_climate_ui("page_dashboard_climate")
          ),

          tabItem(
            tabName = "oilandgas",
            mod_page_dashboard_oil_and_gas_ui("page_dashboard_oil_and_gas")
          ),

          tabItem(
            tabName = "mining",
            mod_page_dashboard_mining_ui("page_dashboard_mining")
          ),

          tabItem(
            tabName = "recreation",
            mod_page_dashboard_recreation_ui("page_dashboard_recreation")
          ),

          tabItem(
            tabName = "report",
            mod_page_report_ui("page_report")
          ),

          tabItem(
            tabName = "querybuilder",
            mod_page_query_builder_ui("page_querybuilder")
          ),

          tabItem(
            tabName = "mapviewer",
            mod_page_map_viewer_ui("page_mapviewer")
          )
        )
      )
    ),
    tags$footer(
      class = 'footer',
      tags$div(
        class = 'container',
        tags$ul(
          tags$li(tags$a(href = '.', 'Home')),
          tags$li(
            tags$a(href = 'https://www2.gov.bc.ca/gov/content/home/disclaimer', 'Disclaimer')
          ),
          tags$li(
            tags$a(href = 'https://www2.gov.bc.ca/gov/content/home/privacy', 'Privacy')
          ),
          tags$li(
            tags$a(href = 'https://www2.gov.bc.ca/gov/content/home/accessibility', 'Accessibility')
          ),
          tags$li(
            tags$a(href = 'https://www2.gov.bc.ca/gov/content/home/copyright', 'Copyright')
          ),
          tags$li(
            tags$a(href = 'https://github.com/bcgov/devhub-app-web/issues', 'Contact Us')
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
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(path = app_sys('app/www'),
                            app_title = 'CLUS Explorer'),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    golem::activate_js(),
    golem::favicon()
  )

}
