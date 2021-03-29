library(shinydashboard)
library(rhandsontable)
library(leaflet)
library(dplyr)

#Source helper functions
r.dir <- here::here("R")

source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V3.0.7.R")
source(file.path(r.dir,"run_decisiontool.R"))

#A zoom effect for boxes
setZoom <- shinyEffects::setZoom
existing_outputs <- list.files(here::here("Scenarios"))

#User interface

ui <- dashboardPage(
  dashboardHeader(title = "DST Scenario Planning", titleWidth = 300),
  dashboardSidebar(    
    sidebarMenu(
    menuItem("Specify Model", tabName = "specify_model", icon = icon("dashboard")),
    menuItem("View Result Tables & Figures", tabName = "view_output", icon = icon("chart-area")),
    menuItem("Visualize Areas", tabName = "visualize_areas", icon = icon("binoculars")),
    menuItem("Additional Info", tabName = "view_help", icon = icon("question"))
    )
  ),
  dashboardBody(    
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "specify_model",
              h4("Specify scenarios and scenario parameters"),
              fluidRow(
                box(width=4,
                    selectInput("existing_scenarios",
                                "Choose existing scenario:",
                                selected = "",
                                c("",existing_input_scenarios),
                                multiple = F),
                    actionButton("update_list",
                                 "Refresh")),
                box(width=4,
                    selectInput("maprefname",
                                "Select Fishery:",
                                selected = "",
                                c("","Gillnet or Other Trap/Pot","Lobster")),
                    uiOutput("gearmap.ui"),
                    selectInput("whalemapname",
                                "Select Whale Habitat Model:",
                                selected = "",
                                c("","Duke_HumpbackWhaleModel_v10_DSTv3.Rdata","Duke_HumpbackWhaleModel_v10_DSTv3_Expanded.Rdata",
                                  "Duke_RightWhaleModel_v11_0309.Rdata","Duke_RightWhaleModel_v11_0318.Rdata",
                                  "Duke_RightWhaleModel_v11_1018.Rdata","Duke_FinWhaleModel_v11.Rdata")),
                   textInput("filename", label = "Enter new scenario name:", value = NULL)),
                box(width=4,
                    textInput("comment", label = "Add scenario comments:", placeholder = "Enter text..."),
                    checkboxInput("testscenario", label = "Run a test scenario", value = TRUE),
                    checkboxInput("coOccur", label = "Run co-occurrence only", value = FALSE),
                    checkboxInput("highres", label = "Run in high resolution", value = FALSE))
               ),
              fluidRow(
                box(
                  helpText('Parameterize actions by entering information into the spreadsheet above.
                           Right click and select "Insert Row Above" to incorporate multiple actions into the
                           scenario.'),
                  rHandsontableOutput("hot", width = "100%"),
                  br(),
                  actionButton(inputId="run",label="Run model"),
                  width = 12
                  )
                ),
              fluidRow(
                shinyjs::useShinyjs(),
                  textOutput("run-text")
                )
              ),
      tabItem(tabName = "view_output",
              #textOutput("table_scenario_name"),
              selectInput("run_scenarios",
                          "Choose scenario:",
                          selected = "",
                          c("",existing_outputs),
                          multiple = F),
              tabsetPanel(type='tab',
                # using iframe along with tags() in server to display pdf with scroll, height and width could be adjusted
                tabPanel("Tables",
                         uiOutput("pdfTables")),
                tabPanel("Gear Redistribution Figures",
                         uiOutput("pdfGearRedFigs")),
                tabPanel("Default Figures",
                         uiOutput("pdfDefaultFigs")),
                tabPanel("Scenario Figures",
                         uiOutput("pdfScenarioFigs")),
                tabPanel("Threat Distributions",
                         uiOutput("pdfThreatDist"))
              )),
      
      tabItem(tabName = "visualize_areas",
              fluidPage(
              shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', leafletOutput('help_map',width="100%",height="80vh"))
       )
      ),
      tabItem(tabName = "view_help",
              htmlOutput("renderedReadme"))
    )
  )
)