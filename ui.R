library(shinydashboard)
library(htmlwidgets)
library(rhandsontable)
library(rgdal)
library(sp)
library(maps)
library(maptools)
library(grid)
library(gtable)
library(gridExtra)
library(shinyjs)
library(leaflet)
library(imager)
library(shinyEffects)
library(stringr)
library(dplyr)

#Source helper functions
r.dir <- "R"

source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
message("model specs: done")
source("function_DecisionSupportTool_V3.0.2.R")
message("DST: done")
source(file.path(r.dir,"run_decisiontool.R"))
message("run_dst: done")

#A zoom effect for boxes
setZoom <- shinyEffects::setZoom

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
                box(
                  selectInput("existing_scenarios",
                              "Choose existing scenario:",
                              selected = "",
                              c("",existing_input_scenarios),
                              multiple = F),
                  actionButton("update_list",
                               "Refresh")
                ),
                box(
                  selectInput("gearmapname",
                              "Select Gear Map:",
                              selected = "",
                              c("","GearMap_Lobster_V3.0.0.Rdata","GearMap_Lobster_MassRMA_V3.0.0.Rdata")),
                  selectInput("whalemapname",
                              "Select Whale Habitat Model:",
                              selected = "",
                              c("","Duke_HumpbackWhaleModel_v10_DSTv3.Rdata","Duke_HumpbackWhaleModel_v10_DSTv3_Expanded.Rdata",
                                "Duke_RightWhaleModel_v10_0309.Rdata","Duke_RightWhaleModel_v10_0318.Rdata",
                                "Duke_RightWhaleModel_v10_1018.Rdata","Duke_FinWhaleModel_v11.Rdata")),
                  textInput("filename", label = "Enter new scenario name:", value = NULL)
                  
                )),
              fluidRow(
                box(
                  rHandsontableOutput("hot", width = "100%"),
                  br(),
                  actionButton(inputId="run",label="Run model"),
                  # actionButton('cancel', 'Cancel'),
                  # actionButton('status', 'Check Status'),
                  helpText('Parameterize actions by entering information into the spreadsheet above.
                           Right click and select "Insert Row Above" to incorporate multiple actions into the
                           scenario.'),
                  width = 12
                  )
                ),
              fluidRow(
                shinyjs::useShinyjs(),
                  textOutput("run-text")
                )
              ),
      tabItem(tabName = "view_output",
              textOutput("table_scenario_name"),
              tabsetPanel(type='tab',
                # using iframe along with tags() in server to display pdf with scroll, height and width could be adjusted
                tabPanel("Tables",
                         uiOutput("pdfTables")),
                tabPanel("Gear Redistribution Figures",
                         uiOutput("pdfGearRedFigs")),
                tabPanel("Scenario Figures",
                         uiOutput("pdfScenFigs")),
                tabPanel("Threat Distributions",
                         uiOutput("pdfThreatDist"))
              )),
      
      tabItem(tabName = "visualize_areas",
              fluidPage(
              shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', leafletOutput('help_map',width="100%",height="80vh")),
              absolutePanel(top = 60, left = 320,

          #                   sliderInput("range", "Magnitudes", 1,10,
          #                               value = range(1:10), step = 0.1),
          #                   checkboxGroupInput(inputId='shapefiles',label="Display Options",c("100f"="iso100ft","EastCoast"="EastCoastLines","GB"="GB","GOM"="GOM"),
          #                                      selected = c("iso100f","EastCoast"),inline = T)                  
          # )
          h3("Display options"),
          checkboxInput(inputId='shapefile8a',label="A1",value = F),
          checkboxInput(inputId='shapefile8b',label="A2",value = F),
          checkboxInput(inputId='shapefile8c',label="A3",value = F),
          checkboxInput(inputId='shapefile8d',label="A2_3_overlap",value = F),
          checkboxInput(inputId='shapefile1',label="SouthShoreA",value = F),
          checkboxInput(inputId='shapefile2',label="SouthShoreB",value = F),
          checkboxInput(inputId='shapefile5',label="SouthShoreC",value = F),
          checkboxInput(inputId='shapefile3',label="GB",value = F),
          checkboxInput(inputId='shapefile4',label="GOM",value = F),
          checkboxInput(inputId='shapefile6',label="CCBay",value = F),
          checkboxInput(inputId='shapefile7',label="MassExpansion",value = F),
          checkboxInput(inputId='shapefile9',label="MASS_RA",value = F),
          checkboxInput(inputId='shapefile10',label="MASS_RANE",value = F),
          checkboxInput(inputId='shapefile11',label="NEA_NR",value = F),
          checkboxInput(inputId='shapefile12',label="NEA_WGOM",value = F),
          checkboxInput(inputId='shapefile13',label="StatAreas",value = F),
          checkboxInput(inputId='shapefile14',label="OffshoreA",value = F)
          #checkboxInput(inputId='shapefile15',label="TinyWedge",value = F)
          #checkboxInput(inputId='shapefile14',label="StatAreas",value = F)
        ) 
       )
      ),
      tabItem(tabName = "view_help",
              htmlOutput("renderedReadme"))
    )
  )
)