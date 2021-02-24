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
r.dir <- here::here("R")

source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V3.0.2.R")
source(file.path(r.dir,"run_decisiontool.R"))

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
                  textInput("filename", label = "Enter new scenario name:", value = NULL)
                  ),
                box(
                  selectInput("existing_scenarios",
                              "Choose existing scenario:",
                              selected = "",
                              c("",existing_input_scenarios),
                              multiple = F),
                  actionButton("update_list",
                  "Refresh")
                  )
                ),
              fluidRow(
                box(
                  
                  rHandsontableOutput("hot", width = "100%"),
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
                # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
                tabPanel("Tables", 
                         tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                     src="test1_Tables.pdf")),
                tabPanel("Gear Redistribution Figures", 
                         tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                     src="test1_GearRedistributionFigures.pdf")),
                tabPanel("Scenario Figures", 
                         tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                     src="test1_ScenarioFigures.pdf")),
                tabPanel("Threat DistributionsScenario Figures", 
                         tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                     src="test1_ScenarioFigures.pdf"))
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