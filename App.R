library(shinydashboard)
library(rhandsontable)
library(leaflet)
library(dplyr)

#Source helper functions
HD="/net/shiny1/amiller/DST"
r.dir <-"/net/shiny1/amiller/DST/R"

source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V3.0.7.R")
source(file.path(r.dir,"run_decisiontool.R"))

existing_input_csvs <- list.files("/net/shiny1/amiller/DST/InputSpreadsheets")
existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")
existing_outputs <- list.files("/net/shiny1/amiller/DST/Scenarios")

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

server <- function(input, output, session) {
  ###### LEAFLET HELP MAP  
  output$help_map = renderLeaflet({
    # Makes a leaflet map to visualize management areas
    
    leaflet() %>%
      setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250)) %>%
      
      addPolygons(group = "StatAreas" ,data = StatAreas , stroke = TRUE, color = '#5a5a5a',
                  opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      addPolygons(group = "Coast-3nm" ,data = zero2three , stroke = TRUE, color = '#5a5a5a',
                  opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      addPolygons(group = "3-12nm" ,data = three2twelve , stroke = TRUE, color = '#5a5a5a',
                  opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      addPolygons(group = "12nm-EEZ" ,data = twelve2EEZ , stroke = TRUE, color = '#5a5a5a',
                  opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      # addPolygons(group = "Maine Zones" ,data = MaineA , stroke = TRUE, color = '#5a5a5a',
      #             opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      # addPolygons(group = "Lobster Management Areas" ,data = LMAs , stroke = TRUE, color = '#5a5a5a',
      #             opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>%
      addLabelOnlyMarkers(
        group = "StatAreas",data = coords,lng = ~Long,
        lat = ~Lat, label = ~as.character(StatAreas$Id),
        labelOptions = labelOptions(noHide = T, textOnly = T)) %>%
      addLayersControl(
        #overlayGroups = c(,"3-12nm","12nm-EEZ","SouthShoreAreas","StatAreas","Lobster Management Areas","Maine Zones"),
        overlayGroups = c("Coast-3nm","3-12nm","12nm-EEZ","StatAreas"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      #hideGroup(c("SouthShoreAreas","StatAreas","Coast-3nm","3-12nm","12nm-EEZ","Lobster Management Areas","Maine Zones"))
      hideGroup(c("Coast-3nm","3-12nm","12nm-EEZ","StatAreas"))
  })
  
  ##### MODEL INPUT SECTION
  observeEvent(input$update_list,{
    
    existing_input_csvs <- list.files("/net/shiny1/amiller/DST/InputSpreadsheets")
    existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")
    
    # Can also set the label and select items
    updateSelectInput(session,
                      "existing_scenarios",
                      choices = c("",existing_input_scenarios),
                      selected = "")
  })
  output$gearmap.ui <- renderUI({  #generates dynamic UI for fishery
    switch(input$maprefname,
           "Gillnet or Other Trap/Pot" = selectInput("gearmapname", "Select Gear Map:",
                                                     choices = c("","GearMap_Gillnet_IEC_V3.0.0.Rdata",
                                                                 "GearMap_OtherTrapPot_IEC_V3.0.0.Rdata","GearMap_NewEnglMultispecies_V3.0.0.Rdata",
                                                                 "GearMap_Monkfish_V3.0.0.Rdata","DST_BlackSeaBass_Federal_v3.0.0.Rdata")
           ),
           "Lobster" = selectInput("gearmapname", "Select Gear Map:",
                                   choices = c("","GearMap_Lobster_V3.0.0.Rdata","GearMap_Lobster_MassRMA_V3.0.0.Rdata")
           )
    )
  })
  
  #Specifies table layout for custom input parameters
  output$hot = renderRHandsontable({
    #Show blank template if no input file is chosen
    if (input$existing_scenarios == ""){
      
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_cols(colWidths = c(100,50)) %>% 
        hot_col(col = "Action", type = "dropdown", source = Action) %>% 
        hot_col(col = "LMA", type = "dropdown", source = LMA) %>% 
        hot_col(col = "State", type = "dropdown", source = State) %>% 
        hot_col(col = "StatArea", type = "dropdown", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "dropdown", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "dropdown", source = shapefile_names) %>% 
        hot_col(col = "Months", type = "dropdown", source = "Months") %>%
        hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
        hot_col(col = "StringRegulation", type = "dropdown", source = StringRegulation) %>% 
        hot_col(col = "StringLen", type = "dropdown", source = StringLen) %>%
        hot_col(col = "MaxRopeStrength", type = "numeric", strict = F) %>%
        hot_col(col = "BuoylineDevice", type = "dropdown", source = BuoylineDevice) %>%
        hot_col(col = "RopelessDevice", type = "dropdown", source = RopelessDevice) %>%
        hot_col(col = "GearCap", type = "numeric", strict = F) %>%
        hot_col(col = "MaxGearSnglLn", type = "numeric", strict = F) 
      
      #Show filled template if input file is chosen
    } else {
      print(paste("Selected model:",input$existing_scenarios))
        
      DF <- read.csv(paste0(HD,"/InputSpreadsheets/",input$existing_scenarios,".csv"))
      
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_cols(colWidths = c(100,50)) %>% 
        hot_col(col = "Action", type = "dropdown", source = Action) %>% 
        hot_col(col = "LMA", type = "dropdown", source = LMA) %>% 
        hot_col(col = "State", type = "dropdown", source = State) %>% 
        hot_col(col = "StatArea", type = "dropdown", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "dropdown", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "dropdown", source = shapefile_names) %>% 
        hot_col(col = "Months", type = "dropdown", source = "Months") %>%
        hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
        hot_col(col = "StringRegulation", type = "dropdown", source = StringRegulation) %>% 
        hot_col(col = "StringLen", type = "dropdown", source = StringLen) %>%
        hot_col(col = "MaxRopeStrength", type = "numeric", strict = F) %>%
        hot_col(col = "BuoylineDevice", type = "dropdown", source = BuoylineDevice) %>%
        hot_col(col = "RopelessDevice", type = "dropdown", source = RopelessDevice) %>%
        hot_col(col = "GearCap", type = "numeric", strict = F) %>%
        hot_col(col = "MaxGearSnglLn", type = "numeric", strict = F) 
    }
  })
  
  observeEvent(input$filename, {
    
    #Prevent model run if no file is chosen and no custom input
    if (is.null(input$hot)){
      shinyjs::disable("run")
      
      #Prevent model run if custom parameters exist without a scenario name
    } else if (input$filename == "" | input$gearmapname == "" | input$whalemapname == ""){
      shinyjs::disable("run")
      
      #Otherwise run the model and save the ouput to csv
    } else {
      shinyjs::enable("run")
    }
    
  })
  
  
  #Observes the "Run Model" button-------------------------------------------------------------------
  observeEvent(input$run, {
    showNotification(" Running... ",duration=NULL,id="running",type="message")
    
    #Converts table input into something shiny can use 
    
    param <- hot_to_r(input$hot) 
    param[is.na(param)] <- ""
    param$Action <- as.character(param$Action)
    param$LMA <- as.character(param$LMA)
    param$State <- as.character(param$State)
    param$StatArea <- as.character(param$StatArea)
    param$Fishery <- as.character(param$Fishery)
    param$Shapefile <- as.character(param$Shapefile)
    param$Months <- as.character(param$Months)
    param <- param %>% dplyr::filter(Action != "")
    print(param)
    
    #specify fishery from plain language
    if (input$maprefname == "Gillnet or Other Trap/Pot"){
      maprefdomain <- "MapRef_3.0.2.Rdata"  
    } else {
      maprefdomain <- "MapRef_HR_Lobster_V3.0.0.Rdata"
    }
    
    #Saves output and runs model
    print("Saving parameters to file.")
    write.csv(param, 
              file = paste0(HD,"/InputSpreadsheets/",input$filename,".csv"), na="",row.names = F) #make trap for existing files
    #Run decision tool function here. Will print messages associated w/ function in UI
    withCallingHandlers({
      shinyjs::html("run-text", "")
      tryCatch({
        Version="DecisionSupportTool_V3.0.7.R"
        HD="/net/shiny1/amiller/DST"
        source("/net/shiny1/amiller/DST/function_DecisionSupportTool_V3.0.7.R")
        
        print('About to run decision tool function.')
        run_decisiontool(HD=HD,
                         InputSpreadsheetName=paste0(input$filename,".csv"),
                         MapRefDomain=maprefdomain,
                         GearMapName=input$gearmapname,
                         WhaleMapName=input$whalemapname,
                         CommentText=input$comment,
                         TestScenario=input$testscenario,
                         CoOccurrence=input$coOccur,
                         HighResolution=input$highres)
      },
      error = function(e){
        message("Error in decision tool function.")
      }
      )
      
    },
    message = function(m) {
      shinyjs::html(id = "run-text", html = paste0(m$message,"<br>"), add = TRUE)
    })
    #creates the www directory for tags$iframe embedding pdf output files into the app
    # if (!fs::dir_exists(paste0(HD,'/www'))) fs::dir_create(paste0(HD,'/www'))
    #move pdf output files into www directory
    file.copy(paste0(HD,'/Scenarios/',input$filename,'/',input$filename,'_GearRedistributionFigures.pdf'),
              paste0(HD,'/www/',input$filename,'_GearRedistributionFigures.pdf'), overwrite = TRUE)
    file.copy(paste0(HD,'/Scenarios/',input$filename,'/',input$filename,'_Tables.pdf'),
              paste0(HD,'/www/',input$filename,'_Tables.pdf'), overwrite = TRUE)
    file.copy(paste0(HD,'/Scenarios/',input$filename,'/',input$filename,'_ThreatDistributions.pdf'),
              paste0(HD,'/www/',input$filename,'_ThreatDistributions.pdf'), overwrite = TRUE)
    file.copy(paste0(HD,'/Scenarios/',input$filename,'/',input$filename,'_DefaultFigures.pdf'),
              paste0(HD,'/www/',input$filename,'_DefaultFigures.pdf'), overwrite = TRUE)
    file.copy(paste0(HD,'/Scenarios/',input$filename,'/',input$filename,'_ScenarioFigures.pdf'),
              paste0(HD,'/www/',input$filename,'_ScenarioFigures.pdf'), overwrite = TRUE)
    existing_outputs <- list.files("/net/shiny1/amiller/DST/Scenarios")
    updateSelectInput(session,
                      "run_scenarios",
                      choices = c("",existing_outputs),
                      selected = input$filename)
    removeNotification(id="running")
    
    
  })
  
  #View output tab-----------------------------------------------------------------------------------
  observeEvent(input$run_scenarios, {
    
    if(input$run_scenarios!=''){
      
      output$pdfTables <- renderUI({
        tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_Tables.pdf"))
      }) #adds pdf outputs for figures and tables
      output$pdfGearRedFigs <- renderUI({
        if(file.exists(paste0(HD,"/www/",input$run_scenarios,"_GearRedistributionFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_GearRedistributionFigures.pdf"))
        } else {
          HTML("Gear redistribution maps not generated for this scenario run.")
        }
      }) #adds pdf outputs for figures and tables
      output$pdfThreatDist <- renderUI({
        tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_ThreatDistributions.pdf"))
      }) #adds pdf outputs for figures and tables
      output$pdfDefaultFigs <- renderUI({
        if(file.exists(paste0(HD,"/www/",input$run_scenarios,"_DefaultFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_DefaultFigures.pdf"))
        } else {
          HTML("Default maps not generated for this model run.")
        }
      })
      output$pdfScenarioFigs <- renderUI({
        if(file.exists(paste0(HD,"/www/",input$run_scenarios,"_ScenarioFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_ScenarioFigures.pdf"))
        } else {
          HTML("Scenario maps not generated for this scenario run.")
        }
      }) #adds pdf outputs for figures and tables
      
    }
    
  })
  
  output$renderedReadme <- renderUI({           
    includeHTML(rmarkdown::render(input = paste0(HD,"/README.md"), "html_document"))
  })
  # session$onSessionEnded(function() {
  #   # unlink(isolate(paste0(HD,'/www')),recursive=TRUE) #Removes www folder when Shiny session ends 
  #   # unlink(isolate(paste0(HD,'/InputSpreadsheets')),recursive=TRUE) #Removes InputSpreadsheets folder when Shiny session ends 
  #   # unlink(isolate(paste0(HD,'/Scenarios')),recursive=TRUE) #Removes InputSpreadsheets folder when Shiny session ends 
  #   #Collect files generated during app session to remove when app closes
  #   template = "ScenarioTemplate_V3.0.1"
  #   sessionfiles = list.files(paste0(HD,"/Scenarios/"))
  #   filestoremove = setdiff(sessionfiles,template)
  #   #removes Scenario folders generated during session
  #   unlink(isolate(paste0(HD,"/Scenarios/",filestoremove)),recursive=TRUE)
  #   #removes input spreadsheets generated during session
  #   unlink(isolate(paste0(HD,"/InputSpreadsheets/",filestoremove,'.csv')),recursive=TRUE)
  # })
}
shinyApp(ui, server)