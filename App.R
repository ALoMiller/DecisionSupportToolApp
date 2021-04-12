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
                shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', 
                                    leafletOutput('help_map',width="100%",height="80vh"),
                                    tags$style("
                                                #controls {
                                                  background-color: white;
                                                  opacity: 0.7;
                                                }
                                                #controls:hover{
                                                  opacity: 1;
                                                }
                                                       "),
                                    absolutePanel(id = "controls", class = "panel panel-default",
                                                  top = 85, left = 330, fixed=TRUE,
                                                  draggable = TRUE, 
                                                  height = "auto",
                                                  width = 300, 
                                                  #tags$i(h6("Test Panel")),
                                                  fluidPage(
                                                  #tags$b(h4("Management Areas")),
                                    checkboxInput(input="zero", "Coast-3nm", value=F),
                                    checkboxInput(input="three", "3-12nm", value=F),
                                    checkboxInput(input="twelve", "12nm-EEZ", value=F),
                                    checkboxInput(inputId='stat',label="Statistical Areas",value = F),
                                    checkboxInput(input="lma", "Lobster Management Areas", value=F),
                                    checkboxInput(input="medmr", "Maine DMR Zones", value=F),
                                    checkboxInput(inputId='ma_ra',label="Massachusetts Restricted Areas",value = F),
                                    checkboxInput(inputId='gsc',label="Great South Channel Restricted Area",value = F)
                                    )
                                   
                                    
                                    ))
              )
      ),
      tabItem(tabName = "view_help",
              htmlOutput("renderedReadme"))
    )
  )
)
#Server code

server <- function(input, output, session) {
  ###### LEAFLET HELP MAP  
  output$help_map = renderLeaflet({
    # Makes a leaflet map to visualize management areas
    
    leaflet() %>%
      setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))
  })
  
  observeEvent(input$ma_ra, {
    if(input$ma_ra == T) {
      leafletProxy("help_map") %>% clearGroup(group = "mass_ra")  %>%
        addPolygons(group = "mass_ra" ,data = MASS_RA ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = MASS_RA$AREANAME) %>%
        addPolygons(group = "mass_ra" ,data = MASS_RANE ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = MASS_RANE$AREANAME)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "mass_ra")
    }
  })
  observeEvent(input$gsc, {
    if(input$gsc == T) {
      leafletProxy("help_map") %>% clearGroup(group = "gsc")  %>%
        addPolygons(group = "gsc" ,data = GSC_Gillnet ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3,
                    popup = "Great South Channel Restricted Trap/Pot/Gillnet Area") %>%
        # addPolygons(group = "gsc" ,data = GSC_Trap ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
        #             weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = GSC_Trap$AREANAME) %>%
        addPolygons(group = "gsc" ,data = GSC_Sliver ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = GSC_Sliver$AREANAME) 
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "gsc")
    }
  }) 
  observeEvent(input$zero, {
    if(input$zero == T) {
      leafletProxy("help_map") %>% clearGroup(group = "zero3")  %>%
        addPolygons(group = "zero3" ,data = zero2three ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "zero3")
    }
  }) 
  observeEvent(input$three, {
    if(input$three == T) {
      leafletProxy("help_map") %>% clearGroup(group = "three12")  %>%
        addPolygons(group = "three12" ,data = three2twelve ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "three12")
    }
  }) 
  observeEvent(input$twelve, {
    if(input$twelve == T) {
      leafletProxy("help_map") %>% clearGroup(group = "twelveEEZ")  %>%
        addPolygons(group = "twelveEEZ" ,data = twelve2EEZ ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "twelveEEZ")
    }
  }) 
  observeEvent(input$lma, {
    if(input$lma == T) {
      leafletProxy("help_map") %>% clearGroup(group = "lma")  %>%
        addPolygons(group = "lmar" ,data = LMAs ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = LMAs$AREANAME)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "lma")
    }
  }) 
  observeEvent(input$medmr, {
    if(input$medmr == T) {
      leafletProxy("help_map") %>% clearGroup(group = "me_dmr")  %>%
        addPolygons(group = "me_dmr" ,data = MaineDMR ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = MaineDMR$ZONEID)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "me_dmr")
    }
  }) 
  observeEvent(input$stat, {
    if(input$stat == T) {
      leafletProxy("help_map") %>% clearGroup(group = "stat_areas")  %>%
        addPolygons(group = "stat_areas" ,data = StatAreas ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, 
                    weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3, popup = StatAreas$Id) %>%
        addLabelOnlyMarkers(
          group = "StatAreas",data = coords,lng = ~Long,
          lat = ~Lat, label = ~as.character(StatAreas$Id),
          labelOptions = labelOptions(noHide = T, textOnly = T)) 
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "stat_areas")
    }
  })
  ##### MODEL INPUT SECTION
  observeEvent(input$update_list,{
    # get existing scenarios for listing as scenaerio inputs
    existing_input_csvs <- list.files(here::here("InputSpreadsheets"))
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
      
      DF <- read.csv(paste0(here::here("InputSpreadsheets",input$existing_scenarios),".csv"))
      
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
              file = paste0(here::here("InputSpreadsheets",input$filename),".csv"), na="",row.names = F) #make trap for existing files
    #Run decision tool function here. Will print messages associated w/ function in UI
    withCallingHandlers({
      shinyjs::html("run-text", "")
      tryCatch({
        r.dir <- here::here("R")
        source(file.path(r.dir,"run_decisiontool.R"))
        source(paste0(here::here(),"/function_DecisionSupportTool_V3.0.7.R"))
        
        print('About to run decision tool function.')
        run_decisiontool(HD=here::here(),
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
    if (!fs::dir_exists(paste0(here::here(),'/www'))) fs::dir_create(paste0(here::here(),'/www'))
    #move pdf output files into www directory
    file.copy(paste0(here::here(),'/Scenarios/',input$filename,'/',input$filename,'_GearRedistributionFigures.pdf'),
              paste0(here::here(),'/www/',input$filename,'_GearRedistributionFigures.pdf'), overwrite = TRUE)
    file.copy(paste0(here::here(),'/Scenarios/',input$filename,'/',input$filename,'_Tables.pdf'),
              paste0(here::here(),'/www/',input$filename,'_Tables.pdf'), overwrite = TRUE)
    file.copy(paste0(here::here(),'/Scenarios/',input$filename,'/',input$filename,'_ThreatDistributions.pdf'),
              paste0(here::here(),'/www/',input$filename,'_ThreatDistributions.pdf'), overwrite = TRUE)
    file.copy(paste0(here::here(),'/Scenarios/',input$filename,'/',input$filename,'_DefaultFigures.pdf'),
              paste0(here::here(),'/www/',input$filename,'_DefaultFigures.pdf'), overwrite = TRUE)
    file.copy(paste0(here::here(),'/Scenarios/',input$filename,'/',input$filename,'_ScenarioFigures.pdf'),
              paste0(here::here(),'/www/',input$filename,'_ScenarioFigures.pdf'), overwrite = TRUE)
    existing_outputs <- list.files(here::here("Scenarios"))
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
        if(file.exists(paste0(here::here(),"/Scenarios/",input$run_scenarios,"/",input$run_scenarios,"_GearRedistributionFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_GearRedistributionFigures.pdf"))
        } else {
          HTML("Gear redistribution maps not generated for this scenario run.")
        }
      }) #adds pdf outputs for figures and tables
      output$pdfThreatDist <- renderUI({
        tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_ThreatDistributions.pdf"))
      }) #adds pdf outputs for figures and tables
      output$pdfDefaultFigs <- renderUI({
        if(file.exists(paste0(here::here(),"/Scenarios/",input$run_scenarios,"/",input$run_scenarios,"_DefaultFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_DefaultFigures.pdf"))
        } else {
          HTML("Default maps not generated for this model run.")
        }
      })
      output$pdfScenarioFigs <- renderUI({
        if(file.exists(paste0(here::here(),"/Scenarios/",input$run_scenarios,"/",input$run_scenarios,"_ScenarioFigures.pdf"))){
          tags$iframe(style="height:800px; width:100%", src=paste0(input$run_scenarios,"_ScenarioFigures.pdf"))
        } else {
          HTML("Scenario maps not generated for this scenario run.")
        }
      }) #adds pdf outputs for figures and tables
      
    }
    
  })
  
  output$renderedReadme <- renderUI({           
    includeHTML(rmarkdown::render(input = paste0(here::here(),"/README.md"), "html_document"))
  })
  session$onSessionEnded(function() {
    unlink(isolate(paste0(here::here(),'/www')),recursive=TRUE) #Removes www folder when Shiny session ends 
    #Collect files generated during app session to remove when app closes
    template = "ScenarioTemplate_V3.0.1"
    sessionfiles = list.files(paste0(here::here(),"/Scenarios/"))
    filestoremove = setdiff(sessionfiles,template)
    #removes Scenario folders generated during session
    unlink(isolate(paste0(here::here(),"/Scenarios/",filestoremove)),recursive=TRUE)
    #removes input spreadsheets generated during session
    unlink(isolate(paste0(here::here(),"/InputSpreadsheets/",filestoremove,'.csv')),recursive=TRUE) })
}

shinyApp(ui, server)