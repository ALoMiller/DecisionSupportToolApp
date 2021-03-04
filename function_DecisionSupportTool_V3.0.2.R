
DecisionTool=function(
  HomeDir,
  ModelVersion,
  InputSpreadsheetName,
  MapRefDomain, ## V2.3.0
  GearMapName,
  StringLengthModelName=NA,
  RopeStrengthModelName,
  ThreatModel=NA,
  WhaleInputModel,
  
  CommentText,
  TestScenario=TRUE,
  CoOccurrence=FALSE,
  HighResolution=FALSE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
  RelocationCostExp=1, ## RelocationCost=Distance^(RelocationCostExp); higher values cause more 'fencing' lower values spread gear more evenly
  ExpressRedistribution=FALSE,
  AggregateStrings=TRUE, ## option to aggregate String distributions within vessel classes to decrease run time.
  UpdateEndlineStrengths=TRUE,
  RopeStrengthResolution=100,

  PrintTables=TRUE,
  PrintDefaultMaps=TRUE,
  PrintScenarioMaps=TRUE,
  PrintRedistributionMaps=TRUE, ## maps of Gear that were moved or removed as a result of a closure
  WriteOutputCsv=TRUE,
  WriteMapSources=FALSE,
  WriteDetailedOutput=FALSE,
  ArchiveInputSpreadsheet=FALSE,
  PrintSummary=TRUE,
  ManageMemory=TRUE
) {

  StartTime=Sys.time() ## V2.3.2
  Fold=TRUE ## dummy variable to allow text folding
  #AggregateStrings=TRUE
  #ManageMemory=FALSE
  
  ## Version Notes 
  if(Fold) { ## Version Notes 
    ## V1.2 added option to run in high-resolution (1Nm) rather than low-resolution (10Nm)
    ## This is only useful if one wants to apply management decisions at finer scales
    ## or expects that there is fine-scale co-occurrence patterns between whales and gear
    ## otherwise, this significantly increases model run time.
    
    ## V1.3 added redistribution of Gear around closures. Should have further revision to allow
    ## simultaneous assessment of multiple closures by building the Unmoved / Unaffected Gear monthly
    
    ## V1.4 Implementation of min / max String lengths
    ## SC_MaxRopeDia
    ## SC_StringLength
    ## SC_BuoylineDevice
    ## SC_RopelessDevice
    ## Updated to handle High Resolution
    ## Writing produced maps to .Rdata
    
    ## V1.5 Implemented low-resolution Gear redistribution
    ## Allow closures for partial months
    ## Fishery Constraint with everything but exempt
    ## replaced "Region" field that was mistakenly removed
    
    ## V1.6 
    ## Streamlining of high / low resolution model runs
    ## expanded function arguements to select input layers (Gear, gear, whales, etc.)
    ## Output of model function arguements
    ## expanded summarized output to tables
    ## Expanded output
    ## Error parsing of ScenarioInputs spreadsheet
    ## 1.6.2 fixed code error in Stage 9s aggregation
    
    ## 1.6.3 fixed map ref code in ropeless from MapRef_HR_CF to MapRef_CR
    
    ## V1.6.5 fixed problem of plotting abridged maps with sparse data
    ## V1.6.6 Plot constrained spatial domain map; redefine MapRef_HR with MapRef_HR_Cr to increase model speed
    ## V1.6.6 Update nameing of GearMap for more input flexibility
    ## V1.6.7 Standardize reading in GearMapName; Add input arguement for StringLengthModel
    ## V1.7.0 Implement code for Gear caps
    ##    Renamed Stage 4, 3, and 2 and 5, 4, and 3
    ##    exchanged break() at error catches with informative return() statements
    ## V1.7.1 Added OCC to list of available LMAs
    ##    Added map plotting function and replaced individual mapping code
    
    ## V2.0.0 Switch to rope breaking strength rather than String length
    ## Added CoOccurrence option
    ## added TestScenario option
    ## set default save version=2
    ## V2.0.1 Constrained MapRef to extent of GearMap before spatial constraints added
    ## V2.0.2 Updated to accept rope strength threat model
    
    ## V2.1.0 Updated to accept rope strength threat model with uncertainty
    ## Added option to decrease resolution in rope strength and threat
    
    ## V2.1.1 Added ModelVersion to model run documentation
    ## V2.1.2 Added risk change maps to output
    
    ## 2.1.4 fixed code bug in MaxGearSingleLine > MaxGearSingleLine
    
    ## 2.1.5 added seasonal (month) option to ropeless
    ## 2.1.6 added seasonal (month) option to MaxGearSnglLine
    
    ## 2.1.7 added partial line conversion to lower weight rope
    ## 2.1.8 added UpdateEndlineStrengths option
    
    ## 2.2.0 Updated GearMap with 2017 IEc inshore model; reprocessed A3 with corrected LMA shapefile
    ## Build updated MapRef, GearMap, GearPerStringModel and LinesPerString Model
    ## Updated Index_HR as numeric rather than character string for faster indexing
    ## Updated indices for Whale models Add fields to GearMap and WhaleDensityModel after spatial constraints
    ## 
    
    ## 2.2.1 lose Distance and GearPerStringInt fields throughout; standardized GIS projection throughout
    
    ## 2.2.2 add AggregateStrings option
    ## 2.2.3 added memory management and some tweaks to decrease run time; no changes to outcomes.
    
    ## 2.2.4 moved updating of spatial domain outside spatial constraints section to capture changes in domain from fishery constraints
    
    ## 2.2.5 added capacity to read input and write output with subfolders
    
    ## 2.2.6 fixed write InputSpreadsheet to output; Added ArchiveInputSpreadsheet
    ## 2.2.7 Fixed Stage3s_GearDensity_PostClosure_Px and Stage7s_LineStrength output map names
    ## V2.2.8 catch for CoOccurrence run with MaxRopeStrength
    ## V2.2.8 catch for non-zero lines affected by MaxRopeStrength scenario
    
    ## V2.2.9 Fixed error catch in non-zero lines, stage 7
    
    ## V2.2.9 spTransform MapRef_LR to Proj4
    ## V2.2.9 fixed plotting error for 8d; how could this have worked before?
    
    ## V2.3.0 MapRef not constrained to GearMap domain
    ## V2.3.0 MapRefDomain now specified
    ## V2.3.0 Changed OutputMap function input argument from 'MapRef_LR=' to 'MapRef=' and attempts to convert to spatial pixels internally.
    ## V2.3.0 Added script to tranfer attributed data from MapRef_HR to _LR for low-resolution runs
    
    ## V2.3.1 Major revamp to Gear redistribution routine. Primarily in response to bug that wouldn't run multiple closures correctly
    ##        Futher added spatial realism by redistributing on a per-source pixel basis; may be computationally slow in HR and need an 'express' option
    ## V2.3.1 Added RelocationCostExp argument to allow users to adjust spread of Gear adjacent to closures
    
    ## V2.3.2 Added internal runtime clock
    
    ## V3.0.0 Update to trap / gillnet terminology compromise with gear assembled into strings
    
    ## V3.0.1 changed input spreadsheet to LCMAs_Simple_Bad
    ## V3.0.1 added break in closed areas when no index points inside closure
    ## V3.0.1 fixed extent of closed area loop so it doesn't crash
    
    ## V3.0.2 fixed error in defining UnAffectedTraps in closure scenarios. Affects all scenarios involving multiple closures where the months do not match. 
    
    ## To Do List
    ## Gearcaps for Area 3
    ## Gear caps for Mass, etc.
    ## Capture Gear, Lines redistributed in output tables
    ## Include Model version used in ModelConfiguration
    ## produce scatterplot of threat, gear density and whale density, monthly?
    ## option to produce high-res maps
    ## add constrained MapRef to extended output
    
  } ## fold Version notes
  
  if(Fold) { ## basic startup 
    
    library(grid)
    library(gtable)
    library(gridExtra)
    library(maptools)
    library(rgdal)
    library(geosphere);
    library(lattice);
    
    spRef_UTM_19="+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    spRef_DD="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
    ####################### Misc Functions ############################--
    source(paste(HD, "/FunctionsEtc/functionsDecisionSupportTool_V2.3.0.R", sep=""))
    print(paste("Running ", InputSpreadsheetName))
    
    ## capture model configuration information 
    ModelConfiguration=data.frame(ModelConfiguration=c(
      as.character(Sys.time()),
      paste("Home Directory: ", HD),
      paste("Model Version: ", ModelVersion),
      paste("Input Spreadsheet: ", InputSpreadsheetName),
      paste("MapRefDomain: ", MapRefDomain),## V2.3.0
      paste("GearMap: ", GearMapName),
      paste("StringLengthModel: ", "IncludedInGearMap"),
      paste("Rope Strength Model: ", RopeStrengthModelName),
      paste("Threat Model: ", ThreatModel),
      paste("Whale Model:", WhaleInputModel),
      paste(" "),
      paste("Comment: ", CommentText ),
      paste("CoOccurrence:", CoOccurrence),
      paste("Run Test Scenario:", TestScenario),
      paste("AggregateStrings=", AggregateStrings),
      paste("HighResolution=", HighResolution),
      paste("RelocationCostExp: ", RelocationCostExp),
      paste("ExpressRedistribution: ", ExpressRedistribution),
      paste("Update Endline Strengths: ", UpdateEndlineStrengths),
      paste("RopeStrengthResolution: ", RopeStrengthResolution),
      paste(" "),
      paste("PrintTables=", PrintTables),
      paste("PrintDefaultMaps=", PrintDefaultMaps),
      paste("PrintScenarioMaps=", PrintScenarioMaps),
      paste("PrintRedistributionMaps=", PrintRedistributionMaps),
      paste("WriteMapSources=", WriteMapSources),
      paste("WriteOutputCsv=", WriteOutputCsv),
      paste("WriteDetailedOutput=", WriteDetailedOutput),
      paste("ArchiveInputSpreadsheet: ", ArchiveInputSpreadsheet)
    )); print(ModelConfiguration  )
    
    ## check and set home directory of input and output files
    if(dir.exists(HomeDir)){
      HD=HomeDir; setwd(HD)
    } else {
      message("Error: Home Directory does not exist"); return(HD)
    }
    
    OutputDir=gsub(".csv", "", InputSpreadsheetName); OutputDir
    
    ## detect if a subfolder is specified for input csv and output
    SubfolderPos=regexpr("/", OutputDir)[1] ## V2.2.5
    if(SubfolderPos>0){
      HasSubfolder=TRUE
      Subfolder=substr(OutputDir,1,SubfolderPos-1); Subfolder;
      OutputDir=substr(OutputDir,SubfolderPos+1, nchar(OutputDir)); OutputDir
    } else { 
      HasSubfolder=FALSE} ## V2.2.5
    
    
  } ## ## basic startup 
  
  ## 0.0 Load standard inputs #############################################--
  
  if(Fold) { 
    message("Loading Data")
    ## 0.1 Start with Gear per grid by month. 
    ## This is calculated from the Area 3 Vertical Line model as a function of the number of vertical lines and String lengths. 
    ## For Area 3, this need to be modeled separately for the crab and lobster fishery with two different classes of lobster vessels.
    
    load(paste(HD, "/Inputs/", MapRefDomain, sep="")) ## V2.3.0
    Proj4=proj4string(MapRef_HR); Proj4 ## get projection to use for all future spatial objects
    ###summary(MapRef_HR)
    
    # load(paste(HD, "/Inputs/MapRef_LR.Rdata", sep="")) ## as of MapRef v2.2 MapRef_LR is included with _HR
    # MapRef_LR=spTransform(MapRef_LR, Proj4) ## V2.2.8 ## sould be obsolete as of 2.3.0
    ### summary(MapRef_LR)
    
    ## Gear Map ##################################################--
    ## note GearPerStringModel and EndlinesPerString included in GearMap starting V2.2.0
    if(file.exists(paste0(HD, "/Inputs/", GearMapName))){ 
      load(paste0(HD, "/Inputs/", GearMapName))
    } else {
      message("Error: Specified Gear Map not found"); return(paste0(HD, "/Inputs/", GearMapName))
    }
    names(GearPerStringModel)[names(GearPerStringModel)=="GearPerString"]="GearPerString"
    
    # GearMap=GearMap_V2.2; rm(GearMap_V2.2, GearMap_V2.2_readMe);
    # GearPerStringModel=GearPerStringModel_V2.2; rm(GearPerStringModel_V2.2);
    # EndlinesPerString=EndlinesPerString_V2.2; rm(EndlinesPerString_V2.2);
    ### summary(GearMap)
    ## head(GearMap)
    
    if(AggregateStrings){ ## aggregate and rebuild GearPerString without length distributions
      # summary(GearPerStringModel)
      GearPerStringModel$WtProp=with(GearPerStringModel, GearPerString*StringProportion);
      GearPerStringModel_Agg=aggregate(WtProp~VesselKey+Month, GearPerStringModel, sum);
      names(GearPerStringModel_Agg)=c("VesselKey", "Month", "GearPerString");
      # summary(GearPerStringModel_Agg)
      GearPerStringModel_Agg$StringProportion=1;
      GearPerStringModel_Agg$StringUnitCost=
        GearPerStringModel_Agg$GearPerString * 
        GearPerStringModel_Agg$StringProportion
      GearPerStringModel=GearPerStringModel_Agg
    }
    
    ## Shapefile and Cumulative Gear fished ##############################--
    GearCap_Regions=readOGR(dsn=paste0(HD, "/InputShapefiles"),
                            layer="Regions_GearCap", 
                            stringsAsFactors=FALSE, verbose = FALSE); #plot(GearCap_Regions)
    GearCap_Regions=spTransform(GearCap_Regions, Proj4);
    load(paste0(HD, "/Inputs/GearCapCDF.Rdata")); #summary(GearCapCDF)
    
    
    ## Zone Adjacency for spatial redistribution of Gear ################--
    ZoneAdjacency=read.csv(paste(HD, "/Inputs/ZoneAdjacency_DSTv2.2.csv", sep=""), stringsAsFactors = FALSE); 
    summary(ZoneAdjacency);
    ZoneAdjacency$SourceZone=gsub(" ", "", ZoneAdjacency$SourceZone)
    ZoneAdjacency$SinkZone=gsub(" ", "", ZoneAdjacency$SinkZone)
    
    ##################### Rope Strength / String Length Model #############################--
    if(CoOccurrence){ ## if CoOccurrence only
      LineStrengthMod=data.frame( ## build a simple line strength model where all Strings have one rope strength
        # GearPerStringInt=1:100,
        GearPerString_Applied=1:100,
        RopeStrength=1,
        Prop_Strength=1
      )
    } else { ## otherwise, load the rope strength model
      
      RopeStrengthString=paste0(HD, "/Inputs/", RopeStrengthModelName)
      if(file.exists(RopeStrengthString)){
        load(RopeStrengthString);
        # names(LineStrengthMod)=c("GearPerStringInt", "RopeStrength", "Prop_Strength")
        names(LineStrengthMod)=c("GearPerString_Applied", "RopeStrength", "Prop_Strength") ## V2.1.8 field name change
      } else {
        message("Error: Specified rope strength  model not found"); return(RopeStrengthString)
      }; 
      # summary(LineStrengthMod)
      
      if(!is.na(RopeStrengthResolution)){ ## aggregate rope strength model to specified resolution
        LineStrengthMod$RopeStrength_Agg=
          floor(
            (LineStrengthMod$RopeStrength)/RopeStrengthResolution
          ) * RopeStrengthResolution + RopeStrengthResolution/2
        
        LineStrengthMod=aggregate(Prop_Strength~GearPerString_Applied+RopeStrength_Agg,
                                  LineStrengthMod, sum)
        names(LineStrengthMod)=c("GearPerString_Applied", "RopeStrength", "Prop_Strength");
        # dim(LineStrengthMod)
        # aggregate(Prop_Strength~GearPerString_Applied, LineStrengthMod, sum)
        # head(LineStrengthMod)
        # LineStrengthMod[LineStrengthMod$GearPerString_Applied==5, ]
        # xyplot(Prop_Strength~RopeStrength, groups=GearPerString_Applied, LineStrengthMod, type="l")
      }
      
    } ## end Rope Strength Model
    
    
    # summary(LineStrengthMod); dim(LineStrengthMod)
    
    ###########################################################################--
    if(CoOccurrence){ ## skip threat model if running CoOccurrence
      ModRopeThreat=data.frame(
        RopeStrength=1,
        ThreatMod=c("CoOccurrence"),
        Threat=1
      )
    } else {
      
      ThreatString=paste0(HD, "/Inputs/", ThreatModel)
      if(file.exists(ThreatString)){
        load(ThreatString);
        # summary(ModRopeThreat)
        if(!is.na(RopeStrengthResolution)){ ## aggregate rope strength model to specified resolution
          ModRopeThreat$RopeStrength_Agg=
            floor(
              (ModRopeThreat$RopeStrength)/RopeStrengthResolution
            ) * RopeStrengthResolution + RopeStrengthResolution/2
          # plot(RopeStrength_Agg~RopeStrength, ModRopeThreat); abline(0,1)
          
          ModRopeThreat=aggregate(cbind(Threat, Threat_Lower, Threat_Upper)~RopeStrength_Agg, ModRopeThreat, mean)
          names(ModRopeThreat)=c("RopeStrength", "Threat", "Threat_Lower", "Threat_Upper")
        }
        
        ModRopeThreat$CoOccurrence=1
        
        ModRopeThreat=Wide2Long(ModRopeThreat, 
                                cols=2:ncol(ModRopeThreat), 
                                fName="ThreatMod", 
                                vName="Threat")    
        # xyplot(Threat~RopeStrength, ModRopeThreat, groups=ThreatMod, type=c("b", "g"))
        
      } else {
        message("Error: Specified threat model not found"); return(ThreatString)
      }
    } ## end load rope threat model
    
    RopelessRisk=read.csv(paste(HD, "/Inputs/RopelessRisk.csv", sep=""), stringsAsFactors = FALSE); 
    # RopelessRisk=RopelessRisk[ ,c("RopelessDevice", "Distance", "LineMultiplier")]
    
    GearConversion=read.csv(paste(HD, "/Inputs/GearConversion.csv", sep=""), stringsAsFactors=FALSE);
    # summary(GearConversion)
    
    ## StringLengthModel - Now read in with GearMap V>2.2.0 #########################################--
    # if(file.exists(paste0(HD, "/Inputs/", StringLengthModelName))){
    #   load(paste0(HD, "/Inputs/", StringLengthModelName))
    # } else {
    #   message("Error: Specified String Length Model not found"); return(paste0(HD, "/Inputs/", StringLengthModelName))
    # }
    # names(StringLengthModel)=c("Region","VesselKey","Month","GearPerString","StringProportion","StringUnitCost") #V2.1.8 Found a bug!
    # 
    ## RopeDiameterModel
    
    
    ## Endlines per String model - Now read in with GearMap V>2.2.0
    # load(paste(HD, "/Inputs/EndlinesPerStringModel.Rdata", sep=""))
    # summary(EndlinesPerString)
    
    ## Whale Habitat Model ##################################################################################--
    WhaleString=paste0(HD, "/Inputs/", WhaleInputModel); WhaleString
    if(file.exists(WhaleString)){
      load(WhaleString);
    } else {
      message("Error: Specified Whale Model file not found"); return(WhaleString)
    }
    dim(WhaleDensityModel)
    #summary(WhaleDensityModel)
    
    
    # sub=WhaleDensityModel[WhaleDensityModel$Month==4, ]
    # coordinates(sub)=c("x", "y");
    # spplot(sub["WhaleDensity"])
    
    MonthString=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    MonthOrder=c("Sep", "Oct", "Nov", "Dec",
                 "May", "Jun", "Jul", "Aug", 
                 "Jan", "Feb", "Mar", "Apr")
    
    MonthDF=data.frame(
      Month=1:12,
      MonthName=MonthString
    )
  } ## Load model inputs and submodels
  
  if(Fold) { ## load GIS layers and bathymetry
    ShapefileDir=paste(HD, "/InputShapefiles", sep="")
    # message("Loading Shapefiles")
    spStatAreas=readOGR(dsn=ShapefileDir, 
                        layer="StatAreas_DecisionTool",
                        verbose=FALSE)
    spStatAreas=spTransform(spStatAreas, Proj4);
    # plot(spStatAreas)
    # text(
    #   getSpPPolygonsLabptSlots(spStatAreas),
    #   labels=as.character(spStatAreas$Id),
    #   cex=0.4)
    
    spLMAs=readOGR(dsn=ShapefileDir, 
                   layer="LCMAs_Simple_Bad",
                   verbose=FALSE)
    proj4string(spLMAs)=Proj4;
    #summary(spLMAs)
    spLMAs=spLMAs[spLMAs$Name %in% c("A1", "A2", "OCC", "A2_3overlap", "A3"), ]
    # plot(spLMAs)
    
    # par(mar=c(1,1,1,1))
    # plot(spStatAreas)
    # text(
    #   getSpPPolygonsLabptSlots(spStatAreas),
    #   labels=as.character(spStatAreas$Id),
    #   cex=1)
    # plot(spLMAs, border="blue", add=TRUE, lwd=3)
    
    Coast=readOGR(dsn=ShapefileDir, layer="EastCoastLines",
                  verbose=FALSE); #plot(Coast); 
    # Iso100=readOGR(dsn=ShapefileDir, layer="100f_Isobath",
    #                verbose=FALSE); #plot(Iso100); summary(Iso100)
    
    ## objects for spatial plotting
    spCoast_layout1=list("sp.lines", as(Coast, "SpatialLinesDataFrame"), lwd=3, col="white")
    spCoast_layout2=list("sp.lines", as(Coast, "SpatialLinesDataFrame"), lwd=1, col="black")
    
    spLMA_layout1=list("sp.lines", as(spLMAs, "SpatialLinesDataFrame"), lwd=3, col="white")
    spLMA_layout2=list("sp.lines", as(spLMAs, "SpatialLinesDataFrame"), lwd=1, col="black")
    
    spStatAreas_layout1=list("sp.lines", as(spStatAreas, "SpatialLinesDataFrame"), lwd=3, col="white")
    spStatAreas_layout2=list("sp.lines", as(spStatAreas, "SpatialLinesDataFrame"), lwd=1, col="black")
  } ## load GIS layers and bathymetry 
  
  OutputData=data.frame(
    Variable=character(0),
    Scenario=character(0),
    Month=character(0),
    Value=numeric(0)
  ); #OutputData
  
  ##############################################################--
  ## 0.1 Scenario Inputs
  if(Fold) {
    
    ## read in input spreadsheet
    if(file.exists(paste("InputSpreadsheets", ## subdirectory
                         InputSpreadsheetName, ## file name
                         sep="/"))){
      ScenarioInputs=read.csv(
        paste("InputSpreadsheets", ## subdirectory
              InputSpreadsheetName, ## file name
              sep="/"), stringsAsFactors=FALSE, na.strings=""); 
    } else {
      message("Error: InputSpreadsheet not found in expected directory"); return(paste("InputSpreadsheets", ## subdirectory
                                                                                     InputSpreadsheetName, ## file name
                                                                                     sep="/"))
    }
    
    ScenarioInputs=ScenarioInputs[!is.na(ScenarioInputs$Action), ]
    if(nrow(ScenarioInputs)==0){ ## v2.2.6
      message("No actions specified in the input spreadsheet.")
      message("If no actions are desired, enter 'NoAction' under 'Actions' in the input spreadsheet")
      return(paste("InputSpreadsheets", ## subdirectory
                   InputSpreadsheetName, ## file name
                   sep="/"))
    }
    
    ScenarioInputs$StatArea=as.character(ScenarioInputs$StatArea)
    ScenarioInputs$Months=as.character(ScenarioInputs$Months)
    
    ScenarioInputs
    
    ## series of tests to ensure applicable values are entered
    if(Fold) { ## check constraints on inputs for typos, etc.
      AvailableActions=c("Constraint_Fishery",  "Constraint_Spatial",  "GearReduction",  "GearCap", 
                         "Closure",  "StringLength", "MaxGearWSingleLine", "MaxRopeStrength",  "BuoylineDevice", "RopelessDevice",
                         "NoAction" ##2.2.6
      ); AvailableActions
      
      AvailableLMAs=c("All", "A1", "A2", "A2_3overlap", "A3", "OCC"); #AvailableLMAs
      
      AvailableStates=c("All", "ME", "NH", "MA"); #AvailableStates
      
      AvailableFisheries=c("All", "NonExempt", "Exempt", "Midshelf_Lobster", "Midshelf_Crab", "Offshore_Lobster",
                           "Offshore_Crab", "EverythingButExempt"); #AvailableFisheries
      
      AvailableStatAreas=as.character(c(464, 465, 511, 512, 513, 514, 515, 521, 522, 525, 526, 561, 562, 537, 538, 539))
      
      AvailableStringRegulations=c("Min", "Max", "Exactly" )
      
      AvailableMaxRopeStrength=unique(ModRopeThreat$RopeStrength);
      
      AvailableBuoylineDevice=c("1,700@100m", "SSS_Regular", "SSS_To500m", "TimedTensionCutter")
      
      AvailableRopelessDevice=c("TimedRelease", "AcousticRelease")
      
      
      if(!all(ScenarioInputs$Action %in% AvailableActions, na.rm=TRUE)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'Action' field"); return(ScenarioInputs$Action)
      }
      
      if(!all(na.omit(ScenarioInputs$LMA) %in% AvailableLMAs)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'LMA' field"); return(ScenarioInputs$LMA)
      }
      
      if(!all(na.omit(ScenarioInputs$State) %in% AvailableStates)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'State' field"); return(ScenarioInputs$State)
      }
      
      if(!all(na.omit(ScenarioInputs$Fishery) %in% AvailableFisheries)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'Fishery' field"); return(ScenarioInputs$Fishery)
      }
      
      if(!all(na.omit(ScenarioInputs$StatArea) %in% AvailableStatAreas)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'StatArea' field"); return(ScenarioInputs$StatArea)
      }
      
      if(!all(na.omit(ScenarioInputs$StringRegulation) %in% AvailableStringRegulations)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'StringRegulation' field"); return(ScenarioInputs$StringRegulation)
      }
      
      if(!all(na.omit(ScenarioInputs$MaxRopeStrength) >= min(AvailableMaxRopeStrength) ) &
         !all(na.omit(ScenarioInputs$MaxRopeStrength) <= max(AvailableMaxRopeStrength) ) ) {
        message("Error in InputSpreadsheet: 'MaxRopeStrength' field out of bounds"); return(ScenarioInputs$MaxRopeStrength)
      }
      
      ## check that max rope strengths match available values in ModRopeStrength
      
      if(!all(na.omit(ScenarioInputs$BuoylineDevice) %in% AvailableBuoylineDevice)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'BuoylineDevice' field"); return(ScenarioInputs$BuoylineDevice)
      }
      
      if(!all(na.omit(ScenarioInputs$RopelessDevice) %in% AvailableRopelessDevice)) {
        message("Error in InputSpreadsheet: Unrecognized input in 'RopelessDevice' field"); return(ScenarioInputs$RopelessDevice)
      }
    } ## check constraints on inputs for typos, etc.
    
    #################
    if(Fold) { ## fold reading individual actions and error checks
      ScenarioInputPass=TRUE
      
      Constraints_Spatial=ScenarioInputs[ScenarioInputs$Action=="Constraint_Spatial", ];
      if(nrow(Constraints_Spatial)>0){
        for(i in 1:nrow(Constraints_Spatial)){
          if(!is.na(Constraints_Spatial$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under Constraints_Spatial")
          }
          if(!is.na(Constraints_Spatial$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under Constraints_Spatial")
          }
        }
        Constraints_Spatial=Constraints_Spatial[ ,c("Action", "LMA", "State", "StatArea", "Shapefile")]
      }
      
      Constraints_Fishery=ScenarioInputs[ScenarioInputs$Action=="Constraint_Fishery", ]
      if(nrow(Constraints_Fishery)>0){
        for(i in 1:nrow(Constraints_Fishery)){
          if(!is.na(Constraints_Fishery$LMA[i])){
            message("Warning: In input csv, non-Null input in LMA field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$State[i])){
            message("Warning: In input csv, non-Null input in State field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$StatArea[i])){
            message("Warning: In input csv, non-Null input in StatArea field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$Shapefile[i])){
            message("Warning: In input csv, non-Null input in Shapefile field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$Months[i])){
            message("Warning: In input csv, non-Null input in Months field under Constraints_Fishery")
          }
          # if(!is.na(Constraints_Fishery$Fishery[i])){
          #   message("Warning: In input csv, non-Null input in Fishery field under Constraints_Fishery")
          # }
          if(!is.na(Constraints_Fishery$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under Constraints_Fishery")
          }
          if(!is.na(Constraints_Fishery$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under Constraints_Fishery")
          }
          ###            
          if(is.na(Constraints_Fishery$Fishery[i])){
            message("Error: In input csv, Null value in required field Fishery field under Constraints_Fishery"); ScenarioInputPass=FALSE
          }
        }
        Constraints_Fishery=Constraints_Fishery[ ,c("Action", "Fishery")]
      }
      
      SC_Closures=ScenarioInputs[ScenarioInputs$Action=="Closure", ];
      SC_Closures$Percentage[SC_Closures$Percentage>0.9999]=0.9999
      if(nrow(SC_Closures)>0){
        for(i in 1:nrow(SC_Closures)){
          if(!is.na(SC_Closures$LMA[i])){
            message("Warning: In input csv, non-Null input in LMA field under Closures")
          }
          if(!is.na(SC_Closures$State[i])){
            message("Warning: In input csv, non-Null input in State field under Closures")
          }
          if(!is.na(SC_Closures$StatArea[i])){
            message("Warning: In input csv, non-Null input in StatArea field under Closures")
          }
          if(!is.na(SC_Closures$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under Closures")
          }
          
          if(!is.na(SC_Closures$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Closures")
          }
          if(!is.na(SC_Closures$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under Closures")
          }
          if(!is.na(SC_Closures$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under Closures")
          }
          if(!is.na(SC_Closures$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under Closures")
          }
          if(!is.na(SC_Closures$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under Closures")
          }
          if(!is.na(SC_Closures$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under Closures")
          }
          if(!is.na(SC_Closures$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under Closures")
          }
          ###            
          if(is.na(SC_Closures$Shapefile[i])){
            message("Error: In input csv, Null value in required field Shapefile field under Closures"); ScenarioInputPass=FALSE
          }
        }
        SC_Closures=SC_Closures[ ,c("Action", "Shapefile", "Months", "Percentage")]
        SC_Closures$Percentage[is.na(SC_Closures$Percentage)]=1 ## fill in values for all Percentages
      }
      
      SC_GearReductions=ScenarioInputs[ScenarioInputs$Action=="GearReduction", ];
      if(nrow(SC_GearReductions)>0){
        SC_GearReductions$Percentage[SC_GearReductions$Percentage>0.9999]=0.9999
        for(i in 1:nrow(SC_GearReductions)){
          
          if(!is.na(SC_GearReductions$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under GearReductions")
          }
          
          if(!is.na(SC_GearReductions$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under GearReductions")
          }
          if(!is.na(SC_GearReductions$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under GearReductions")
          }
          if(!is.na(SC_GearReductions$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under GearReductions")
          }
          if(!is.na(SC_GearReductions$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under GearReductions")
          }
          if(!is.na(SC_GearReductions$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under GearReductions")
          }
          if(!is.na(SC_GearReductions$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under GearReductions")
          }
          if(!is.na(SC_GearReductions$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under GearReductions")
          }
          ###            
          if(is.na(SC_GearReductions$Percentage[i])){
            message("Error: In input csv, Null value in required field Percentage field under GearReductions"); ScenarioInputPass=FALSE
          }
        }
        SC_GearReductions=SC_GearReductions[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","Percentage")]
      }
      
      SC_GearCaps=ScenarioInputs[ScenarioInputs$Action=="GearCap", ];
      if(nrow(SC_GearCaps)>0){
        for(i in 1:nrow(SC_GearCaps)){
          
          if(!is.na(SC_GearCaps$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under GearCaps")
          }
          
          if(!is.na(SC_GearCaps$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under GearCaps")
          }
          if(!is.na(SC_GearCaps$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under GearCaps")
          }
          if(!is.na(SC_GearCaps$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under GearCaps")
          }
          if(!is.na(SC_GearCaps$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under GearCaps")
          }
          if(!is.na(SC_GearCaps$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under GearCaps")
          }
          if(!is.na(SC_GearCaps$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under GearCaps")
          }
          
          if(!is.na(SC_GearCaps$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under GearCaps")
          }
          ###            
          if(is.na(SC_GearCaps$GearCap[i])){
            message("Error: In input csv, Null value in required field GearCap under action SC_GearCaps"); ScenarioInputPass=FALSE
          }
        }
        SC_GearCaps=SC_GearCaps[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","GearCap")]
      }
      
      SC_MaxRopeStrength=ScenarioInputs[ScenarioInputs$Action=="MaxRopeStrength", ];
      if(nrow(SC_MaxRopeStrength)>0){
        if(CoOccurrence){ ## V2.2.8 catch for CoOccurrence run with MaxRopeStrength
          message("Warning: Running a CoOccurrence run while modifying MaxRopeStrength")
          SC_MaxRopeStrength$MaxRopeStrength=1
        } else {
          for(i in 1:nrow(SC_MaxRopeStrength)){
            if(!is.na(SC_MaxRopeStrength$MaxRopeStrength[i]) & 
               !SC_MaxRopeStrength$MaxRopeStrength[i] %in% AvailableMaxRopeStrength){ ## if not match
              
              AdjustedMaxRopeStrength=AvailableMaxRopeStrength[ ## find and replace with closest available value
                which(abs(SC_MaxRopeStrength$MaxRopeStrength[i]-AvailableMaxRopeStrength)==
                        min(abs(SC_MaxRopeStrength$MaxRopeStrength[i]-AvailableMaxRopeStrength)))][1]
              message("Warning: Scenario MaxRopeStrength value not matched with available Rope Threat Model")
              message(paste0("Replacing input value of ", 
                           SC_MaxRopeStrength$MaxRopeStrength[i], 
                           " with best matching value of ",
                           AdjustedMaxRopeStrength))
              
              SC_MaxRopeStrength$MaxRopeStrength[i]=AdjustedMaxRopeStrength
            }
          } ## end check of available RopeStrengths
        }
        ## match scenario rope strenght to threat models
        
        for(i in 1:nrow(SC_MaxRopeStrength)){
          
          if(!is.na(SC_MaxRopeStrength$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under MaxRopeStrength")
          }
          
          # if(!is.na(SC_MaxRopeStrength$Percentage[i])){ ## V2.1.7
          #   message("Warning: In input csv, non-Null input in Percentage field under MaxRopeStrength")
          # }
          if(!is.na(SC_MaxRopeStrength$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under MaxRopeStrength")
          }
          if(!is.na(SC_MaxRopeStrength$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under MaxRopeStrength")
          }
          
          if(!is.na(SC_MaxRopeStrength$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under MaxRopeStrength")
          }
          if(!is.na(SC_MaxRopeStrength$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under MaxRopeStrength")
          }
          if(!is.na(SC_MaxRopeStrength$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under MaxRopeStrength")
          }
          if(!is.na(SC_MaxRopeStrength$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under MaxRopeStrength")
          }
          ###            
          if(is.na(SC_MaxRopeStrength$MaxRopeStrength[i])){
            message("Error: In input csv, Null value in required field MaxRopeStrength field under MaxRopeStrength"); ScenarioInputPass=FALSE
          }
        }
        SC_MaxRopeStrength=SC_MaxRopeStrength[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "Percentage", "MaxRopeStrength")]
      }
      
      SC_StringLength=ScenarioInputs[ScenarioInputs$Action=="StringLength", ]
      if(nrow(SC_StringLength)>0){
        for(i in 1:nrow(SC_StringLength)){
          if(!is.na(SC_StringLength$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under StringLength")
          }
          if(!is.na(SC_StringLength$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under StringLength")
          }
          if(!is.na(SC_StringLength$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under StringLength")
          }
          if(!is.na(SC_StringLength$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under StringLength")
          }
          if(!is.na(SC_StringLength$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under StringLength")
          }
          if(!is.na(SC_StringLength$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under StringLength")
          }
          if(!is.na(SC_StringLength$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under StringLength")
          }
          ###            
          if(is.na(SC_StringLength$StringRegulation[i])){
            message("Error: In input csv, Null value in required field StringRegulation field under StringLength"); ScenarioInputPass=FALSE
          }
          if(is.na(SC_StringLength$StringLen[i])){
            message("Error: In input csv, Null value in required field StringLen field under StringLength"); ScenarioInputPass=FALSE
          }
        }
        SC_StringLength=SC_StringLength[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","StringRegulation", "StringLen")]
      }
      
      SC_BuoylineDevice=ScenarioInputs[ScenarioInputs$Action=="BuoylineDevice", ];
      if(nrow(SC_BuoylineDevice)>0){
        for(i in 1:nrow(SC_BuoylineDevice)){
          if(!is.na(SC_BuoylineDevice$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under BuoylineDevice")
          }
          if(!is.na(SC_BuoylineDevice$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under BuoylineDevice")
          }
          ###            
          if(is.na(SC_BuoylineDevice$BuoylineDevice[i])){
            message("Error: In input csv, Null value in required field BuoylineDevice field under BuoylineDevice"); ScenarioInputPass=FALSE
          }
        }
        SC_BuoylineDevice=SC_BuoylineDevice[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","BuoylineDevice")]
      }
      
      SC_RopelessDevice=ScenarioInputs[ScenarioInputs$Action=="RopelessDevice", ];
      if(nrow(SC_RopelessDevice)>0){
        for(i in 1:nrow(SC_RopelessDevice)){
          if(!is.na(SC_RopelessDevice$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under RopelessDevice")
          }
          if(!is.na(SC_RopelessDevice$MaxGearSnglLn[i])){
            message("Warning: In input csv, non-Null input in MaxGearSnglLn field under RopelessDevice")
          }
          ###            
          if(is.na(SC_RopelessDevice$RopelessDevice[i])){
            message("Error: In input csv, Null value in required field RopelessDevice field under RopelessDevice"); ScenarioInputPass=FALSE
          }
        }
        SC_RopelessDevice=SC_RopelessDevice[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","RopelessDevice")]
      }
      
      SC_MaxGearWSingleLine=ScenarioInputs[ScenarioInputs$Action=="MaxGearWSingleLine", ]
      if(nrow(SC_MaxGearWSingleLine)>0){
        for(i in 1:nrow(SC_MaxGearWSingleLine)){
          if(!is.na(SC_MaxGearWSingleLine$Fishery[i])){
            message("Warning: In input csv, non-Null input in Fishery field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$Percentage[i])){
            message("Warning: In input csv, non-Null input in Percentage field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$StringRegulation[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$StringLen[i])){
            message("Warning: In input csv, non-Null input in StringRegulation field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$MaxRopeStrength[i])){
            message("Warning: In input csv, non-Null input in MaxRopeStrength field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$BuoylineDevice[i])){
            message("Warning: In input csv, non-Null input in BuoylineDevice field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$RopelessDevice[i])){
            message("Warning: In input csv, non-Null input in RopelessDevice field under MaxGearWSingleLine")
          }
          if(!is.na(SC_MaxGearWSingleLine$GearCap[i])){
            message("Warning: In input csv, non-Null input in GearCap field under MaxGearWSingleLine")
          }
          if(is.na(SC_MaxGearWSingleLine$MaxGearSnglLn[i])){
            message("Error: In input csv, Null value in required field MaxGearSnglLn field under MaxGearWSingleLine"); ScenarioInputPass=FALSE
          }
        }
        SC_MaxGearWSingleLine=SC_MaxGearWSingleLine[ ,c("Action", "LMA", "State", "StatArea", "Shapefile", "Months","MaxGearSnglLn")]
      }
      
      if(ScenarioInputPass==FALSE){
        message("Error in inputs csv file. Ending model run"); return(ScenarioInputPass)
      }
      
    } ## fold reading individual actions and error checks
    
  }  ## Scenario.csv
  
  #############################################################--

  ## 0.2 Constrain spatial extent based on user inputs
  if(Fold) { ## fold spatial constraint 
    
    ## constrain spatial domain to extent of GearMap
    # GearMap_Indices=unique(GearMap$Index_HR) ## V2.3.0
    # MapRef_HR=MapRef_HR[MapRef_HR$Index_HR %in% GearMap_Indices, ]; ## V2.3.0
    # MapRef_LR=MapRef_LR[MapRef_LR$Index_LR %in%  MapRef_HR$Index_LR, ];
    
    if(nrow(Constraints_Spatial)>0){
      
      for(i in 1:nrow(Constraints_Spatial)){
        ## constrain spatially
        MapRef_HR_CrI=MapRef_HR;
        if(!is.na(Constraints_Spatial$LMA[i])) {
          MapRef_HR_CrI=MapRef_HR_CrI[MapRef_HR_CrI$LMA==Constraints_Spatial$LMA[i] &
                                        !is.na(MapRef_HR_CrI$LMA), ]
        } 
        if(!is.na(Constraints_Spatial$State[i])) {
          MapRef_HR_CrI=MapRef_HR_CrI[MapRef_HR_CrI$State==Constraints_Spatial$State[i] &
                                        !is.na(MapRef_HR_CrI$State), ]
        } 
        if(!is.na(Constraints_Spatial$StatArea[i])) {
          StatAreasI=as.numeric(strsplit(Constraints_Spatial$StatArea[i], ",")[[1]])
          MapRef_HR_CrI=MapRef_HR_CrI[MapRef_HR_CrI$StatArea %in% StatAreasI &
                                        !is.na(MapRef_HR_CrI$StatArea), ]
        }
        if(!is.na(Constraints_Spatial$Shapefile[i])) {
          Constraints_SpatialShape=Constraints_Spatial$Shapefile[i]; Constraints_SpatialShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=Constraints_SpatialShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, Proj4)
          # plot(ShapeI)
          MapRef_HR_CrI = MapRef_HR_CrI[!is.na(over(MapRef_HR_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          # plot(MapRef_HR_CrI, pch='.')
        }
        
        if(exists("MapRef_HR_Cr")){
          MapRef_HR_Cr=rbind(MapRef_HR_Cr, MapRef_HR_CrI)
        } else {     MapRef_HR_Cr=MapRef_HR_CrI }
      }
      
      ## selection MapRef and indices based on desired resolution and constrained area
      # if(exists("MapRef_HR_Cr")){MapRef_HR=MapRef_HR_Cr } ## coding fix V2.3.0?
      if(exists("MapRef_HR_Cr") ){ ## coding fix V2.3.0?
        Names=c(names(MapRef_HR_Cr), "Longitude", "Latitude"); Names
        MapRef_HR_DF=unique(data.frame(MapRef_HR_Cr)[ ,Names]); # summary(MapRef_HR_DF)
        MapRef_HR=MapRef_HR_DF; 
        coordinates(MapRef_HR)=c("Longitude", "Latitude"); proj4string(MapRef_HR)=Proj4;
        # summary(MapRef_HR);
        } ## coding fix V2.3.0?
      
    } 
    
  } ## Spatial Constraints
  
  # str(MapRef_HR_CrI);
  # str(MapRef_HR_Cr);
  # str(MapRef_HR);
  
  if(Fold) {
    if(nrow(Constraints_Fishery)>0) {
      Constraints_Fishery
      if(Constraints_Fishery$Fishery=="EverythingButExempt"){
        GearMap=GearMap[!GearMap$Fishery %in% "Exempt", ]; dim(GearMap) ## drop all data outside constraints
      } else {
        GearMap=GearMap[GearMap$Fishery %in% Constraints_Fishery$Fishery, ]; dim(GearMap) ## drop all data outside constraints
      }
      # table(GearMap$Fishery)
      ## update spatial domain as needed
      ## MapRef_HR=MapRef_HR[MapRef_HR$Index_HR %in% unique(GearMap$Index_HR), ] ## dropped for V2.3.0; causing problems with comparing scenarios
    }
    
  } ## Fishery constraints
  
  # Bk=GearMap; 
  # GearMap=Bk;
  # str(GearMap);
  # str(MapRef_HR);
  # str(data.frame(MapRef_HR, stringsAsFactors=FALSE));
  
  if(Fold){ ## update spatial domain, save some output V2.2.4
    MapRef_LR=MapRef_LR[MapRef_LR$Index_LR %in%  unique(MapRef_HR$Index_LR), ];
    # GearMap=merge(GearMap, data.frame(MapRef_HR)[ ,c("Index_HR", "Index_LR", "Region", "Distance", "SourceZone")])
    GearMap=merge(GearMap, 
                  data.frame(MapRef_HR)[ ,c("Index_HR", "Index_LR", "Region", "SourceZone")]) ## drop Distance field
    GearMap$SourceZone=as.character(GearMap$SourceZone);
    
    if(nrow(GearMap)==0){ 
      message("Error: Spatial Constraints removed all data"); return(ConstraintsSpatial);
    } 
    WhaleDensityModel=merge(WhaleDensityModel, data.frame(MapRef_HR)[ ,c("Index_HR", "Index_LR")]); 
    
    if(HighResolution){
      MapRef=MapRef_HR;
      MapRef$Index=MapRef$Index_HR
      GearMap$Index=GearMap$Index_HR
      WhaleDensityModel$Index = WhaleDensityModel$Index_HR
    } else {
      ## to use the _LR grid, we need to pass the location attributes from _HR to _LR after having cropped _HR to the desired extent
      MapRef=MapRef_LR[ ,c("Index_LR")]; summary(MapRef); 
      
      ## move referece columns from MapRef_HR to MapRef_LR V2.3.0
      MapRefAttributes=c("LMA", "State", "StatArea") ## list of columns to transfer
      MapRef_DF=data.frame(MapRef_HR); summary(MapRef_DF) ## _HR as data frame
      for(AttI in MapRefAttributes){ ## loop across columns
        if(AttI %in% names(MapRef_DF)){ ## if column exists in _HR
          Dat=MapRef_DF[ ,c("Index_HR", "Index_LR", AttI)]; ## extract only needed columns
          names(Dat)=c("Index_HR", "Index_LR", "NextColumn") ## temporarily rename
          nPixels=aggregate(Index_HR~Index_LR+NextColumn, Dat, length); ## get number of pixels by LR cell represented by attribute values
          names(nPixels)=c("Index_LR", "NextColumn", "nPix"); summary(nPixels); ## rename
          maxPixels=aggregate(nPix~Index_LR, nPixels, max); summary(maxPixels); ## find locations that represent the majority of the attribute values
          nPixels=merge(nPixels, maxPixels); summary(nPixels); ## match the retained attribute back to the location
          if(max(aggregate(nPix~Index_LR, nPixels, length)$nPix)>1){ ## if a location is evenly split between two attributes... fix someday
            message("Error: Unable to find unique assignments for LowRes grid. Not sure how to best fix this."); break();
          }
          names(nPixels)=c("Index_LR", "nPix", AttI) ## rename
          MapRef=merge(MapRef, nPixels[ ,c("Index_LR", AttI)], all.x=TRUE) ## append to MapRef
        } ## end update of one column
      } ## end loop across columns
      summary(MapRef)
      MapRef$Index=MapRef$Index_LR;
      
      GearMap$Index=GearMap$Index_LR
      WhaleDensityModel$Index = WhaleDensityModel$Index_LR
    }
    
    ## output monthly whale densities
    WhalesOutput=aggregate(WhaleDensity~Month, WhaleDensityModel, sum); WhalesOutput
    WhalesTotal=aggregate(WhaleDensity~1, WhalesOutput, sum); WhalesTotal
    WhalesTotal$Month="Total"
    WhalesOutput=rbind(WhalesOutput, WhalesTotal); WhalesOutput
    WhalesOutput$Scenario="Default"
    
    WhalesOutput2=WhalesOutput;
    WhalesOutput2$Scenario="Scenario"
    
    WhaleOutput=rbind(WhalesOutput, WhalesOutput2); WhaleOutput
    WhaleOutput$Variable="WhaleDensity"
    WhaleOutput=WhaleOutput[ ,c("Variable", "Scenario", "Month", "WhaleDensity")]; #WhaleOutput
    names(WhaleOutput)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, WhaleOutput); #OutputData
    
    
    
    if(ManageMemory){
      CleanUp(InputList=c("CommentText", "DecisionTool", "InputSpreadsheetName", "ModelVersion", "Version",
                          "AvailableActions", "AvailableBuoylineDevice", "AvailableFisheries", "AvailableLMAs",
                          "AvailableMaxRopeStrength", "AvailableRopelessDevice", "AvailableStatAreas", "AvailableStates", "AvailableStringRegulations",
                          "MapRef_HR_readMe", "ScenarioInputPass", "ShapefileDir", "ThreatString", "GearMap_V2.2.3_readMe", "VesselKey", "WhaleDensityModel_ReadMe",
                          "RopeStrengthModelName", "WhaleInputModel", "WhaleString", "GearMap_Indices", "GearMapName", "MapRef_HR_Cr", "MapRef_HR_CrI",
                          "StatAreasI", "Constraints_Fishery", "Constraints_Spatial", "Constraints_SpatialShape", "ShapeI", "WhaleOutput", "WhalesOutput", "WhalesOutput2",
                          "LineStrengthMod_ReadMe","MonthOrder","RopeStrengthResolution","RopeStrengthString","ThreatModel","GearConversion","GearPerStringModel_Agg"))
      gc();
    }
  } ## update spatial domain, save some output 
  
  par(mar=c(1,1,1,1));
  message(plot(MapRef_HR, pch='.', main="Constrained Spatial Domain"))
  message(plot(spStatAreas, add=TRUE, border="blue"))
  message(plot(spLMAs, add=TRUE, border="green"))
  
  # str(MapRef_HR); 
  # str(MapRef_LR); 
  # str(GearMap);
  
  ## Filled data frame for future plotting
  PlotDF=expand.grid(Index_LR=unique(data.frame(MapRef_LR)$Index_LR),
                     Month=1:12); #summary(PlotDF)
  ##########################################################--
  ## 1.0 Gear Reductions
  if(Fold) {
    message("1. Applying any Gear reductions")
    ## Gear are further removed due to Gear reductions (management option #2). 
    ## Easiest assumption is that Gear will be removed proportionally over the entire management area.
    Stage1d=GearMap[GearMap$GearFished>0, ]; 
    #   ############### Stage 1 Default ######################################################--
    if(PrintDefaultMaps){ 
      OutputMapCall=OutputMap(
        PlotDF=PlotDF,
        MapRef=MapRef_LR,
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Stage1d,
        VarName="GearFished",
        Plot1_Title="Aggregate Gear Density - Post Reduction - Default",
        PlotLog=TRUE,
        LogOffset=1,
        Plot2_Title="Aggregate Gear Density, log-scaled, Post Reduction - Default"
      )
      
      Stage1d_GearDensity_Px=OutputMapCall$Plot1_Px;
      map1dGearDensity=OutputMapCall$Map1;
      Stage1d_GearDensityLog_Px=OutputMapCall$Plot1_Px
      map1dGearDensityLog=OutputMapCall$Map2
    } ## stage 1 default maps
    
    if(TestScenario){
      
      Stage1s=GearMap[GearMap$GearFished>0, ]; 
      
      if(nrow(SC_GearReductions)>0){
        for(i in 1:nrow(SC_GearReductions)){
          ## 
          ## note Gear reductions are always done on high resolution
          MapRef_HR_I=MapRef_HR;
          if(!is.na(SC_GearReductions$LMA[i])) {
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$LMA==SC_GearReductions$LMA[i], ]
          } 
          if(!is.na(SC_GearReductions$State[i])) {
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$State==SC_GearReductions$State[i], ]
          } 
          if(!is.na(SC_GearReductions$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_GearReductions$StatArea[i], ",")[[1]])
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_GearReductions$Shapefile[i])) {
            SC_GearReductionsShape=SC_GearReductions$Shapefile[i]; #SC_GearReductionsShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_GearReductionsShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            ReductionPx=MapRef_HR$Index_HR[!is.na(over(MapRef_HR, ShapeI)$ID)] ## get spatial overlap of shapefile from overlay
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$Index_HR %in% ReductionPx, ]
          }
          
          #summary(MapRef_HR_I)
          ## applicable months
          if(!is.na(SC_GearReductions$Months[i])){
            message("Really, you're performing a seasonal Gear reduction?")
            Months=as.numeric(strsplit(SC_GearReductions$Months[i], ",")[[1]])
          } else {Months=1:12}
          
          Stage1s$GearFished[Stage1s$Index_HR %in% MapRef_HR_I$Index_HR &
                                Stage1s$Month %in% Months]=
            Stage1s$GearFished[Stage1s$Index_HR %in% MapRef_HR_I$Index_HR &
                                  Stage1s$Month %in% Months] * (1-(SC_GearReductions$Percentage[i])) ## apply reduction
          # aggregate(GearFished~StatArea, GearMap, sum)
        }
        
      } ## perform Gear reductions
      
      
      ############### Stage 1 Scenario ######################################################--
      if(PrintScenarioMaps){
        OutputMapCall=OutputMap(
          PlotDF=PlotDF,
          MapRef=MapRef_LR,
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Stage1s,
          VarName="GearFished",
          Plot1_Title="Aggregate Gear Density - Post Reduction - Scenario",
          PlotLog=TRUE,
          LogOffset=1,
          Plot2_Title="Aggregate Gear Density, log-scaled, Post Reduction - Scenario"
        )
        
        Stage1s_GearDensity_Px=OutputMapCall$Plot1_Px;
        map1sGearDensity=OutputMapCall$Map1;
        Stage1s_GearDensityLog_Px=OutputMapCall$Plot1_Px
        map1sGearDensityLog=OutputMapCall$Map2
        
      } ## Stage1 Scenario Maps
    }    
    
    #########################--
    Stage1dOutput=aggregate(GearFished~Month, Stage1d, sum); Stage1dOutput
    Stage1dTotal=aggregate(GearFished~1, Stage1dOutput, sum); Stage1dTotal
    Stage1dTotal$Month="Total"
    Stage1dOutput=rbind(Stage1dOutput, Stage1dTotal); Stage1dOutput
    Stage1dOutput$Scenario="Default"
    
    if(TestScenario){
      Stage1sOutput=aggregate(GearFished~Month, Stage1s, sum); Stage1sOutput
      Stage1sTotal=aggregate(GearFished~1, Stage1sOutput, sum); Stage1sTotal
      Stage1sTotal$Month="Total"
      Stage1sOutput=rbind(Stage1sOutput, Stage1sTotal); Stage1sOutput
      Stage1sOutput$Scenario="Scenario"
      
      Stage1Output=rbind(Stage1dOutput, Stage1sOutput); Stage1Output
    } else {
      Stage1Output=Stage1dOutput
    }
    
    Stage1Output$Variable="GearFished_PostReduction"
    Stage1Output=Stage1Output[ ,c("Variable", "Scenario", "Month", "GearFished")]; #Stage1Output
    names(Stage1Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage1Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("GearMap", "MapRef_HR_I", "ShapeI", "SC_GearReductions","SC_GearReductionsShape", "ReductionPx", "Months", "OutputMapCall",
                          "Stage1dOutput", "Stage1dTotal", "Stage1sOutput", "Stage1sTotal", "Stage1Output","Stage1s_GearDensity_Px", "Stage1s_GearDensityLog_Px")); gc();}
    
  } ## fold Gear reductions
  
  ##########################################################--
  ## 2.0 Gear Caps
  if(Fold) {
    message("2. Applying any Gear caps")
    ## Gear are further removed due to implementation of Gear caps (management option #3?). 
    ## Cumulative number of Gear fished by region are cropped to new Gear cap and change in area-under-curve is applied to number of Gear fished.
    Stage2d=Stage1d
    #   ############### Stage 2 Default ######################################################--
    if(PrintDefaultMaps){ 
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Stage2d,
        VarName="GearFished",
        Plot1_Title="Aggregate Gear Density - Post GearCaps - Default",
        PlotLog=TRUE,
        LogOffset=1,
        Plot2_Title="Aggregate Gear Density, log-scaled, Post GearCaps - Default"
      )
      
      Stage2d_GearDensity_Px=OutputMapCall$Plot1_Px;
      map2dGearDensity=OutputMapCall$Map1;
      Stage2d_GearDensityLog_Px=OutputMapCall$Plot1_Px
      map2dGearDensityLog=OutputMapCall$Map2
      
    } ## stage 1 default maps
    
    if(TestScenario){
      Stage2s=Stage1s
      
      if(nrow(SC_GearCaps)>0){
        for(i in 1:nrow(SC_GearCaps)){
          ## 
          ## note Gear caps are always done on high resolution
          MapRef_HR_I=MapRef_HR;
          if(!is.na(SC_GearCaps$LMA[i])) {
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$LMA==SC_GearCaps$LMA[i], ]
          } 
          if(!is.na(SC_GearCaps$State[i])) {
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$State==SC_GearCaps$State[i], ]
          } 
          if(!is.na(SC_GearCaps$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_GearCaps$StatArea[i], ",")[[1]])
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_GearCaps$Shapefile[i])) {
            SC_GearCapsShape=SC_GearCaps$Shapefile[i]; SC_GearCapsShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_GearCapsShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            ReductionPx=MapRef_HR$Index_HR[!is.na(over(MapRef_HR, ShapeI)$ID)] ## get spatial overlap of shapefile from overlay
            MapRef_HR_I=MapRef_HR_I[MapRef_HR_I$Index_HR %in% ReductionPx, ]
          }
          
          #summary(MapRef_HR_I)
          ## applicable months
          if(!is.na(SC_GearCaps$Months[i])){
            message("Interesting, you're applying a seasonal Gear cap?")
            Months=as.numeric(strsplit(SC_GearCaps$Months[i], ",")[[1]])
          } else {Months=1:12}
          
          ## split into areas affected and unaffected by the caps
          Capped=Stage2s[Stage2s$Index_HR %in% MapRef_HR_I$Index_HR & 
                           Stage2s$Month %in% Months, ]; #dim(Capped) ## areas and months affected by Gear cap
          NotCapped=Stage2s[!Stage2s$Index_HR %in% MapRef_HR_I$Index_HR |
                              !Stage2s$Month %in% Months, ]; #dim(NotCapped) ## ## areas and months not affected by Gear cap
          
          ## find shapefile regions affected by the Gear cap by overlay
          spCapped=unique(Capped[ ,c("Index_HR", "x", "y")]); 
          coordinates(spCapped)=c("x", "y"); proj4string(spCapped)=Proj4
          spCapped$RegionID=over(spCapped, GearCap_Regions)$RegionID; 
          if(any(is.na(spCapped$RegionID))){ ## error check
            message(paste("Error; Gear cap", i, "extends beyond the area where latent effort has been characterized")); return(SC_GearCaps[i, ]);
          }
          
          ## get affected regions as merge of regionID and months
          AffectedRegions=merge(data.frame(RegionID=unique(spCapped$RegionID)),
                                data.frame(Month=Months)); #summary(AffectedRegions)
          AffectedRegions=merge(AffectedRegions, GearCapCDF, all.x=TRUE); #summary(AffectedRegions) ## bring in data
          AffectedRegions$PostCap=AffectedRegions$GearFished; ## create copy of Gear fished
          AffectedRegions$PostCap[AffectedRegions$PostCap>SC_GearCaps$GearCap[i]]=SC_GearCaps$GearCap[i] ## threshold Gear fished at designated Gear cap
          FishedTotals=aggregate(GearFished~RegionID+Month, AffectedRegions, sum); names(FishedTotals)=c("RegionID", "Month", "TotalFished") ## sum Gear across quantiles
          CappedTotals=aggregate(PostCap~RegionID+Month, AffectedRegions, sum); names(CappedTotals)=c("RegionID", "Month", "TotalCapped") ## sum capped Gear across quantiles
          Totals=merge(FishedTotals, CappedTotals); #summary(Totals) ## merge Gear totals
          Totals$Reduction=with(Totals, (TotalFished - TotalCapped)/TotalFished) ## calculate reduction
          
          ## merge back to indices for individual pixels
          AffectedPx=merge(data.frame(spCapped)[ ,c("Index_HR", "RegionID")],
                           Totals[ ,c("RegionID", "Month", "Reduction")]); #summary(AffectedPx)
          Capped=merge(Capped, AffectedPx, all.x=TRUE); #summary(Capped)
          if(any(is.na(Capped$Reduction))){
            message(paste("Error; Gear cap", i, "; some regions not matched to a reduction")); return(Capped);
          }
          ## calculate Gear removed
          GearRemoved_I=Capped; GearRemoved_I$GearRemoved=with(GearRemoved_I, GearFished * Reduction); #summary(GearRemoved_I)
          if(i==1){ ## rbind across iterations
            GearRemoved=GearRemoved_I
          } else {
            GearRemoved=rbind(GearRemoved, GearRemoved_I)
          }
          
          ## calculate Gear retained in affected areas
          GearRetained=Capped; GearRetained$GearFished=GearRetained$GearFished * (1-GearRetained$Reduction); #summary(GearRetained)
          Stage2s=rbind(NotCapped, GearRetained[ ,names(NotCapped)]); #dim(Stage2s) ## rbind back together to Stage2s to feed through next iteration
        }
        
      } ## perform Gear caps
      
      
      ############### Stage 2 Scenario ######################################################--
      if(PrintScenarioMaps){
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Stage2s,
          VarName="GearFished",
          Plot1_Title="Aggregate Gear Density - Post GearCaps - Scenario",
          PlotLog=TRUE,
          LogOffset=1,
          Plot2_Title="Aggregate Gear Density, log-scaled, Post GearCaps - Scenario"
        )
        
        Stage2s_GearDensity_Px=OutputMapCall$Plot1_Px;
        map2sGearDensity=OutputMapCall$Map1;
        Stage2s_GearDensityLog_Px=OutputMapCall$Plot1_Px
        map2sGearDensityLog=OutputMapCall$Map2
        
        
      } ## Stage2 Scenario Maps
      
      ############### Gear Cap reductions ######################################################--
      if(nrow(SC_GearCaps)>0){ ## print GearCap Gear reduction maps
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=GearRemoved,
          VarName="GearRemoved",
          Plot1_Title="Total Gear Removed by Gear Caps",
          PlotLog=TRUE,
          LogOffset=1,
          Plot2_Title="Total Gear Removed by Gear Caps, log-scaled"
        )
        
        GearRemoved_Px=OutputMapCall$Plot1_Px;
        map2sGearRemoved=OutputMapCall$Map1;
        GearRemovedLog_Px=OutputMapCall$Plot1_Px
        map2sGearRemovedLog=OutputMapCall$Map2
        
        #############--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=GearRemoved,
          VarName="Reduction",
          Plot1_Title="Percent Gear Removed by Gear Caps"
        )
        
        GearCapPercentRemoved_Px=OutputMapCall$Plot1_Px;
        map2sGearCapPercentRemoved=OutputMapCall$Map1;
        
      } ## Stage2 Gear Reduced By GearCap Maps
    }
    
    #########################--
    Stage2dOutput=aggregate(GearFished~Month, Stage2d, sum); Stage2dOutput
    Stage2dTotal=aggregate(GearFished~1, Stage2dOutput, sum); Stage2dTotal
    Stage2dTotal$Month="Total"
    Stage2dOutput=rbind(Stage2dOutput, Stage2dTotal); Stage2dOutput
    Stage2dOutput$Scenario="Default"
    
    if(TestScenario){
      Stage2sOutput=aggregate(GearFished~Month, Stage2s, sum); Stage2sOutput
      Stage2sTotal=aggregate(GearFished~1, Stage2sOutput, sum); Stage2sTotal
      Stage2sTotal$Month="Total"
      Stage2sOutput=rbind(Stage2sOutput, Stage2sTotal); Stage2sOutput
      Stage2sOutput$Scenario="Scenario"
      
      Stage2Output=rbind(Stage2dOutput, Stage2sOutput); Stage2Output
    } else {
      Stage2Output=Stage2dOutput
    }
    
    Stage2Output$Variable="GearFished_PostCap"
    Stage2Output=Stage2Output[ ,c("Variable", "Scenario", "Month", "GearFished")]; #Stage2Output
    names(Stage2Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage2Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall", "MapRef_HR_I", "Months", "SC_GearCaps", "StatAreasI", "SC_GearCapsShape", "ShapeI", "ReductionPx", "Capped", 
                          "NotCapped", "spCapped", "AffectedRegions", "FishedTotals", "CappedTotals", "Totals", "AffectedPx", "GearCap_Regions","GearRemoved_I", 
                          "GearRemoved", "GearRetained", "Stage2dOutput", "Stage2dTotal", "Stage2sTotal", "Stage2sOutput", "Stage2Output","GearCapCDF","Stage2s_GearDensity_Px",
                          "Stage2s_GearDensityLog_Px"))
      gc();
    };
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage1d", "Stage1s"))};
    
  } ## fold Gear capss
  
  ##########################################################--
  ## 3.0 Closures
  if(Fold) {
    message("3. Applying any closures");
    ## Gear are removed or redistributed based on the locations and timing of seasonal closures 
    ## (management option #1 above). 
    ## We would need to consider a rough approach to how to model redistributing Gear. 
    # if(nrow(Stage2d)==
    #    nrow(unique(Stage2d[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "Distance", "VesselKey", "SourceZone")]))){
    #   Stage3d=Stage2d[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "Distance", "VesselKey", "SourceZone", "GearFished")]
    # } else {Stage3d=aggregate(GearFished~Region+Index+Index_LR+Month+Fishery+Distance+VesselKey+SourceZone, Stage2d, sum);    }
    
    if(nrow(Stage2d)==
       nrow(unique(Stage2d[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "VesselKey", "SourceZone")]))){
      Stage3d=Stage2d[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "VesselKey", "SourceZone", "GearFished")]
    } else {Stage3d=aggregate(GearFished~Region+Index+Index_LR+Month+Fishery+VesselKey+SourceZone, Stage2d, sum);    }
    
    
    if(TestScenario){
      
      if(nrow(Stage2s)==
         nrow(unique(Stage2s[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "VesselKey", "SourceZone")]))){
        Stage3s=Stage2s[ ,c("Region", "Index", "Index_LR", "Month", "Fishery", "VesselKey", "SourceZone", "GearFished")]
      } else { Stage3s=aggregate(GearFished~Region+Index+Index_LR+Month+Fishery+VesselKey+SourceZone, Stage2s, sum);}
      
      if(nrow(SC_Closures)>0){
        ## things get complex here. Eventually, the existing Gear (Stage3s) get divided into multiple categories:
        ## RedistributedGear - Gear that were moved to new locations as a result of a closure
        ## RemovedGear - Gear that were removed as the result of a closure where no redistribution was possible
        ## UnmovedGear - Gear that are "left behind" in a closure due to a partial (month) closure; combined with Unaffected Gear at the end
        ## 
        
        ## build some empty data frames for catching outputs at different levels    
        Stage3Names=names(Stage3s)
        
        BlankDF=Stage3s[0, ];
        MovedGear=BlankDF;
        UnmovedGear=BlankDF;
        RemovedGear=BlankDF;
        RedistributedGear=BlankDF;
        
        ## First find set of Gear that will be moved #######################################################--
        ## this is done across all closures here to ensure Gear don't get moved into another proposed closure
        for(i in 1:nrow(SC_Closures)){ ## for each closure
          message(paste("Defining Closure", i))
          
          ## define the area affected by the closure
          ClosureShape=SC_Closures$Shapefile[i]; ClosureShape ## get name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=ClosureShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field in the shapefile
          ShapeI=spTransform(ShapeI, Proj4)
          ClosedPx=MapRef[!is.na(over(MapRef, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          # plot(MapRef); points(ClosedPx, pch=16, col="blue")
          
          ## break model if no indices within polygon V3.0.1
          if(nrow(ClosedPx)==0){
            message(paste0("Error: No Reference Points Found Within Closure Shape ", ClosureShape));
            message("Check Location of Polygon or Consider Running at Higher Resolution");
            break()
          };
          
          ## define the months applicable to the closure
          if(!is.na(SC_Closures$Months[i])){
            ClosureMonthsI=as.numeric(strsplit(SC_Closures$Months[i], ",")[[1]]); ClosureMonthsI ## get months
          } else {ClosureMonthsI=1:12}; ## or use all months
          
          ## find Gear within bounds and months of closure
          AffectedGearI=Stage3s[Stage3s$Index %in% ClosedPx$Index & ## Affected Gear
                                   Stage3s$Month %in% ClosureMonthsI, ];

          ## Gear moved by partial or complete closures
          MovedColocatedGear=AffectedGearI;
          MovedColocatedGear$GearFished= 
            MovedColocatedGear$GearFished * (SC_Closures$Percentage[i]); ## AffectedGear * the percent affected
          MovedGear=rbind(MovedGear, MovedColocatedGear);
          
          ## Gear not moved due to partial closures
          if(SC_Closures$Percentage[i]<1){
            UnmovedColocatedGear=AffectedGearI;
            UnmovedColocatedGear$GearFished= 
              UnmovedColocatedGear$GearFished * (1-SC_Closures$Percentage[i]); ## AffectedGear less the percent removed
            UnmovedGear=rbind(UnmovedGear, UnmovedColocatedGear);
          }
          
          ## check math
          # sum(MovedColocatedGear$GearFished)+sum(UnmovedColocatedGear$GearFished);
          # sum(AffectedGearI$GearFished);
          
        }; ## end Define set of Gear that will be moved #######################################################--
        
        ## make sure nothing got double counted
        MovedGear=unique(MovedGear) ## remove any double-counted Gear from overlapping closures
        
        ## clean up; no need to retain vessel class, etc.
        MovedGearAgg=aggregate(GearFished~Index+Fishery+SourceZone+Month, MovedGear, sum); #summary(MovedGearAgg);

        UnmovedGear=unique(UnmovedGear) ## add these back into UnAffected Gear at end of Stage3
        
        ## remove AffectedGear from Gear Pool, leaving UnAffectedGear
        RemovedPx=unique(MovedGearAgg[ ,c("Index", "Month")]);
        
        RemovedPx$Remove=TRUE ## V3.0.2
        GearStatus=merge(Stage3s, RemovedPx, all.x=TRUE); #summary(GearStatus);## V3.0.2
        UnAffectedGear=GearStatus[is.na(GearStatus$Remove), names(Stage3s)]; #summary(UnAffectedGear); ## V3.0.2

        ## V3.0.2
        # UnAffectedGear=Stage3s[
        #   -which(Stage3s$Index %in% RemovedPx$Index &
        #            Stage3s$Month %in% RemovedPx$Month),
        #   ]; summary(UnAffectedGear);
        
        ## check math
        # sum(UnAffectedGear$GearFished)+
        #   sum(MovedGearAgg$GearFished) +
        #   sum(UnmovedGear$GearFished)
        # sum(Stage3s$GearFished);
        
        ## For each Fishery / SourceZone, redistribute Gear #######################################--
        for(i in 1:nrow(SC_Closures)){ ## loop across closures (I)
          message(paste("Analyzing closure", i, "of", (nrow(SC_Closures)), ";", SC_Closures$Shapefile[i]))
          RedistributedGearI=BlankDF;
          
          ## define the area affected by the closure
          ClosureShape=SC_Closures$Shapefile[i]; ClosureShape ## name of shapefile
          ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=ClosureShape, verbose=FALSE) ## load shapefile
          ShapeI$ID=1 ## create a known field 
          ShapeI=spTransform(ShapeI, Proj4)
          ClosedPx=MapRef[!is.na(over(MapRef, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          #summary(ClosedPx)
          
          ## get months that closure is active
          if(!is.na(SC_Closures$Months[i])){
            ClosureMonthsI=as.numeric(strsplit(SC_Closures$Months[i], ",")[[1]]); ClosureMonthsI ## get months
          } else {ClosureMonthsI=1:12}; ## or use all months
          
          # sum(MovedGearAgg$GearFished);
          
          ## get Gear to be moved in this iteration
          MovedGearI=MovedGearAgg[MovedGearAgg$Index %in% ClosedPx$Index &
                                   MovedGearAgg$Month %in% ClosureMonthsI, ]; #summary(MovedGearAggI)
          
          ## deduct these from Moved Gear in each iteration; this should draw down to zero records by end of iterations
          MovedGearAgg=MovedGearAgg[-which(MovedGearAgg$Index %in% ClosedPx$Index &
                                         MovedGearAgg$Month %in% ClosureMonthsI), ]; #summary(MovedGearAggI)
          
          ## check math
          # sum(MovedGearAgg$GearFished)+sum(MovedGearI$GearFished);
          
          ## if non-zero Gear redistributed
          if(sum(MovedGearI$GearFished)>0){
            ## get a list of affected fisheries, locations and months to locate adjacent Gear
            MovedFleetZoneMonthList = unique(MovedGearI[ ,c("Fishery", "SourceZone", "Month")]) ## lists including Month
            MovedFleetZoneList = unique(MovedFleetZoneMonthList[ ,c("Fishery", "SourceZone")]) ## lists excluding Month
            
            ## for each combination of fishery, and zone
            for(j in 1:nrow(MovedFleetZoneList)){ ## loop across fishery and zone (J) 
              message(paste("Relocating Gear for Closure", ClosureShape, "Subarea", j, "of", nrow(MovedFleetZoneList), sep=" "))
              
              ## Identify lists of Gear to move by fleet and zone
              MovedFleetZoneListJ=MovedFleetZoneList[j, ] ## start with fishery, zone
              MovedFleetZoneMonthListJ=merge(MovedFleetZoneMonthList, MovedFleetZoneListJ); ## add in applicable months
              
              ## select set of Gear moved in this fishery, location, and month
              MovedGearSetJ=merge(MovedGearI, MovedFleetZoneListJ); #summary(MovedGearSetJ)
              ## pick up coordinates needed for distance calculations below
              MovedGearSetJ=merge(MovedGearSetJ, data.frame(ClosedPx)[ ,c("Longitude", "Latitude", "Index")], all.x=TRUE); #summary(MovedGearSetJ)
              names(MovedGearSetJ)[names(MovedGearSetJ)=="GearFished"]="GearToMove" ## rename to avoid confusion below
              # summary(MovedGearSetJ); ## this is the set of Gear to move in this iteration
              
              ## create spatial objects for distance calculations to adjacent Gear
              spMovedGearSetJ=unique(MovedGearSetJ[ ,c("Index", "Longitude", "Latitude")]); 
              coordinates(spMovedGearSetJ)=c("Longitude", "Latitude");
              
              ## Now select Adjacent areas where Gear can be moved to, based on fleet, location, and adjacency
              AdjacentGearIndexJ=merge(MovedFleetZoneMonthListJ, ZoneAdjacency); AdjacentGearIndexJ
              # AdjacentGearIndexJ=AdjacentGearIndexJ[ ,c("SinkZone", "Fishery", "Distance", "Month")]
              AdjacentGearIndexJ=AdjacentGearIndexJ[ ,c("SinkZone", "Fishery", "Month")]
              names(AdjacentGearIndexJ)[names(AdjacentGearIndexJ)=="SinkZone"]="SourceZone" ## rename to allow merging below
              AdjacentGearJ=merge(UnAffectedGear, AdjacentGearIndexJ); #summary(AdjacentGearJ)
              # summary(AdjacentGearJ)
              
              RedistributedGearJ=BlankDF; ## start loop below with clean DF
              
              ## redistribute Gear if possible
              if(nrow(AdjacentGearJ)==0 | sum(AdjacentGearJ$GearFished)==0){ ## if no adjacent Gear
                message(paste("Note: No adjacent habitat found for", 
                            MovedFleetZoneListJ$Fishery, "fishery in ",
                            MovedFleetZoneListJ$SourceZone, "source area. Removing Gear due to closure"))
                
                ## recover some lost columns
                RemovedGearJ=merge(unique(MovedGearSetJ[ ,c("Index", "Fishery", "Month")]), 
                                    Stage3s[ ,c("Index", "Fishery", "Month", "Region", "Index_LR", "VesselKey", "SourceZone", "GearFished")])
                RemovedGearJ=RemovedGearJ[ ,Stage3Names];
                RemovedGear=rbind(RemovedGear, RemovedGearJ) ## if no adjacent area, remove Gear
                
              } else {
                
                AdjacentGearJ=merge(AdjacentGearJ, 
                                     data.frame(MapRef)[ ,c("Longitude", "Latitude", "Index")]); ## add coordinates for distance calculations
                spAdjacentGearJ=unique(AdjacentGearJ[ ,c("Index", "Longitude", "Latitude")]); ## get unique set, used repeatedly in loop below 
                coordinates(spAdjacentGearJ)=c("Longitude", "Latitude"); 
                
                # plot(spAdjacentGearJ); points(spMovedGearSetJ, pch=16) ## useful check for visualization / debugging
                
                
                ## this is a short-cut to speed up Gear redistribution at some cost of spatial resolution
                ## particularly not recommended if there is high resolution and spatial hetergeneity in Gear distributions
                ## below scales of zone and within closure areas.
                ## may also perform strangly for strongly irregularly shaped closures (i.e. not minimum convex polygons)
                if(ExpressRedistribution){
                  ## get the proportion of Gear represented by each location across the year
                  MovedGearSetJ$TotalGear=sum(MovedGearSetJ$GearToMove);
                  MovedGearSetJ$PropGear=with(MovedGearSetJ, GearToMove/TotalGear);

                  ## use these proportions to weight spatial coordinates
                  MovedGearSetJ$PropLat=with(MovedGearSetJ, Latitude*PropGear);
                  MovedGearSetJ$PropLong=with(MovedGearSetJ, Longitude*PropGear);
                  MovedGearSetJ$PropIndex=with(MovedGearSetJ, Index*PropGear);

                  ## average coordinates to get mean Gear location within closure
                  spMovedGearSetJ=aggregate(cbind(PropIndex, PropLat, PropLong)~1, MovedGearSetJ, sum); 
                  names(spMovedGearSetJ)=c("Index", "Latitude", "Longitude");

                  ## get monthly total Gear and merge with new coordinates
                  MovedGearSetJ=aggregate(GearToMove~Fishery+SourceZone+Month, MovedGearSetJ, sum);
                  MovedGearSetJ=merge(spMovedGearSetJ, MovedGearSetJ); MovedGearSetJ
                  
                  coordinates(spMovedGearSetJ)=c("Longitude", "Latitude"); ## make spatial object
                  
                  ## now loop below (for k in UnqSources) will only run once
                }
                
                ############# Redistribute Gear to adjacent areas #######################################--
                ## this loop runs for each pixel located within the closure, zone, and fleet
                UnqSources=unique(spMovedGearSetJ$Index); length(UnqSources); ## get list of pixel indices (source locations)
                
                for(k in UnqSources){ ## loop across Source locations (K)
                  # message(paste("Redistributing Gear for ", k))
                  # k=UnqSources[1] ## for debugging
                  MovedGearSetJK=MovedGearSetJ[MovedGearSetJ$Index==k, ] ## Gear to be moved in this iteration
                  spSourceK=spMovedGearSetJ[spMovedGearSetJ$Index==k , ] ## location where Gear are coming from
                  
                  ## calculate distances between select location inside closure and all adjacent Gear
                  AdjacentGearJK=merge(
                    AdjacentGearJ, ## adjacent Gear
                    data.frame(Index=spAdjacentGearJ$Index,
                               DistanceToMove=spDistsN1(spAdjacentGearJ, spSourceK, longlat=TRUE) ## distance from closure site to adjacent sites
                    )
                  ); # summary(AdjacentGearJK)
                  
                  ## rename to track original Gear vs Gear being moved to location
                  names(AdjacentGearJK)[names(AdjacentGearJK)=="GearFished"]="OrigGearFished"
                  
                  ## this is literally a benefit:cost ratio 
                  AdjacentGearJK$Benefit2Cost= ## benefit for moving Gear estimated from
                    AdjacentGearJK$OrigGearFished / ## density of Gear at location (proxy for habitat quality, catch rate, etc.)
                    (AdjacentGearJK$DistanceToMove ^ RelocationCostExp)  ## offset by distance Gear have to be moved
                  ## area for potential improvement in future
                  
                  ## get total monthly weights for standardizing and proportioning
                  Benefit2CostByMonth=aggregate(Benefit2Cost~Month, AdjacentGearJK, sum); names(Benefit2CostByMonth)=c("Month", "TotalBenefit2Cost");
                  AdjacentGearJK=merge(AdjacentGearJK, Benefit2CostByMonth); #summary(AdjacentGearJK)
                  
                  ## get proportion of moved Gear that should be applied to a given location / model vessel
                  AdjacentGearJK$RedistProp=AdjacentGearJK$Benefit2Cost / AdjacentGearJK$TotalBenefit2Cost 
                  # summary(AdjacentGearJK)
                  
                  ## visualize redistribution by distance and Gear density
                  # plot(RedistProp~DistanceToMove, AdjacentGearJK, pch=1, cex=sqrt(AdjacentGearJK$OrigGearFished)/10)
                  
                  ## now add in the total number of Gear to redistribute
                  AdjacentGearJK=merge(AdjacentGearJK, MovedGearSetJK[ ,c("Month", "GearToMove")]);
                  AdjacentGearJK$GearFished=with(AdjacentGearJK, RedistProp * GearToMove) ## calculate Gear move to each location and gear config
                  
                  ## check math   
                  # sum(AdjacentGearJK$GearFished);
                  # sum(MovedGearSetJK$GearToMove)
                  
                  ## lose unnecessary columns
                  AdjacentGearJK=AdjacentGearJK[ ,Stage3Names]; summary(AdjacentGearJK)
                  
                  # sum(MovedGearSetJK$GearToMove)
                  # sum(AdjacentGearJK$GearFished)
                  
                  ## compile results
                  RedistributedGearJ=rbind(RedistributedGearJ, AdjacentGearJK)
                  
                  # message(dim(RedistributedGearJ))
                } ## end loop across source location (K)
                
                ## clean up redundancy
                RedistributedGearJ=aggregate(GearFished~Region+Index+Index_LR+Month+Fishery+VesselKey+SourceZone,
                                              RedistributedGearJ, sum);
                
                # sum(RedistributedGearJ$GearFished)
                # sum(MovedGearSetJ$GearToMove)
                
                ## append to larger data set
                RedistributedGearI=rbind(RedistributedGearI, RedistributedGearJ)
              } ## end redistribute Gear if possible for a source zone and fishery
              
            } ## end loop across fishery and zone (J) within a closure
            
          } ## end if non-zero Gear redistributed
          
          ## clean up redundancy
          RedistributedGearI=aggregate(GearFished~Region+Index+Index_LR+Month+Fishery+VesselKey+SourceZone,
                                        RedistributedGearI, sum);
          
          ## append to larger data set
          RedistributedGear=rbind(RedistributedGear, RedistributedGearI)
          ## check math
          # dim(RedistributedGearJK); sum(RedistributedGearJK$GearFished); sum(MovedGearSetJ$GearToMove)
        } ## end loop across each closures (I)
        
      ## final check math      
      StartGear=sum(Stage3s$GearFished); StartGear ## how many Gear you started with
      EndGear=    sum(UnAffectedGear$GearFished) +
        sum(RedistributedGear$GearFished) + 
        sum(RemovedGear$GearFished) +
        sum(UnmovedGear$GearFished); EndGear ## how many Gear you ended with
      
      if(abs(StartGear-EndGear)/StartGear>0.05) {
        message("Error; Greater than 5% of Gear unaccounted for in redistribution around closures; Ending Scenario Run")
        return()
      }
      
      ## rbind everything back together
      Stage3s=rbind(UnAffectedGear, UnmovedGear, RedistributedGear)
      
      if(PrintRedistributionMaps & sum(RedistributedGear$GearFished>0)){
        ###################### Stage3 Scenario ########################################################--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=RedistributedGear,
          VarName="GearFished",
          Plot1_Title="Redistributed Gear Density from Closures",
          PlotLog=TRUE,
          LogOffset=.01,
          Plot2_Title="Redistributed Gear Density from Closures, Log-Scaled"
        )
        
        Redist_GearDensity_Px=OutputMapCall$Plot1_Px;
        map3sRedistributedGearDensity=OutputMapCall$Map1;
        Redist_GearDensityLog_Px=OutputMapCall$Plot1_Px
        map3sRedistributedGearDensityLog=OutputMapCall$Map2
        
        ##############################################################################--
        if(nrow(RemovedGear)==0) {
          message("No Gear removals associated with closures; No Gear removal maps produced")
        } else {
          
          OutputMapCall=OutputMap(         
            PlotDF=PlotDF,         
            MapRef=MapRef_LR,         
            Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
            InputDF=RemovedGear,
            VarName="GearFished",
            Plot1_Title="Gear Removals associated with Closures",
            PlotLog=TRUE,
            LogOffset=1,
            Plot2_Title="Gear Removals associated with Closures, Log-Scaled"
          )
          
          GearRemov_GearDensity_Px=OutputMapCall$Plot1_Px;
          map3sGearRemovedGearDensity=OutputMapCall$Map1;
          GearRemov_GearDensityLog_Px=OutputMapCall$Plot1_Px
          map3sGearRemovedGearDensityLog=OutputMapCall$Map2
          
        }
        
      } ## Gear redistribution maps
      
    } ## end Gear redistribution
      
    } ## end TestScenario V3.0.1
    
    ###################### Stage3 Scenario ########################################################--
    OutputMapCall=OutputMap(         
      PlotDF=PlotDF,         
      MapRef=MapRef_LR,         
      Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
      InputDF=Stage3s,
      VarName="GearFished",
      Plot1_Title="Post-Closure Gear Density - Scenario",
      PlotLog=TRUE,
      LogOffset=1,
      Plot2_Title="Post-Closure Gear Density, Log-Scaled - Scenario"
    )
    
    Stage3s_GearDensity_PostClosure_Px=OutputMapCall$Plot1_Px;
    map3sGearDensity_PostClosure=OutputMapCall$Map1;
    Stage3s_GearDensity_PostClosureLog_Px=OutputMapCall$Plot1_Px
    map3sGearDensity_PostClosureLog=OutputMapCall$Map2
  
    #########################################################--
    Stage3dOutput=aggregate(GearFished~Month, Stage3d, sum); Stage3dOutput
    Stage3dTotal=aggregate(GearFished~1, Stage3dOutput, sum); Stage3dTotal
    Stage3dTotal$Month="Total"
    Stage3dOutput=rbind(Stage3dOutput, Stage3dTotal); Stage3dOutput
    Stage3dOutput$Scenario="Default"
    
    if(TestScenario){
      Stage3sOutput=aggregate(GearFished~Month, Stage3s, sum); Stage3sOutput
      Stage3sTotal=aggregate(GearFished~1, Stage3sOutput, sum); Stage3sTotal
      Stage3sTotal$Month="Total"
      Stage3sOutput=rbind(Stage3sOutput, Stage3sTotal); Stage3sOutput
      Stage3sOutput$Scenario="Scenario"
      
      Stage3Output=rbind(Stage3dOutput, Stage3sOutput); Stage3Output
    } else {
      Stage3Output=Stage3dOutput
    }
    
    Stage3Output$Variable="GearFished_PostClosure"
    Stage3Output=Stage3Output[ ,c("Variable", "Scenario", "Month", "GearFished")]; #Stage3Output
    names(Stage3Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage3Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall", "MapRef_HR_I", "Months", "RemovedGear", "RedistributedGear", "ClosureShape", "ShapeI", 
                          "ClosedPx", "ClosureMonthsI", "UnmovedGear", "AffectedPx", "GearMonthSetsIndices", 
                          "GearSetsIndices", "GearSetsIndexJ", "GearMonthSetsIndicesJ", "GearSetJ", "AdjacentSetIndexJ", "AdjacentSetJ", 
                          "GearToMove", "spGearSetJ", "NumPts", "subspGearSetJ", "InteriorDist", "spAdjacentSetJ", "ExteriorDist", 
                          "spAdjacentSetJ", "AdjacentSetJ_2", "GearWtByMonth", "Tmp", "StartGear", "EndGear", "Stage3dOutput", "Stage3dTotal", 
                          "Stage3sOutput", "Stage3sTotal", "Stage3Output", "SC_Closures","ZoneAdjacency", "Stage3s_GearDensity_PostClosure_Px", 
                          "Stage3s_GearDensity_PostClosureLog_Px",
                          "RemovedPx", "GearStatus"))
      
      gc();
    };
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage2d", "Stage2s"))};
    
  } ## fold closures
  
  ###################################################################--
  ## 4.0 Convert Gear to Strings
  if(Fold) { 
    message("4 Converting Gear to Strings")
    
    ## V2.1.8 hack to track original String length during Stringing when safe to assume fishermen will not change endline strength
    ##  Added a GearPerString_Applied field, capture original String length, and use this to link to Line Strength model
    
    if(Fold) { ## Default data 
      ## Gear are converted to distribution of Strings based on existing empirical data or models of Gear / String 
      ## at the scale of stat areas and proposed management (#3 above).
      ## Note: String Length Model may need to be modified before merging due to new regulations on min / max String lengths
      # with(Stage4, tapply(X=GearFished, INDEX=list(Month, VesselKey), FUN=sum))
      
      # if(nrow(Stage3d)==
      #    nrow(unique(Stage3d[ ,c("Region", "Index", "Index_LR", "Month", "VesselKey")]))){
      #   Stage4d=Stage3d[ ,c("Region", "Index", "Index_LR", "Month", "VesselKey", "GearFished")]
      # } else {Stage4d=aggregate(GearFished~Region+VesselKey+Month+Index+Index_LR, Stage3d, sum);    }
      
      Stage4d=aggregate(GearFished~Region+VesselKey+Month+Index+Index_LR, Stage3d, sum);
      
      dim(Stage4d)
      Stage4d=merge(Stage4d, 
                    GearPerStringModel, 
                    all.x=TRUE); #summary(Stage4d)
      ## Stage4d[is.na(Stage4d$StringProportion), ]
      Stage4d$StringProportion[is.na(Stage4d$StringProportion) &
                                Stage4d$GearFished==0]=0
      Stage4d$GearPerString[is.na(Stage4d$GearPerString) &
                              Stage4d$GearFished==0]=1
      Stage4d$StringUnitCost[is.na(Stage4d$StringUnitCost) &
                              Stage4d$GearFished==0]=1
      Stage4d$GearPerString_Applied=round(Stage4d$GearPerString) ## V2.1.8
      
      if(length(which(is.na(Stage4d$StringProportion)))>0 | length(which(is.na(Stage4d$StringUnitCost)))>1){
        message("Error: Some Gear not matched to String configurations. Error in Stage4d");
        return(Stage4d)
      }
      
      Stage4d$StringUnits=with(Stage4d, GearFished / StringUnitCost); #summary(Stage4d) ## get the Strings multiplier
      Stage4d$StringsAtLength=with(Stage4d, StringUnits * StringProportion); ## get number of Strings at String length for grid cell
      ### summary(Stage4d)
      sum(Stage4d$StringsAtLength)
      
      if(PrintDefaultMaps){
        ############--
        Tmp=Stage4d[ ,c("VesselKey", "Month", "Index_LR", "GearPerString", "StringsAtLength")];
        Tmp2=aggregate(StringsAtLength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "Index_LR", "Totals")
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$Totals[Tmp$Totals==0]=NA
        Tmp$Strings=with(Tmp, GearPerString * StringsAtLength/Totals);
        # Tmp$Strings[is.na(Tmp$Strings)]=0
        ### summary(Tmp)
        ############### Plotting ####################--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Tmp,
          VarName="Strings",
          Plot1_Title="Mean String Length (# Pots) - Default",
          PlotLog=FALSE
        )
        
        Stage4d_StringLength_Px=OutputMapCall$Plot1_Px;
        map4dStringLength=OutputMapCall$Map1;
        
      } ## print Stage 3 default map
    } ## end default data
    
    
    if(TestScenario){
      # if(nrow(Stage3s)==
      #    nrow(unique(Stage3s[ ,c("Region", "Index", "Index_LR", "Month", "VesselKey")]))){
      #   Stage4s=Stage3s[ ,c("Region", "Index", "Index_LR", "Month", "VesselKey", "GearFished")]
      # } else {Stage4s=aggregate(GearFished~Region+VesselKey+Month+Index+Index_LR, Stage3s, sum);    }
      
      Stage4s=aggregate(GearFished~Region+VesselKey+Month+Index+Index_LR, Stage3s, sum);
      
      ################## Stage 4 Scenario ############################################################--
      if(Fold) { ## fold Scenario data 
        
        # with(Stage4, tapply(X=GearFished, INDEX=list(Month, VesselKey), FUN=sum))
        dim(Stage4s)
        
        Stage4s=merge(Stage4s, 
                      GearPerStringModel, 
                      all.x=TRUE); #summary(Stage4s)
        Stage4s$GearPerString_Applied=round(Stage4s$GearPerString) ## V2.1.8
        
        
        ## implement String length regulations ###############################################################--
        if(nrow(SC_StringLength)>0){ ## loop through String length changes 
          for(i in 1:nrow(SC_StringLength)){
            message(paste("Reallocating String lengths for ", i, " of ", nrow(SC_StringLength), " scenarios", sep=""))
            
            ## constrain spatially
            MapRef_CrI=MapRef;
            if(!is.na(SC_StringLength$LMA[i])) {
              MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_StringLength$LMA[i], ]
            } 
            if(!is.na(SC_StringLength$State[i])) {
              MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_StringLength$State[i], ]
            } 
            if(!is.na(SC_StringLength$StatArea[i])) {
              StatAreasI=as.numeric(strsplit(SC_StringLength$StatArea[i], ",")[[1]])
              MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
            }
            if(!is.na(SC_StringLength$Shapefile[i])) {
              SC_StringLengthShape=SC_StringLength$Shapefile[i]; SC_StringLengthShape ## name of shapefile
              ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_StringLengthShape, verbose=FALSE) ## load shapefile
              ShapeI$ID=1 ## create a known field 
              ShapeI=spTransform(ShapeI, Proj4)
              MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
            }
            
            ## get months applicable to String scenario
            if(!is.na(SC_StringLength$Months[i])){
              StringLengthMonthsI=as.numeric(strsplit(SC_StringLength$Months[i], ",")[[1]]); StringLengthMonthsI ## get months
            } else {StringLengthMonthsI=1:12}; ## or use all months
            
            
            # plot(MapRef_CrI)
            
            ## split Stage4 into areas Affected and Unaffected by management change 
            AffectedPx=merge(data.frame(Index=unique(MapRef_CrI$Index)), 
                             data.frame(Month=StringLengthMonthsI)); #summary(AffectedPx)
            # AffectedPx=Stage4s[Stage4s$Index %in% unique(MapRef_CrI$Index) &
            #                      Stage4s$Month %in% StringLengthMonthsI, ]; #summary(AffectedPx)
            
            ## split data into affected, unaffected; modify affected; recombine
            # UnAffectedGear=Stage4s[!Stage4s$Index %in% AffectedPx$Index | (Stage4s$Index %in% AffectedPx$Index & !Stage4s$Month %in% StringLengthMonthsI), 
            #                         c("Region", "VesselKey","Month","Index", "Index_LR", "Distance","GearFished",
            #                           "GearPerString", "GearPerString_Applied","StringProportion","StringUnitCost")]; dim(UnAffectedGear)
            UnAffectedGear=Stage4s[!Stage4s$Index %in% AffectedPx$Index | (Stage4s$Index %in% AffectedPx$Index & !Stage4s$Month %in% StringLengthMonthsI), 
                                    c("Region", "VesselKey","Month","Index", "Index_LR","GearFished",
                                      "GearPerString", "GearPerString_Applied","StringProportion","StringUnitCost")]; dim(UnAffectedGear)
            AffectedGear=merge(AffectedPx, Stage4s); dim(AffectedGear)
            
            if(SC_StringLength$StringRegulation[i]=="Max"){
              # ChangedGear=AffectedGear[AffectedGear$GearPerString>=SC_StringLength$StringLen[i], 
              #                            c("Region", "VesselKey", "Month", "Index", "Index_LR", "Distance", "GearFished", "GearPerString", "GearPerString_Applied",
              #                              "StringProportion")] ## Adjust these
              # UnChangedGear=AffectedGear[AffectedGear$GearPerString<SC_StringLength$StringLen[i], 
              #                              c("Region", "VesselKey", "Month", "Index", "Index_LR", "Distance", "GearFished", "GearPerString", "GearPerString_Applied",
              #                                "StringProportion")] 
              ChangedGear=AffectedGear[AffectedGear$GearPerString>=SC_StringLength$StringLen[i], 
                                         c("Region", "VesselKey", "Month", "Index", "Index_LR", "GearFished", "GearPerString", "GearPerString_Applied",
                                           "StringProportion")] ## Adjust these
              UnChangedGear=AffectedGear[AffectedGear$GearPerString<SC_StringLength$StringLen[i], 
                                           c("Region", "VesselKey", "Month", "Index", "Index_LR", "GearFished", "GearPerString", "GearPerString_Applied",
                                             "StringProportion")] 
              ChangedGear$Prod=with(ChangedGear, GearPerString * StringProportion); 
              ChangedGear$GearPerString=SC_StringLength$StringLen[i]
              ChangedGear=aggregate(Prod~Region+VesselKey+Month+Index+Index_LR+GearFished + GearPerString, 
                                     ChangedGear, sum); 
              UnChangedGear$Prod=with(UnChangedGear, GearPerString * StringProportion); UnChangedGear
              UnChangedGear=UnChangedGear[ ,names(ChangedGear)]; 
              
              AffectedGear=rbind(ChangedGear, UnChangedGear); 
            } ## end ifMax 
            
            if(SC_StringLength$StringRegulation[i]=="Min"){
              # ChangedGear=AffectedGear[AffectedGear$GearPerString<=SC_StringLength$StringLen[i], 
              #                            c("Region", "VesselKey", "Month", "Index", "Index_LR", "Distance", "GearFished", "GearPerString", "GearPerString_Applied",
              #                              "StringProportion")] ## Adjust these
              # UnChangedGear=AffectedGear[AffectedGear$GearPerString>SC_StringLength$StringLen[i], 
              #                              c("Region", "VesselKey", "Month", "Index", "Index_LR", "Distance", "GearFished", "GearPerString", "GearPerString_Applied",
              #                                "StringProportion")] 
              ChangedGear=AffectedGear[AffectedGear$GearPerString<=SC_StringLength$StringLen[i], 
                                         c("Region", "VesselKey", "Month", "Index", "Index_LR", "GearFished", "GearPerString", "GearPerString_Applied",
                                           "StringProportion")] ## Adjust these
              UnChangedGear=AffectedGear[AffectedGear$GearPerString>SC_StringLength$StringLen[i], 
                                           c("Region", "VesselKey", "Month", "Index", "Index_LR", "GearFished", "GearPerString", "GearPerString_Applied",
                                             "StringProportion")] 
              ChangedGear$Prod=with(ChangedGear, GearPerString * StringProportion); #summary(ChangedGear)
              ChangedGear$GearPerString=SC_StringLength$StringLen[i]
              ChangedGear=aggregate(Prod~Region+VesselKey+Month+Index+Index_LR+GearFished + GearPerString + GearPerString_Applied, 
                                     ChangedGear, sum); 
              UnChangedGear$Prod=with(UnChangedGear, GearPerString * StringProportion); UnChangedGear
              UnChangedGear=UnChangedGear[ ,names(ChangedGear)]; 
              
              AffectedGear=rbind(ChangedGear, UnChangedGear); 
            } ## end if Min
            
            if(SC_StringLength$StringRegulation[i]=="Exactly"){
              ChangedGear=AffectedGear
              ChangedGear$Prod=with(ChangedGear, GearPerString * StringProportion); 
              ChangedGear$GearPerString=SC_StringLength$StringLen[i]
              # ChangedGear=aggregate(Prod~Region+VesselKey+Month+Index+Index_LR+Distance+GearFished + GearPerString + GearPerString_Applied, 
              #                        ChangedGear, sum); 
              ChangedGear=aggregate(Prod~Region+VesselKey+Month+Index+Index_LR+GearFished + GearPerString + GearPerString_Applied, 
                                     ChangedGear, sum); 
              AffectedGear=ChangedGear; 
            } ## end if Exactly
            
            # mean(AffectedGear$GearPerString)
            
            ## Update String unit costs
            # hist(AffectedGear$GearPerString)
            
            AffectedGear$Prop1=AffectedGear$GearPerString * AffectedGear$Prod; AffectedGear
            
            # Prop1Tot=aggregate(Prop1~Region+VesselKey+Month+Index+Index_LR+Distance+GearFished, AffectedGear, sum); Prop1Tot
            Prop1Tot=aggregate(Prop1~Region+VesselKey+Month+Index+Index_LR+GearFished, AffectedGear, sum); Prop1Tot
            names(Prop1Tot)[names(Prop1Tot)=="Prop1"]="Prop1Total"
            
            AffectedGear=merge(AffectedGear, Prop1Tot); AffectedGear
            
            AffectedGear$StringProportion=with(AffectedGear, Prop1 / Prop1Total) # 5 new proportions
            AffectedGear$Product=with(AffectedGear, StringProportion * GearPerString)
            
            # UnitCost=aggregate(Product~Region+VesselKey+Month+Index+Index_LR+Distance+GearFished, AffectedGear, sum); UnitCost
            # names(UnitCost)=c("Region", "VesselKey", "Month", "Index", "Index_LR", "Distance", "GearFished", "StringUnitCost");
            
            UnitCost=aggregate(Product~Region+VesselKey+Month+Index+Index_LR+GearFished, AffectedGear, sum); UnitCost
            names(UnitCost)=c("Region", "VesselKey", "Month", "Index", "Index_LR", "GearFished", "StringUnitCost");
            
            AffectedGear=merge(AffectedGear, UnitCost); #summary(AffectedGear)
            
            # AffectedGear=AffectedGear[ ,c("Region", "VesselKey","Month","Index", "Index_LR", "Distance","GearFished",
            #                                 "GearPerString", "GearPerString_Applied","StringProportion","StringUnitCost")]; # summary(AffectedGear)
            AffectedGear=AffectedGear[ ,c("Region", "VesselKey","Month","Index", "Index_LR","GearFished",
                                            "GearPerString", "GearPerString_Applied","StringProportion","StringUnitCost")]; # summary(AffectedGear)
            
            # mean(Stage4s$GearPerString)
            # mean(Stage4s$GearPerString)
            
            ## append output within the loop
            Stage4s=rbind(UnAffectedGear, AffectedGear)
            
          } ## end SC_StringLength loop
          
          ## after changing String lengths, optionally update String length applied to line strength model
          if(UpdateEndlineStrengths) {Stage4s$GearPerString_Applied=round(Stage4s$GearPerString)}
        } ## end update to String lengths
        
        ## Stage4s[is.na(Stage4s$StringProportion), ]
        Stage4s$StringProportion[is.na(Stage4s$StringProportion) &
                                  Stage4s$GearFished==0]=0
        Stage4s$GearPerString[is.na(Stage4s$GearPerString) &
                                Stage4s$GearFished==0]=1
        Stage4s$StringUnitCost[is.na(Stage4s$StringUnitCost) &
                                Stage4s$GearFished==0]=1
        if(length(which(is.na(Stage4s$StringProportion)))>0 | length(which(is.na(Stage4s$StringUnitCost)))>1){
          message("Error: Some Gear not matched to String configurations. Error in Stage4s");
          return(Stage4s)
        }
        
        Stage4s$StringUnits=with(Stage4s, GearFished / StringUnitCost); #summary(Stage4s) ## get the Strings multiplier
        Stage4s$StringsAtLength=with(Stage4s, StringUnits * StringProportion); ## get number of Strings at String length for grid cell
        ### summary(Stage4s)
        sum(Stage4s$StringsAtLength)
        
        if(PrintScenarioMaps){
          
          ############--
          Tmp=Stage4s[ ,c("VesselKey", "Month", "Index_LR", "GearPerString", "StringsAtLength")];
          Tmp2=aggregate(StringsAtLength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
          names(Tmp2)=c("Month", "Index_LR", "Totals")
          Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
          Tmp$Totals[Tmp$Totals==0]=NA
          Tmp$Strings=with(Tmp, GearPerString * StringsAtLength/Totals);
          # Tmp$Strings[is.na(Tmp$Strings)]=0
          ### summary(Tmp)
          ############### Plotting ####################--
          OutputMapCall=OutputMap(         
            PlotDF=PlotDF,         
            MapRef=MapRef_LR,         
            Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
            InputDF=Tmp,
            VarName="Strings",
            Plot1_Title="Mean String Length (# Pots) - Scenario",
            PlotLog=FALSE
          )
          
          Stage4s_StringLength_Px=OutputMapCall$Plot1_Px;
          map4sStringLength=OutputMapCall$Map1;
          
        } ## Stage 4 scenario maps
        
      } ## fold Scenario data 
      
    }
    
    
    ############# Output Number of Strings #####################################--
    Stage4d_NumStrings_Output=aggregate(StringsAtLength~Month, Stage4d, sum); #Stage4d_NumStrings_Output
    Stage4dTotal=aggregate(StringsAtLength~1, Stage4d_NumStrings_Output, sum); #Stage4dTotal
    Stage4dTotal$Month="Total"
    Stage4d_NumStrings_Output=rbind(Stage4d_NumStrings_Output, Stage4dTotal); #Stage4d_NumStrings_Output
    Stage4d_NumStrings_Output$Scenario="Default"
    
    if(TestScenario){
      Stage4s_NumStrings_Output=aggregate(StringsAtLength~Month, Stage4s, sum); #Stage4s_NumStrings_Output
      Stage4sTotal=aggregate(StringsAtLength~1, Stage4s_NumStrings_Output, sum); #Stage4sTotal
      Stage4sTotal$Month="Total"
      Stage4s_NumStrings_Output=rbind(Stage4s_NumStrings_Output, Stage4sTotal); #Stage4s_NumStrings_Output
      Stage4s_NumStrings_Output$Scenario="Scenario"
      
      Stage4_NumStrings_Output=rbind(Stage4d_NumStrings_Output, Stage4s_NumStrings_Output); #Stage4_NumStrings_Output
    } else {
      Stage4_NumStrings_Output=Stage4d_NumStrings_Output; #Stage4_NumStrings_Output
    }
    Stage4_NumStrings_Output$Variable="NumStrings"
    Stage4_NumStrings_Output=Stage4_NumStrings_Output[ ,c("Variable", "Scenario", "Month", "StringsAtLength")]; #Stage4_NumStrings_Output
    names(Stage4_NumStrings_Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage4_NumStrings_Output); #OutputData
    
    ######################## Output Mean String Length ########################--
    Tmp=Stage4d[ ,c("Month", "GearPerString", "StringsAtLength")];
    Tmp2=aggregate(StringsAtLength~Month, Tmp, sum); ###summary(Tmp2)
    names(Tmp2)=c("Month", "Totals")
    Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
    Tmp$Totals[Tmp$Totals==0]=NA
    Tmp$MeanGearPerString=with(Tmp, GearPerString * StringsAtLength/Totals);
    Stage4d_GpS_Output=aggregate(MeanGearPerString~Month, Tmp, sum); ###summary(Stage4_Agg)
    Stage4d_GpS_Output$Scenario="Default"
    
    if(TestScenario){
      Tmp=Stage4s[ ,c("Month", "GearPerString", "StringsAtLength")];
      Tmp2=aggregate(StringsAtLength~Month, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$Totals[Tmp$Totals==0]=NA
      Tmp$MeanGearPerString=with(Tmp, GearPerString * StringsAtLength/Totals);
      Stage4s_GpS_Output=aggregate(MeanGearPerString~Month, Tmp, sum); ###summary(Stage4_Agg)
      Stage4s_GpS_Output$Scenario="Scenario"
      
      Stage4_GpS_Output=rbind(Stage4d_GpS_Output, Stage4s_GpS_Output); #Stage4_GpS_Output
    } else {
      Stage4_GpS_Output=Stage4d_GpS_Output
    }
    Stage4_GpS_Output$Variable="MeanStringLength"
    Stage4_GpS_Output=Stage4_GpS_Output[ ,c("Variable", "Scenario", "Month", "MeanGearPerString")]; #Stage4_GpS_Output
    names(Stage4_GpS_Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage4_GpS_Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall", "MapRef_CrI", "MapRef_HR_I", "Months", "StatAreasI", "SC_StringLengthShape", "SC_StringLength", 
                          "ShapeI", "StringLengthMonthsI", "AffectedPx", "UnAffectedGear", "AffectedGear", "ChangedGear", "UnChangedGear", "Prop1Tot", 
                          "UnitCost", "UpdateEndlineStrengths", "Tmp", "OutputMapCall", "Stage4d_NumStrings_Output", "Stage4dTotal", "Stage4d_NumStrings_Output", 
                          "Stage4s_NumStrings_Output", "Stage4sTotal", "Stage4_NumStrings_Output", "Tmp2", "Stage4s_GpS_Output", "Stage4d_GpS_Output", "Stage4_GpS_Output",
                          "GearPerStringModel","Stage4s_StringLength_Px"))
      gc();
    }
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage3d", "Stage3s"))};
    
    
  } ## convert Gear to Strings; management action
  
  ###################################################################--
  ## 5.0 Convert Strings to vertical Lines
  if(Fold) { 
    message("5 Calculating Vertical Lines from Strings")
    ## Strings are converted to vertical lines based on String length. 
    ## Expected to be two vertical lines per String for all offshore areas 
    ## but may be different for inshore areas with shorter Strings.
    
    # if(nrow(Stage4d)==
    #    nrow(unique(Stage4d[ ,c("Index", "Index_LR", "Region", "Month", "GearPerString", "GearPerString_Applied")]))){
    #   Stage5d=Stage4d[ ,c("Index", "Index_LR", "Region", "Month", "GearPerString", "GearPerString_Applied", "StringsAtLength")]
    # } else {Stage5d=aggregate(StringsAtLength~Index+Index_LR+Region+Month+GearPerString + GearPerString_Applied, Stage4d, sum);    }
    
    Stage5d=aggregate(StringsAtLength~Index+Index_LR+Region+Month+GearPerString + GearPerString_Applied, Stage4d, sum);
    # Stage5d$GearPerStringInt=round(Stage5d$GearPerString); ## no longer needed?
    Stage5d=merge(Stage5d, EndlinesPerString, all.x=TRUE); ###summary(Stage5d) 
    Stage5d$EndlinesPerString=2 ##V2.2.0
    Stage5d$EndlinesPerString[
      Stage5d$GearPerString <= Stage5d$GearPerStringWithOneEndline]=1 ##V2.2.0
    
    if(any(is.na(Stage5d$EndlinesPerString))){ message("Error in Stage5d: Some String lengths not matched to endlines"); return(Stage5d)}
    
    Stage5d$NumVerticalLines=Stage5d$StringsAtLength * Stage5d$EndlinesPerString
    ### summary(Stage5d)
    # aggregate(NumVerticalLines~Month, Stage5d, sum)
    
    if(PrintDefaultMaps){    
      ############ Plotting ##############################################################--
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Stage5d,
        VarName="NumVerticalLines",
        Plot1_Title="Aggregate Line Density, Default",
        PlotLog=TRUE,
        LogOffset=0.01,
        Plot2_Title="Aggregate Line Density, Log-Scaled - Default"
      )
      
      Stage5d_LineDensity_Px=OutputMapCall$Plot1_Px;
      map5dLineDensity=OutputMapCall$Map1;
      Stage5d_LineDensityLog_Px=OutputMapCall$Plot1_Px
      map5dLineDensityLog=OutputMapCall$Map2
      
    } ## Stage 5 default maps
    
    ######################## Stage 5 Scenario #####################################################################--
    if(TestScenario){
      
      # if(nrow(Stage4s)==
      #    nrow(unique(Stage4s[ ,c("Index", "Index_LR", "Region", "Month", "GearPerString", "GearPerString_Applied")]))){
      #   Stage5s=Stage4s[ ,c("Index", "Index_LR", "Region", "Month", "GearPerString", "GearPerString_Applied", "StringsAtLength")]
      # } else {Stage5s=aggregate(StringsAtLength~Index+Index_LR+Region+Month+GearPerString + GearPerString_Applied, Stage4s, sum);    }
      # dim(Stage5s);
      
      Stage5s=aggregate(StringsAtLength~Index+Index_LR+Region+Month+GearPerString + GearPerString_Applied, Stage4s, sum);
      
      # Stage5s$GearPerStringInt=round(Stage5s$GearPerString);
      Stage5s=merge(Stage5s, EndlinesPerString, all.x=TRUE); ###summary(Stage5s) 
      Stage5s$EndlinesPerString=2 ##V2.2.0
      Stage5s$EndlinesPerString[Stage5s$GearPerString <= Stage5s$GearPerStringWithOneEndline]=1 ##V2.2.0
      
      if(any(is.na(Stage5s$EndlinesPerString))){ message("Error in Stage5s: Some String lengths not matched to endlines"); return(Stage5s)}
      
      ## apply rule for Strings with singe lines ############################################################--
      if(nrow(SC_MaxGearWSingleLine)>0){
        for(i in 1:nrow(SC_MaxGearWSingleLine)){
          ## constrain spatially
          MapRef_CrI=MapRef;
          if(!is.na(SC_MaxGearWSingleLine$LMA[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_MaxGearWSingleLine$LMA[i], ]
          } 
          if(!is.na(SC_MaxGearWSingleLine$State[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_MaxGearWSingleLine$State[i], ]
          } 
          if(!is.na(SC_MaxGearWSingleLine$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_MaxGearWSingleLine$StatArea[i], ",")[[1]])
            MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_MaxGearWSingleLine$Shapefile[i])) {
            SC_MaxGearWSingleLineShape=SC_MaxGearWSingleLine$Shapefile[i]; SC_MaxGearWSingleLineShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_MaxGearWSingleLineShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          }
          
          if(exists("MapRef_Cr")){
            MapRef_Cr=rbind(MapRef_Cr, MapRef_CrI)
          } else {     MapRef_Cr=MapRef_CrI }
          # plot(MapRef_Cr)
          
          ## get months applicable to String scenario
          if(!is.na(SC_MaxGearWSingleLine$Months[i])){
            MaxGearWSingleLineMonthsI=as.numeric(strsplit(SC_MaxGearWSingleLine$Months[i], ",")[[1]]); MaxGearWSingleLineMonthsI ## get months
          } else {MaxGearWSingleLineMonthsI=1:12}; ## or use all months
          
          ## find the locations and months impacted
          AffectedPxI=merge(data.frame(Index=unique(MapRef_CrI$Index)),
                            data.frame( Month=MaxGearWSingleLineMonthsI,
                                        Affected=TRUE)); #summary(AffectedPxI)
          
          namesStage5s=names(Stage5s)
          
          Stage5s=merge(Stage5s, AffectedPxI, all.x=TRUE); #summary(Stage5s)
          
          Stage5s$EndlinesPerString[!is.na(Stage5s$Affected) &
                                     # Stage5s$GearPerStringInt<= SC_MaxGearWSingleLine$MaxGearSnglLn] = 1
                                     Stage5s$GearPerString<= SC_MaxGearWSingleLine$MaxGearSnglLn] = 1
          Stage5s=Stage5s[ ,namesStage5s]; #summary(Stage5s)
          
        } ## end loop for individual scenario
      } ## end loops across scenarios
      # summary(Stage5s)
      Stage5s$NumVerticalLines=Stage5s$StringsAtLength * Stage5s$EndlinesPerString
      
      # Bk=Stage5s;
      ############################### Ropeless Scenarios################################################################--
      if(nrow(SC_RopelessDevice)>0){
        for(i in 1:nrow(SC_RopelessDevice)){
          message(paste("Applying ropeless devices for ", i, " of ", nrow(SC_RopelessDevice), " scenarios", sep=""))
          
          ## constrain spatially
          MapRef_CrI=MapRef;
          if(!is.na(SC_RopelessDevice$LMA[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_RopelessDevice$LMA[i], ]
          } 
          if(!is.na(SC_RopelessDevice$State[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_RopelessDevice$State[i], ]
          } 
          if(!is.na(SC_RopelessDevice$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_RopelessDevice$StatArea[i], ",")[[1]])
            MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_RopelessDevice$Shapefile[i])) {
            SC_RopelessDeviceShape=SC_RopelessDevice$Shapefile[i]; SC_RopelessDeviceShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_RopelessDeviceShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          }
          # plot(MapRef_CrI)
          
          ## find the locations and months impacted
          AffectedPxI=merge(data.frame(Index=unique(MapRef_CrI$Index)),
                            data.frame(
                              Month=as.numeric(strsplit(SC_RopelessDevice$Months[i], ",")[[1]]),
                              Affected=TRUE)
          ); #summary(AffectedPxI)
          
          GearStatus=merge(Stage5s, AffectedPxI, all.x=TRUE); # summary(GearStatus)
          
          
          ## split data into affected, unaffected; modify affected; recombine
          UnAffectedGear=GearStatus[ is.na(GearStatus$Affected), ]; dim(UnAffectedGear) ## V2.1.5
          AffectedGear  =GearStatus[!is.na(GearStatus$Affected), ]; dim(AffectedGear) ## V2.1.5
          # UnAffectedGear=Stage5s[!Stage5s$Index %in% AffectedPx, ]; dim(UnAffectedGear) ## V2.1.4
          # AffectedGear=merge(Stage5s, MapRef_CrI[ ,c("Index", "Distance")]); dim(AffectedGear) ## V2.1.4
          
          ## get ropeless device applied to scenario
          AffectedGear$RopelessDevice=SC_RopelessDevice$RopelessDevice[i]
          AffectedGear=merge(AffectedGear, RopelessRisk); dim(AffectedGear) ## merge with reference list
          
          AffectedGear$NumVerticalLines_Adj=with(AffectedGear, NumVerticalLines * LineMultiplier);
          summary(AffectedGear)
          
          ##  NumVerticalLines with NumVerticalLines_Adj
          # AffectedGear=AffectedGear[ ,c("Index", "Index_LR", "Distance","Month","GearPerString","GearPerStringInt", "GearPerString_Applied","StringsAtLength",
          #                                 "EndlinesPerString","NumVerticalLines_Adj")]
          # names(AffectedGear)=c("Index", "Index_LR", "Distance","Month","GearPerString","GearPerStringInt", "GearPerString_Applied","StringsAtLength",
          #                        "EndlinesPerString","NumVerticalLines")
          
          AffectedGear=AffectedGear[ ,c("Index", "Index_LR","Month","GearPerString", "GearPerString_Applied","StringsAtLength",
                                          "EndlinesPerString","NumVerticalLines_Adj")]
          names(AffectedGear)=c("Index", "Index_LR", "Month","GearPerString", "GearPerString_Applied","StringsAtLength",
                                 "EndlinesPerString","NumVerticalLines")
          
          Stage5s=rbind(UnAffectedGear[ ,names(AffectedGear)], AffectedGear); dim(Stage5s)
          
          ## check to ensure a management actions wasn't applied twice
          if(i==1){
            AffectedPx=AffectedPxI
          } else {
            AffectedPx=rbind(AffectedPx, AffectedPxI)
          }
          AffectedPx_Check=aggregate(Affected~Index+Month, AffectedPx, length);
          if(max(AffectedPx_Check$Affected>1)) {
            message("Error, Ropeless Device applied multiple times to same location and month")
            return(AffectedPx)
          }
          
        } ## end loop across scenarios
      } ## end application of ropeless
      
      ### summary(Stage5s)
      # aggregate(NumVerticalLines~Month, Stage5s, sum)
      
      
      if(PrintScenarioMaps){
        ############ Plotting ##############################################################--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Stage5s,
          VarName="NumVerticalLines",
          Plot1_Title="Aggregate Vertical Line Density - Scenario",
          PlotLog=TRUE,
          LogOffset=0.01,
          Plot2_Title="Aggregate Line Density, Log-Scaled - Scenario"
        )
        
        Stage5s_LineDensity_Px=OutputMapCall$Plot1_Px;
        map5sLineDensity=OutputMapCall$Map1;
        Stage5s_LineDensityLog_Px=OutputMapCall$Plot1_Px
        map5sLineDensityLog=OutputMapCall$Map2
        
      } ## Stage 4 scenario maps
    } ## End Test Scenario
    
    ######################################################################--
    Stage5dOutput=aggregate(NumVerticalLines~Month, Stage5d, sum); #Stage5dOutput
    Stage5dTotal=aggregate(NumVerticalLines~1, Stage5dOutput, sum); #Stage5dTotal
    Stage5dTotal$Month="Total"
    Stage5dOutput=rbind(Stage5dOutput, Stage5dTotal); #Stage5dOutput
    Stage5dOutput$Scenario="Default"
    
    if(TestScenario){
      Stage5sOutput=aggregate(NumVerticalLines~Month, Stage5s, sum); #Stage5sOutput
      Stage5sTotal=aggregate(NumVerticalLines~1, Stage5sOutput, sum); #Stage5sTotal
      Stage5sTotal$Month="Total"
      Stage5sOutput=rbind(Stage5sOutput, Stage5sTotal); #Stage5sOutput
      Stage5sOutput$Scenario="Scenario"
      
      Stage5Output=rbind(Stage5dOutput, Stage5sOutput); #Stage5Output
      
    } else {
      Stage5Output=Stage5dOutput; #Stage5Output
    }
    
    Stage5Output$Variable="NumVerticalLines"
    Stage5Output=Stage5Output[ ,c("Variable", "Scenario", "Month", "NumVerticalLines")]; #Stage5Output
    names(Stage5Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage5Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall","MapRef_CrI","MapRef_HR_I","Months","SC_MaxGearWSingleLine","SC_MaxGearWSingleLineShape","ShapeI","MapRef_Cr",
                          "MaxGearWSingleLineMonthsI","AffectedPxI","SC_RopelessDevice","SC_RopelessDeviceShape","ShapeI","GearStatus","UnAffectedGear",
                          "AffectedGear","AffectedPx","AffectedPx_Check","Stage5dOutput","Stage5dTotal","Stage5sOutput","Stage5sTotal","Stage5Output",
                          "EndlinesPerString", "RopelessRisk", "Stage5s_LineDensity_Px", "Stage5s_LineDensityLog_Px"))
      gc();
    };
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage4d", "Stage4s"))};
    
  } ## convert Strings to vertical lines
  
  ####################################################################--
  ## 6.0 Characterize Vertical Line Strengths
  if(Fold) {
    message("6 Characterizing Vertical Line Strengths")
    ## A distribution of line Strengths for vertical lines is characterized 
    ## based on observed relationships with String length and further modified based on management options.
    Stage6d=Stage5d[Stage5d$NumVerticalLines>0.001, ]; dim(Stage6d)
    Stage6d=merge(Stage6d, LineStrengthMod, all.x=TRUE); #summary(Stage6d)
    
    if(any(is.na(Stage6d$RopeStrength))){ ## if there are any String lengths that don't match an endline
      message("Error: Some Strings lengths not matched to rope diameters.  Error in Stage6d");
      return(Stage6d)
    }
    
    ## apportion lines across sizes
    Stage6d$NumVerticalLinesAtStrength=with(Stage6d, NumVerticalLines * Prop_Strength)
    #summary(Stage6d)
    
    if(PrintDefaultMaps){
      ############--
      Tmp=Stage6d[ ,c("Month", "Index_LR", "RopeStrength", "NumVerticalLinesAtStrength")];
      Tmp2=aggregate(NumVerticalLinesAtStrength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "Index_LR", "Totals")
      # plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
      Tmp2$Totals[Tmp2$Totals==0]=0.001
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$LineStrength=with(Tmp, RopeStrength * NumVerticalLinesAtStrength/Totals);
      
      ### summary(Tmp)
      ###############--
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Tmp,
        VarName="LineStrength",
        Plot1_Title="Mean Line Strength - Default",
        PlotLog=FALSE
      )
      
      Stage6d_LineStrength_Px=OutputMapCall$Plot1_Px;
      map6dLineStrength=OutputMapCall$Map1;
      
    } ## Stage 6 default maps
    
    
    ############### Stage 6 Scenario #####################################--
    if(TestScenario){
      
      Stage6s=Stage5s[Stage5s$NumVerticalLines>0.001, ]; dim(Stage6s)
      Stage6s=merge(Stage6s, LineStrengthMod, all.x=TRUE); #summary(Stage6s)
      
      if(length(which(is.na(Stage6s$RopeStrength)))>0){ ## if there are any String lengths that don't match an endline
        message("Error: Some Strings lengths not matched to line diameters.  Error in Stage6s");
        return(Stage6s)
      }
      
      ## apportion lines across sizes
      Stage6s$NumVerticalLinesAtStrength=with(Stage6s, NumVerticalLines * Prop_Strength)
      #summary(Stage6s)
      # table(Stage6s$Rope)
      
      ## moved scenario mapping to after stage 7
    }    
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall","MapRef_CrI","MapRef_HR_I","Months","Tmp","Tmp2", "LineStrengthMod"))
      gc();
    };
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage5d", "Stage5s"))};
    
    
  } ## fold line strength
  
  ##################################################################--
  ## 7.0 Modify line diameters according to management action
  ## Number of vertical lines are further adjusted for any regulations on ropeless or timed-release fishing (#5 above). 
  ## Given the technology available to the lobster fishery and other fisheries, 
  ## it is improbable that these measures can be implemented immediately 
  ## but it is appropriate to include this now in the model framework.
  ## this includes 
  if(Fold) { ## fold line strength management 
    message("7 Applying any management measures to line diameters")
    
    # if(nrow(Stage6d)==
    #    nrow(unique(Stage6d[ ,c("Index", "Index_LR", "Month", "RopeStrength", "GearPerString")]))){
    #   Stage7d=Stage6d[ ,c("Index", "Index_LR", "Month", "RopeStrength", "GearPerString", "NumVerticalLinesAtStrength")]
    # } else {Stage7d=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength+GearPerString, Stage6d, sum);    }
    
    Stage7d=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength+GearPerString, Stage6d, sum);
    
    if(TestScenario){
      
      # if(nrow(Stage6s)==
      #    nrow(unique(Stage6s[ ,c("Index", "Index_LR", "Month", "RopeStrength", "GearPerString")]))){
      #   Stage7s=Stage6s[ ,c("Index", "Index_LR", "Month", "RopeStrength", "GearPerString", "NumVerticalLinesAtStrength")]
      # } else {Stage7s=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength+GearPerString, Stage6s, sum);    }
      
      Stage7s=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength+GearPerString, Stage6s, sum);
      
      # Stage7d$BuoylineDevice=NA
      
      dim(Stage7d)
      
      
      ## Manipulate line strength for management ######################################################--
      
      if(nrow(SC_MaxRopeStrength)>0){
        # names(SC_MaxRopeStrength)=c("Action", "LMA", "State", "StatArea", "Shapefile", "Months", "Rope");
        # SC_MaxRopeStrength=merge(SC_MaxRopeStrength, LineConversion, all.x=TRUE)
        # if(any(is.na(SC_MaxRopeStrength$Rope))){ message("Error in Stage7 SC_MaxRopeStrength: Some Scenario rope diameters not matched to ropes"); return(Stage7s)}
        
        for(i in 1:nrow(SC_MaxRopeStrength)){
          message(paste("Applying changes to vertical lines for ", i, " of ", nrow(SC_MaxRopeStrength), " scenarios", sep=""))
          
          ## constrain spatially
          MapRef_CrI=MapRef;
          if(!is.na(SC_MaxRopeStrength$LMA[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_MaxRopeStrength$LMA[i], ]
          } 
          if(!is.na(SC_MaxRopeStrength$State[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_MaxRopeStrength$State[i], ]
          } 
          if(!is.na(SC_MaxRopeStrength$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_MaxRopeStrength$StatArea[i], ",")[[1]])
            MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_MaxRopeStrength$Shapefile[i])) {
            SC_MaxRopeStrengthShape=SC_MaxRopeStrength$Shapefile[i]; SC_MaxRopeStrengthShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_MaxRopeStrengthShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          }
          # plot(MapRef_CrI)
          
          if(!is.na(SC_MaxRopeStrength$Months[i])){
            SC_MaxRopeStrengthMonthsI=as.numeric(strsplit(SC_MaxRopeStrength$Months[i], ",")[[1]]); SC_MaxRopeStrengthMonthsI ## get months
          } else {SC_MaxRopeStrengthMonthsI=1:12}; ## or use all months
          
          if(!is.na(SC_MaxRopeStrength$Percentage[i])){
            SC_MaxRopeStrengthPercentageI=SC_MaxRopeStrength$Percentage[i] ## get Percentage
          } else {SC_MaxRopeStrengthPercentageI=1}; ## or use all months
          
          ## split data into affected, unaffected; modify affected; recombine
          AffectedPx=unique(MapRef_CrI$Index)
          UnAvailableLines=Stage7s[!Stage7s$Index %in% AffectedPx |## wrong location
                                     (Stage7s$Index %in% AffectedPx & !Stage7s$Month %in% SC_MaxRopeStrengthMonthsI) | ## right location, wrong month
                                     (Stage7s$Index %in% AffectedPx & Stage7s$Month %in% SC_MaxRopeStrengthMonthsI & Stage7s$RopeStrength<=SC_MaxRopeStrength$MaxRopeStrength[i]) ## right location, right month, rope already below maximum
                                   , ]; dim(UnAvailableLines)
          AvailableLines=Stage7s[Stage7s$Index %in% AffectedPx & 
                                   Stage7s$Month %in% SC_MaxRopeStrengthMonthsI & 
                                   Stage7s$RopeStrength>SC_MaxRopeStrength$MaxRopeStrength[i], ]; dim(AvailableLines)
          
          ## V2.2.8 Catch for no lines affected by MaxRopeStrength
          if(nrow(AvailableLines)>0){ ## non-zero number of lines affected
            ## V2.1.7 split available lines for application of percentage
            ## AffectedLines have the line strength reduction applied to them
            AffectedLines=AvailableLines;
            AffectedLines$RopeStrength=SC_MaxRopeStrength$MaxRopeStrength[i] ## change line strength
            AffectedLines$NumVerticalLinesAtStrength=
              AffectedLines$NumVerticalLinesAtStrength *  SC_MaxRopeStrengthPercentageI## apply percentage
            
            ## UnaffectedLines have no line strength reduction
            UnAffectedLines=AvailableLines; ## portion of available lines that are not changed
            UnAffectedLines$NumVerticalLinesAtStrength=
              UnAffectedLines$NumVerticalLinesAtStrength *  (1-SC_MaxRopeStrengthPercentageI) ## apply percentage
            
            ## error catch to ensure lines were not lost in reallocation 
            ## this is not done with a simple sum check as rounding errors were stopping the model run
            LostLines=(sum(UnAvailableLines$NumVerticalLinesAtStrength) + 
                         sum(AffectedLines$NumVerticalLinesAtStrength) + 
                         sum(UnAffectedLines$NumVerticalLinesAtStrength)) - 
              sum(Stage7s$NumVerticalLinesAtStrength)
            
            pLostLines=abs(LostLines)/sum(Stage7s$NumVerticalLinesAtStrength) ## proportion of lines lost
            
            if(pLostLines > 0.001) { ## if discrepancy is >0.1% break
              message("Error: Records lost in Stage7 splitting Unaffected and Affected")
              Output=list(Stage7s, UnAvailableLines, AvailableLines, AffectedLines, UnAffectedLines);
              names(Output)=c("Stage7s", "UnAvailableLines", "AvailableLines", "AffectedLines", "UnAffectedLines")
              return(Output)
            }
            # table(AffectedLines$RopeStrength)
            
            Stage7s=rbind(UnAvailableLines, AffectedLines, UnAffectedLines);
            
          } else { ## no lines affected by MaxRopeStrength
            message(paste(
              "Warning: No rope strengths modified by MaxRopeStrength", 
              SC_MaxRopeStrength$MaxRopeStrength[i]
                           )) ## V2.2.9
            ## no need to update Stage7s
          } ## end catch for non-zero lines affected
          
        } ## end scenario loop
      } ## end management actions
      
      
      # Stage7s$BuoylineDevice=NA
      
      
      ## Add Buoyline Devices ##########################################################################--
      
      ##  SC_BuoylineDevice
      # unique(BuoylineThreat$BuoylineDevice)
      
      if(nrow(SC_BuoylineDevice)>0){
        
        for(i in 1:nrow(SC_BuoylineDevice)){
          message(paste("Applying changes to vertical lines devices for ", i, " of ", nrow(SC_BuoylineDevice), " scenarios", sep=""))
          
          ## constrain spatially
          MapRef_CrI=MapRef;
          if(!is.na(SC_BuoylineDevice$LMA[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$LMA==SC_BuoylineDevice$LMA[i], ]
          } 
          if(!is.na(SC_BuoylineDevice$State[i])) {
            MapRef_CrI=MapRef_CrI[MapRef_CrI$State==SC_BuoylineDevice$State[i], ]
          } 
          if(!is.na(SC_BuoylineDevice$StatArea[i])) {
            StatAreasI=as.numeric(strsplit(SC_BuoylineDevice$StatArea[i], ",")[[1]])
            MapRef_CrI=MapRef_CrI[MapRef_CrI$StatArea %in% StatAreasI, ]
          }
          if(!is.na(SC_BuoylineDevice$Shapefile[i])) {
            SC_BuoylineDeviceShape=SC_BuoylineDevice$Shapefile[i]; SC_BuoylineDeviceShape ## name of shapefile
            ShapeI=readOGR(dsn=paste(HD, "/TempShapefiles", sep=""), layer=SC_BuoylineDeviceShape, verbose=FALSE) ## load shapefile
            ShapeI$ID=1 ## create a known field 
            ShapeI=spTransform(ShapeI, Proj4)
            MapRef_CrI = MapRef_CrI[!is.na(over(MapRef_CrI, ShapeI)$ID), ] ## get spatial overlap of shapefile from overlay
          }
          # plot(MapRef_CrI)
          
          if(!is.na(SC_BuoylineDevice$Months[i])){
            SC_BuoylineDeviceMonthsI=as.numeric(strsplit(SC_BuoylineDevice$Months[i], ",")[[1]]); SC_BuoylineDeviceMonthsI ## get months
          } else {SC_BuoylineDeviceMonthsI=1:12}; ## or use all months
          
          ## split data into affected, unaffected; modify affected; recombine
          AffectedPx=unique(MapRef_CrI$Index)
          UnAffectedGear=Stage7s[!Stage7s$Index %in% AffectedPx |
                                    (Stage7s$Index %in% AffectedPx &
                                       !Stage7s$Month %in% SC_BuoylineDeviceMonthsI), ]; dim(UnAffectedGear)
          AffectedGear=Stage7s[Stage7s$Index %in% AffectedPx & 
                                  Stage7s$Month %in% SC_BuoylineDeviceMonthsI, ]; dim(AffectedGear)
          
          if(nrow(Stage7s)!= (nrow(UnAffectedGear) + nrow(AffectedGear)) ) {
            message("Error: Records lost in Stage7 splitting Unaffected and Affected")
            return()
          }
          
          AffectedGear$BuoylineDevice = SC_BuoylineDevice$BuoylineDevice[i]
          
          # table(AffectedGear$RopeStrength)
          
          Stage7s=rbind(UnAffectedGear, AffectedGear);
          
        } ## end scenario loop
      } ## end management actions
      
      #######################################
      if(PrintScenarioMaps){
        ############--
        Tmp=Stage7s[ ,c("Month", "Index_LR", "RopeStrength", "NumVerticalLinesAtStrength")];
        Tmp2=aggregate(NumVerticalLinesAtStrength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "Index_LR", "Totals")
        ###plot(quantile(Tmp2$Totals, (0:100)/100), ylim=c(0,1));
        Tmp2$Totals[Tmp2$Totals==0]=0.01
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$LineStrength=with(Tmp, RopeStrength * NumVerticalLinesAtStrength/Totals);
        
        ### summary(Tmp)
        ###############--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Tmp,
          VarName="LineStrength",
          Plot1_Title="Mean Line Strength - Scenario",
          PlotLog=FALSE
        )
        
        Stage7s_LineStrength_Px=OutputMapCall$Plot1_Px;
        map7sLineStrength=OutputMapCall$Map1;
        
      } ## Stage 7 Scenario Maps
      
    } ## end TestScenario
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage6d", "Stage6s"))};
    
    ## get a plot of rope strength distributions V2.1.0
    dRopeStrengthDist=aggregate(NumVerticalLinesAtStrength~Month+RopeStrength, Stage7d, sum); # summary(dRopeStrengthDist)
    Totals=aggregate(NumVerticalLinesAtStrength~Month, dRopeStrengthDist, sum); names(Totals)=c("Month", "Total"); # summary(Totals)
    dRopeStrengthDist=merge(dRopeStrengthDist, Totals); # summary(dRopeStrengthDist)
    dRopeStrengthDist$Prop=with(dRopeStrengthDist, NumVerticalLinesAtStrength/Total); # summary(dRopeStrengthDist)
    dRopeStrengthDist=dRopeStrengthDist[order(dRopeStrengthDist$RopeStrength), ]
    dRopeStrengthDist$Source="Default"
    
    if(TestScenario){
      sRopeStrengthDist=aggregate(NumVerticalLinesAtStrength~Month+RopeStrength, Stage7s, sum); # summary(sRopeStrengthDist)
      Totals=aggregate(NumVerticalLinesAtStrength~Month, sRopeStrengthDist, sum); names(Totals)=c("Month", "Total"); # summary(Totals)
      sRopeStrengthDist=merge(sRopeStrengthDist, Totals); # summary(sRopeStrengthDist)
      sRopeStrengthDist$Prop=with(sRopeStrengthDist, NumVerticalLinesAtStrength/Total); # summary(sRopeStrengthDist)
      sRopeStrengthDist=sRopeStrengthDist[order(sRopeStrengthDist$RopeStrength), ]
      sRopeStrengthDist$Source="Scenario"
    }
    
    if(TestScenario){
      RopeStrengthDist=rbind(
        dRopeStrengthDist,
        sRopeStrengthDist
      )
    } else {
      RopeStrengthDist=
        dRopeStrengthDist
    }
    
    RopeStrengthDist=RopeStrengthDist[order(RopeStrengthDist$RopeStrength), ]
    
    plotRopeStrengthDistribution=
      xyplot(Prop~RopeStrength|factor(Month), groups=Source, RopeStrengthDist, type="b",
             scales=list(x=list(rot=90, alternating=3)), ylim=c(0,NA), 
             pch=20, lwd=c(4,2),
             xlab="Rope Strength", ylab="Proportion", main="Montly Distribution of Rope Strengths",
             par.settings=simpleTheme(col=c("blue", "green"), lwd=3),
             auto.key=list(text=c("Default", "Scenario"),lines=T, points=F,lwd=3,cex=0.75,
                           x=1.0, y=0.950, corner=c(1,1)),
             panel=function(x,y,...){
               panel.xyplot(x,y,...)
               panel.abline(v=1700, col="gray50")
               panel.abline(h=(1:20)/20, col="gray70", lty="dotted")
             }
      )
    
    
    
    ######################## concatenate results ##########################-
    Stage7d_Output=Stage7d; Stage7d_Output$Scenario="Default"
    
    if(TestScenario){
      Stage7s_Output=Stage7s; Stage7s_Output$Scenario="Scenario"
      Stage7_Output=rbind(Stage7d_Output, Stage7s_Output)
    } else {
      Stage7_Output=Stage7d_Output
    }
    
    NumVerticalLines=aggregate(NumVerticalLinesAtStrength~Month+Scenario, Stage7_Output, sum); # sum within months
    names(NumVerticalLines)=c("Month", "Scenario", "TotalLines")
    Stage7_OutputByMonth=merge(Stage7_Output, NumVerticalLines); #summary(Stage7_OutputByMonth)
    Stage7_OutputByMonth$RopeStrength=with(Stage7_OutputByMonth, RopeStrength * NumVerticalLinesAtStrength/TotalLines);
    Stage7_OutputByMonth_Agg=aggregate(RopeStrength~Month+Scenario, Stage7_OutputByMonth, sum); #Stage7_OutputByMonth_Agg;
    
    NumVerticalLines=aggregate(NumVerticalLinesAtStrength~Scenario, Stage7_Output, sum); # sum within months
    names(NumVerticalLines)=c("Scenario", "TotalLines")
    Stage7_OutputTotal=merge(Stage7_Output, NumVerticalLines); #summary(Stage7_OutputTotal)
    Stage7_OutputTotal$RopeStrength=with(Stage7_OutputTotal, RopeStrength * NumVerticalLinesAtStrength/TotalLines);
    Stage7_OutputTotal_Agg=aggregate(RopeStrength~Scenario, Stage7_OutputTotal, sum); Stage7_OutputTotal_Agg;
    Stage7_OutputTotal_Agg$Month="Total";
    
    Stage7_Output=rbind(Stage7_OutputByMonth_Agg, Stage7_OutputTotal_Agg); Stage7_Output
    Stage7_Output$Variable="RopeStrength"
    Stage7_Output=Stage7_Output[ ,c("Variable", "Scenario", "Month", "RopeStrength")]; #
    names(Stage7_Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage7_Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall","MapRef_CrI","MapRef_HR_I","Months","SC_MaxRopeStrength","StatAreasI","SC_MaxRopeStrengthShape",
                          "ShapeI","SC_MaxRopeStrengthMonthsI","SC_MaxRopeStrengthPercentageI","AffectedPx","UnAvailableLines","AvailableLines","AffectedLines",
                          "UnAffectedLines","LostLines","pLostLines","SC_BuoylineDevice","SC_BuoylineDeviceShape","SC_BuoylineDeviceMonthsI","UnAffectedGear","AffectedGear","Tmp",
                          "Tmp2","dRopeStrengthDist","Totals","sRopeStrengthDist","RopeStrengthDist","Stage7d_Output","NumVerticalLines","Stage7s_Output",
                          "Stage7_OutputByMonth","Stage7_Output","Stage7_OutputTotal","Stage7_OutputByMonth_Agg","Stage7_OutputTotal_Agg","Stage7s_LineStrength_Px"))
      gc();
    }
  }  ## fold line diameter management
  
  #################################################################--
  ## 8.0 Merge line diameters with Threat
  if(Fold) {
    message("8 Calculating gear configuration threat")
    ## Line diameters are converted to Threat based on a model to be developed, possibly by polling the TRT.
    Stage8d=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength,
                      Stage7d, sum); 
    # summary(Stage8d)
    
    # Stage8d$GearPerString=round(Stage8d$GearPerString) ## deprecated V2.1.0
    # Stage8d$GearPerString[Stage8d$GearPerString==0]=1 ## deprecated V2.1.0
    # head(Stage8d)
    # dim(Stage8d)
    
    Stage8d=merge(Stage8d, ModRopeThreat, all.x=TRUE); 
    
    if(any(is.na(Stage8d$Threat))>0) { 
      message("Error, Some RopeStrengths not matched to Rope Threats in Stage8d");
      return(Stage8d);
      break();
    }
    
    Stage8d$ThreatScore=with(Stage8d, NumVerticalLinesAtStrength * Threat); 
    Stage8d$Scenario="Default"
    
    ### summary(Stage8d)
    if(PrintDefaultMaps){
      ## do mean threat for median, total threat for median and co-occurrence
      
      ############# mean Threat #############################--
      if(CoOccurrence){
        Tmp=Stage8d[Stage8d$ThreatMod=="CoOccurrence", c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
        
      }     else {
        Tmp=Stage8d[Stage8d$ThreatMod=="Threat", c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
      }
      Tmp2=aggregate(NumVerticalLinesAtStrength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "Index_LR", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$MeanThreat=with(Tmp, Threat * NumVerticalLinesAtStrength /Totals);
      
      ##################--
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Tmp,
        VarName="MeanThreat",
        Plot1_Title="Mean Individual Vertical Line Threat - Default",
        PlotLog=FALSE
      )
      
      Stage8d_MeanThreat_Px=OutputMapCall$Plot1_Px;
      map8dMeanThreat=OutputMapCall$Map1;
      
      ############ total Threat #########################--
      if(CoOccurrence){ ## V2.2.9
        Tmp=Stage8d[Stage8d$ThreatMod=="CoOccurrence" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )]; ## V2.2.9
      }     else { ## V2.2.9
        Tmp=Stage8d[Stage8d$ThreatMod=="Threat" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )]; ## V2.2.9
      } ## V2.2.9
      Tmp2=aggregate(Threat~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
      names(Tmp2)=c("Month", "Index_LR", "Totals")
      Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
      Tmp$TotalThreat=with(Tmp, NumVerticalLinesAtStrength * Threat/Totals);
      
      ### summary(Tmp)
      ###############--
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Tmp,
        VarName="TotalThreat",
        Plot1_Title="Aggregate Line Threat Score (Line Threat X Line Density) - Default",
        PlotLog=TRUE,
        LogOffset=0.01,
        Plot2_Title="Aggregate Line Threat Score (Line Threat X Line Density), Log-Scaled - Default"
      )
      
      Stage8d_TotalThreat_Px=OutputMapCall$Plot1_Px;
      map8dTotalThreat=OutputMapCall$Map1;
      Stage8d_TotalThreatLog_Px=OutputMapCall$Plot1_Px
      map8dTotalThreatLog=OutputMapCall$Map2
      
    } ## stage 8 default maps
    
    ########################## Stage 8 Scenario ##################################--
    if(TestScenario){
      
      Stage8s=aggregate(NumVerticalLinesAtStrength~Index+Index_LR+Month+RopeStrength,
                        Stage7s, sum); 
      
      # Stage8s$GearPerString=round(Stage8s$GearPerString) ## deprecated V2.1.0
      # Stage8s$GearPerString[Stage8s$GearPerString==0]=1 ## deprecated V2.1.0
      # head(Stage8s)
      # dim(Stage8s)
      
      # if(CoOccurrence==TRUE){ ## deprecated V2.1.0
      #   Stage8s$Threat=1 
      # } else {
      #   Stage8s=merge(Stage8s, ModRopeThreat)
      # };
      
      Stage8s=merge(Stage8s, ModRopeThreat, all.x=TRUE)
      
      if(any(is.na(Stage8s$Threat))>0) { 
        message("Error, Some RopeStrengths not matched to Rope Threats in Stage8s");
        return(Stage8d);
        break();
      }
      
      
      Stage8s$ThreatScore=with(Stage8s, NumVerticalLinesAtStrength * Threat); 
      ### summary(Stage8s)
      Stage8s$Scenario="Scenario"
      
      if(PrintScenarioMaps){
        ############# mean Threat #############################--
        if(CoOccurrence){
          Tmp=Stage8s[Stage8s$ThreatMod=="CoOccurrence" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
          
        }     else {
          Tmp=Stage8s[Stage8s$ThreatMod=="Threat" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
        }
        
        Tmp2=aggregate(NumVerticalLinesAtStrength~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "Index_LR", "Totals")
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$MeanThreat=with(Tmp, Threat * NumVerticalLinesAtStrength /Totals);
        
        ##################--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Tmp,
          VarName="MeanThreat",
          Plot1_Title="Mean Individual Vertical Line Threat - Scenario",
          PlotLog=FALSE
        )
        
        Stage8s_MeanThreat_Px=OutputMapCall$Plot1_Px;
        map8sMeanThreat=OutputMapCall$Map1;
        
        ############ total Threat #########################
        if(CoOccurrence){
          Tmp=Stage8s[Stage8s$ThreatMod=="CoOccurrence" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
          
        }     else {
          Tmp=Stage8s[Stage8s$ThreatMod=="Threat" ,c("Month", "Index_LR", "NumVerticalLinesAtStrength", "Threat" )];
        }
        Tmp2=aggregate(Threat~Month+Index_LR, Tmp, sum); ###summary(Tmp2)
        names(Tmp2)=c("Month", "Index_LR", "Totals")
        Tmp=merge(Tmp, Tmp2); ###summary(Tmp)
        Tmp$TotalThreat=with(Tmp, NumVerticalLinesAtStrength * Threat/Totals);
        
        ### summary(Tmp)
        ###############
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Tmp,
          VarName="TotalThreat",
          Plot1_Title="Aggregate Line Threat Score (Line Threat X Line Density) - Scenario",
          PlotLog=TRUE,
          LogOffset=0.01,
          Plot2_Title="Aggregate Line Threat Score (Line Threat X Line Density), Log-Scaled - Scenario"
        )
        
        Stage8s_TotalThreat_Px=OutputMapCall$Plot1_Px;
        map8sTotalThreat=OutputMapCall$Map1;
        Stage8s_TotalThreatLog_Px=OutputMapCall$Plot1_Px
        map8sTotalThreatLog=OutputMapCall$Map2
        
      } ## Stage 8 scenario maps
      
      if(ManageMemory){
        CleanUp(InputList=c("OutputMapCall","Months","Tmp","Tmp2",
                            "Stage8d_MeanThreat_Px","Stage8d_TotalThreat_Px","Stage8d_TotalThreatLog_Px", "ModRopeThreat"))
        gc();
      }
      
    }
    
    ############# Mean Threat Scores for Output ################################--
    #summary(Stage8d)
    # Tmp8d=Stage8d; 
    
    if(ManageMemory & !WriteDetailedOutput){CleanUp(InputList = c("Stage7d", "Stage7s"))};
    
    if(TestScenario){
      # Tmp8s=Stage8s; 
      
      ## mean threat score per Line
      Tmp8=rbind(Stage8d, Stage8s);
    } else {
      Tmp8=Stage8d;
    }
    
    if(!CoOccurrence) { ## if using empirical threat scores, build threat distribution plots; otherwise skip ## 
      
      # if(TestScenario){
      # ThreatDist1=Tmp8[ ,c("Scenario", "ThreatMod", "Month", "Threat", "NumVerticalLinesAtStrength")];
      # } else {
      #   ThreatDist1=Stage8d[ ,c("Scenario", "ThreatMod", "Month", "Threat", "NumVerticalLinesAtStrength")];
      # }
      
      # ThreatDist1=ThreatDist1[ThreatDist1$ThreatMod!="CoOccurrence", ]; dim(ThreatDist1) ## no point in plotting threat for co-occurrence
      
      # ThreatDist1$ThreatBin=round(ThreatDist1$Threat*20)/20; table(ThreatDist1$ThreatBin)
      Tmp8$ThreatBin=round(Tmp8$Threat*20)/20; table(Tmp8$ThreatBin)
      
      ThreatDist2=aggregate(NumVerticalLinesAtStrength~Scenario+ThreatMod+Month+ThreatBin, Tmp8, sum); # summary(ThreatDist2)
      # ThreatDist2=aggregate(NumVerticalLinesAtStrength~Scenario+ThreatMod+Month+ThreatBin, ThreatDist1, sum); # summary(ThreatDist2)
      names(ThreatDist2)[names(ThreatDist2)=="NumVerticalLinesAtStrength"]="Count"
      Totals=aggregate(Count~Scenario+ThreatMod+Month, ThreatDist2, sum);
      names(Totals)[names(Totals)=="Count"]="Total"
      ThreatDist2=merge(ThreatDist2, Totals)
      
      ThreatDist2=merge(
        expand.grid(
          Scenario=unique(ThreatDist2$Scenario),
          ThreatMod=unique(ThreatDist2$ThreatMod),
          Month=unique(ThreatDist2$Month),
          ThreatBin=unique(ThreatDist2$ThreatBin)
        ), ThreatDist2, all.x=TRUE);
      # summary(ThreatDist2)
      
      ThreatDist2$Count[is.na(ThreatDist2$Count)]=0;
      
      # ThreatDist2$Prop=with(ThreatDist2, Count/Total); ThreatDist2$Prop[is.na(ThreatDist2$Prop)]=0;
      
      ThreatDist2=ThreatDist2[order(ThreatDist2$ThreatBin), ]
      
      plotThreatDistributions=  
        xyplot(Count~ThreatBin|factor(Month), groups=Scenario, 
               data=ThreatDist2[ThreatDist2$ThreatMod=="Threat", ], 
               type="l", lwd=2, layout=c(4,3), 
               xlim=c(0,1),
               xlab="Threat Score", ylab="Number of Vertical Lines", main="Distribution of Threat Scores across Vertical Lines; Average Threat Model",
               par.settings=simpleTheme(col=c("blue", "green")), 
               scales=list(y=list(relation="free", rot=0)),
               auto.key=list(text=c("Default", "Scenario"), x=0.95, y=.90, corner=c(1,1), lwd=3, type="l")
        ); #plotThreatDistributions
      
      plotThreatDistributions_Lower=  
        xyplot(Count~ThreatBin|factor(Month), groups=Scenario, 
               data=ThreatDist2[ThreatDist2$ThreatMod=="Threat_Lower", ], 
               type="l", lwd=2,layout=c(4,3), 
               xlim=c(0,1),
               xlab="Threat Score", ylab="Number of Vertical Lines", main="Distribution of Threat Scores across Vertical Lines; Low-Contrast Threat Model",
               par.settings=simpleTheme(col=c("blue", "green")), 
               scales=list(y=list(relation="free", rot=0)),
               auto.key=list(text=c("Default", "Scenario"), x=0.95, y=.90, corner=c(1,1), lwd=3, type="l")
        ); #plotThreatDistributions_Lower
      
      plotThreatDistributions_Upper=  
        xyplot(Count~ThreatBin|factor(Month), groups=Scenario, 
               data=ThreatDist2[ThreatDist2$ThreatMod=="Threat_Upper", ], 
               type="l", lwd=2,layout=c(4,3), 
               xlim=c(0,1),
               xlab="Threat Score", ylab="Number of Vertical Lines", main="Distribution of Threat Scores across Vertical Lines; High-Constrast Threat Model",
               par.settings=simpleTheme(col=c("blue", "green")), 
               scales=list(y=list(relation="free", rot=0)),
               auto.key=list(text=c("Default", "Scenario"), x=0.95, y=.90, corner=c(1,1), lwd=3, type="l")
        ); #plotThreatDistributions_Upper
      
    } ## threat distribution plots
    
    
    Tmp8_Agg=aggregate(cbind(NumVerticalLinesAtStrength, ThreatScore)~Month+Scenario+ThreatMod, Tmp8, sum); #Tmp8_Agg
    Tmp8_Agg$MeanThreat=with(Tmp8_Agg, ThreatScore / NumVerticalLinesAtStrength); #Tmp8_Agg
    
    Tmp8_Agg_Total=aggregate(cbind(NumVerticalLinesAtStrength, ThreatScore)~Scenario+ThreatMod, Tmp8, sum); #Tmp8_Agg_Total
    Tmp8_Agg_Total$MeanThreat=with(Tmp8_Agg_Total, ThreatScore / NumVerticalLinesAtStrength); #Tmp8_Agg_Total
    Tmp8_Agg_Total$Month="Total"
    
    Stage8_MeanThreat=rbind(Tmp8_Agg, Tmp8_Agg_Total)
    Stage8_MeanThreat$Variable=paste0("MeanLineThreat_", Stage8_MeanThreat$ThreatMod)
    Stage8_MeanThreat=Stage8_MeanThreat[ ,c("Variable", "Scenario", "Month", "MeanThreat")]; #
    names(Stage8_MeanThreat)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage8_MeanThreat); #OutputData
    
    ####### Total Line threat ####################################################--
    Tmp8_Agg=aggregate(ThreatScore~Month+Scenario+ThreatMod, Tmp8, sum); #Tmp8_Agg
    names(Tmp8_Agg)=c("Month", "Scenario", "ThreatMod", "TotalThreat")
    
    Tmp8_Agg_Total=aggregate(ThreatScore~Scenario+ThreatMod, Tmp8, sum); #Tmp8_Agg_Total
    names(Tmp8_Agg_Total)=c("Scenario", "ThreatMod", "TotalThreat")
    Tmp8_Agg_Total$Month="Total"
    
    Stage8_TotalThreat=rbind(Tmp8_Agg, Tmp8_Agg_Total)
    Stage8_TotalThreat$Variable=paste0("TotalGearThreat_", Stage8_TotalThreat$ThreatMod)
    Stage8_TotalThreat=Stage8_TotalThreat[ ,c("Variable", "Scenario", "Month", "TotalThreat")]; #
    names(Stage8_TotalThreat)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage8_TotalThreat); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall","MapRef_CrI","MapRef_HR_I","Months","Tmp","Tmp2","Stage8d_MeanThreat_Px","Stage8d_TotalThreat_Px",
                          "Stage8d_TotalThreatLog_Px","Totals","Tmp8","ThreatDist2","Tmp8_Agg","Tmp8_Agg_Total","Stage8_MeanThreat","Stage8_TotalThreat","Stage8s_MeanThreat_Px",
                          "Stage8s_TotalThreat_Px","Stage8s_TotalThreatLog_Px"))
      gc();
    }
    
    
  } ## Threat
  
  #############################################################--
  ## 9. Calculate Risk
  if(Fold) {
    message("9 Calculating composite risk values")
    ## Risk is calculated as the product of Threat and whale presence.
    WhaleModel=aggregate(WhaleDensity~Month+Index+Index_LR, WhaleDensityModel, mean) ## aggregate to 10Nm
    names(WhaleModel)=c("Month", "Index", "Index_LR", "WhaleDensity")
    WhaleModel_Map=WhaleModel ## create copy for mapping     
    
    ##
    if(Fold){ ## fold whale habitat mapping 
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=WhaleModel_Map,
        VarName="WhaleDensity",
        Plot1_Title="Whale Habitat Score",
        PlotLog=TRUE,
        LogOffset=10^(-10),
        Plot2_Title="Whale Habitat Score, Log-Scaled"
      )
      
      Stage9_WhaleHabitat_Px=OutputMapCall$Plot1_Px;
      map9WhaleHabitat=OutputMapCall$Map1;
      Stage9_WhaleHabitatLog_Px=OutputMapCall$Plot1_Px
      map9WhaleHabitatLog=OutputMapCall$Map2
      
      #######################
      # WhaleOutput=aggregate(WhaleDensity~Month, WhaleModel, sum); ## code moved to 0.2
      # WhaleTotal=aggregate(WhaleDensity~1, WhaleModel, sum); #
      # WhaleTotal$Month="Total"
      # WhaleOutput=rbind(WhaleOutput, WhaleTotal); #
      # WhaleOutput$Scenario="Default"
      # 
      # WhaleOutput$Variable="WhaleDensity"
      # WhaleOutput=WhaleOutput[ ,c("Variable", "Scenario", "Month", "WhaleDensity")]; #
      # names(WhaleOutput)=c("Variable", "Scenario", "Month", "Value")
      # 
      # OutputData=rbind(OutputData, WhaleOutput); #OutputData
      
      
    } ## fold whale habitat mapping 
    
    ############# merge gear threats with whale habitat model ###########################--
    Stage9d=aggregate(ThreatScore~Index+Index_LR+ThreatMod+Month, Stage8d, sum); ###summary(Stage9d)
    
    Stage9d=merge(Stage9d, WhaleModel, all.x=TRUE); 
    ### summary(Stage9d)
    if(length(which(is.na(Stage9d$WhaleDensity)))>0){
      message(
        paste("Warning: ", 
              length(which(is.na(Stage9d$WhaleDensity))), 
              " of ", 
              nrow(Stage9d), 
              " records in Stage9d do not have co-located Whale Habitat values", sep="")
      )
      message( "   Some mismatches are expected with the default Duke_v8 whale model if inshore habitats are included")
    }
    Stage9d$WhaleDensity[is.na(Stage9d$WhaleDensity)]=0
    # Stage9d$WhaleDensity[is.na(Stage9d$WhaleDensity)]=0
    # plot(quantile(Stage9d$WhaleDensity, (0:1000)/1000), ylim=c(0,1))
    # plot(quantile(Stage9d$WhaleDensityLog, (0:1000)/1000))
    
    Stage9d$Risk=with(Stage9d, WhaleDensity * ThreatScore); ###summary(Stage9d)
    Stage9d$Risk=Stage9d$Risk+10^(-10)
    Stage9d$RiskLog=log10(Stage9d$Risk)
    
    # plot(sort(log10(Stage9d$Risk)))
    ### summary(Stage9d)
    
    if(PrintDefaultMaps){
      ############### Risk Values ####################--
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Stage9d[Stage9d$ThreatMod=="Threat", ],
        VarName="Risk",
        Plot1_Title="Total Risk Score - Default",
        PlotLog=TRUE,
        LogOffset=0.1,
        Plot2_Title="Total Risk Score, Log-Scaled - Default"
      )
      
      Stage9d_RiskScore_Px=OutputMapCall$Plot1_Px;
      map9dRiskScore=OutputMapCall$Map1;
      Stage9d_RiskScoreLog_Px=OutputMapCall$Plot1_Px
      map9dRiskScoreLog=OutputMapCall$Map2
      
      ## CoOccurrence
      OutputMapCall=OutputMap(         
        PlotDF=PlotDF,         
        MapRef=MapRef_LR,         
        Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
        InputDF=Stage9d[Stage9d$ThreatMod=="CoOccurrence", ],
        VarName="Risk",
        Plot1_Title="Total Risk Score - Default",
        PlotLog=TRUE,
        LogOffset=0.1,
        Plot2_Title="Total Risk Score, Log-Scaled - Default"
      )
      
      Stage9d_RiskScore_Px_CoOc=OutputMapCall$Plot1_Px;
      map9dRiskScore_CoOc=OutputMapCall$Map1;
      Stage9d_RiskScoreLog_Px_CoOc=OutputMapCall$Plot1_Px
      map9dRiskScoreLog_CoOc=OutputMapCall$Map2
      
    } ## stage 9 default maps
    
    ################## Stage 9 Scenario ############################################--
    ## option would be to use all=TRUE and pick up pixels missing in both sets and assume=0
    if(TestScenario){
      Stage9s=aggregate(ThreatScore~Index+Index_LR+ThreatMod+Month, Stage8s, sum); ###updated code for V1.6.2
      Stage9s=merge(Stage9s, WhaleModel, all.x=TRUE); 
      if(length(which(is.na(Stage9s$WhaleDensity)))>0){
        message(
          paste("Warning: ", 
                length(which(is.na(Stage9s$WhaleDensity))), 
                " of ", 
                nrow(Stage9s), 
                " records in Stage9s do not have co-located Whale Habitat values.", sep=""))
        message( "   Some mismatches are expected with the default Duke_v8 whale model if inshore habitats are included")
      }
      
      ### summary(Stage9s)
      Stage9s$ThreatScore[is.na(Stage9s$ThreatScore)]=0
      # Stage9s$WhaleDensity[is.na(Stage9s$WhaleDensity)]=0
      # plot(quantile(Stage9s$WhaleDensity, (0:1000)/1000), ylim=c(0,1))
      Stage9s$WhaleDensity=Stage9s$WhaleDensity+10^(-10)
      # Stage9s$WhaleDensityLog=log10(Stage9s$WhaleDensity)
      # plot(quantile(Stage9s$WhaleDensityLog, (0:1000)/1000))
      
      Stage9s$Risk=with(Stage9s, WhaleDensity * ThreatScore); ###summary(Stage9s)
      Stage9s$Risk=Stage9s$Risk+10^(-10)
      Stage9s$RiskLog=log10(Stage9s$Risk)
      
      # plot(sort(log10(Stage9s$Risk)))
      ### summary(Stage9s)
      
      if(PrintScenarioMaps){
        
        ############### Risk Values ####################--
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Stage9s[Stage9s$ThreatMod=="Threat",],
          VarName="Risk",
          Plot1_Title="Total Risk Score - Scenario, Gear Threat",
          PlotLog=TRUE,
          LogOffset=0.1,
          Plot2_Title="Total Risk Score, Log-Scaled - Scenario, Gear Threat"
        )
        
        Stage9s_RiskScore_Px=OutputMapCall$Plot1_Px;
        map9sRiskScore=OutputMapCall$Map1;
        Stage9s_RiskScoreLog_Px=OutputMapCall$Plot1_Px
        map9sRiskScoreLog=OutputMapCall$Map2
        
        ######## CoOccurrence 
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=Stage9s[Stage9s$ThreatMod=="CoOccurrence",],
          VarName="Risk",
          Plot1_Title="Total Risk Score - Scenario, CoOccurrence",
          PlotLog=TRUE,
          LogOffset=0.1,
          Plot2_Title="Total Risk Score, Log-Scaled - Scenario, CoOccurrence"
        )
        
        Stage9s_RiskScore_Px_CoOc=OutputMapCall$Plot1_Px;
        map9sRiskScore_CoOc=OutputMapCall$Map1;
        Stage9s_RiskScoreLog_Px_CoOc=OutputMapCall$Plot1_Px
        map9sRiskScoreLog_CoOc=OutputMapCall$Map2
        
        ## Change in Risk Maps ########################################################################
        
        DefaultRisk=aggregate(Risk~Index_LR+Month, Stage9d[Stage9d$ThreatMod=="Threat",], sum); names(DefaultRisk)=c("Index_LR", "Month", "Default")
        ScenarioRisk=aggregate(Risk~Index_LR+Month, Stage9s[Stage9s$ThreatMod=="Threat",], sum); names(ScenarioRisk)=c("Index_LR", "Month", "Scenario")
        CombinedRisk=merge(DefaultRisk, ScenarioRisk, all.x=TRUE); # summary(CombinedRisk)
        CombinedRisk$Scenario[is.na(CombinedRisk$Scenario)]=0;
        CombinedRisk$Reduction=CombinedRisk$Default - CombinedRisk$Scenario; # summary(CombinedRisk); 
        CombinedRisk$PropReduction=with(CombinedRisk, Reduction / Default); # summary(CombinedRisk);
        CombinedRisk$PropReduction[CombinedRisk$Default<0.001]=0
        
        ## Change in absolute risk units
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=CombinedRisk,
          VarName="Reduction",
          Plot1_Title="Total Risk Reduction",
          PlotLog=FALSE
        )
        
        Stage9_RiskReduction_Px=OutputMapCall$Plot1_Px;
        map9RiskReduction=OutputMapCall$Map1;
        
        ## local proportional change in risk units
        OutputMapCall=OutputMap(         
          PlotDF=PlotDF,         
          MapRef=MapRef_LR,         
          Sp.Layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2),
          InputDF=CombinedRisk,
          VarName="PropReduction",
          Plot1_Title="Proportional Risk Reduction",
          PlotLog=FALSE
        )
        
        Stage9_PropRiskReduction_Px=OutputMapCall$Plot1_Px;
        map9PropRiskReduction=OutputMapCall$Map1;
        
        ###################################################################--
        # Ymin=max(Ymin, 1); Stage9s$RiskLog[Stage9s$RiskLog<=Ymin]=NA
        # Yseq=Ymin:Ymax; Yval=10^Yseq; Yval
        # 
        # Stage9Log_Wide=Long2Wide(Stage9s[ ,c("Index_LR", "Month", "RiskLog")],
        #                          fName="Month", vName="RiskLog", Prefix="m_"); ###summary(Stage9Log_Wide)
        # names(Stage9Log_Wide)=c("Index_LR", MonthString); ###summary(Stage9Log_Wide)
        # 
        # if(any(colSums(Stage9Log_Wide[, 2:13], na.rm=TRUE)==0)) {
        #   message("Insufficient Values to Plot Abridged Risk for Scenario")
        # } else {
        #   Stage9s_RiskScoreLog_Abridged_Px=merge(MapRef_LR[ ,c("Index_LR")], Stage9Log_Wide); ###summary(Stage9Log_Px)
        #   
        #   map9sRiskScoreLog_Abridged=
        #     spplot(Stage9s_RiskScoreLog_Abridged_Px[MonthOrder],
        #            Cuts=100, 
        #            sp.layout=list(spCoast_layout1, spCoast_layout2, spLMA_layout1, spLMA_layout2, spStatAreas_layout1, spStatAreas_layout2 ),
        #            colorkey=list(labels=list(labels=Yval, at=Yseq)),
        #            main="Total Risk Score, Log-Scaled - Abridged",
        #            layout=c(4,3)
        #     ); ###map9sRiskScoreLog_Abridged
        #   
        # };
      } ## stage 9 scenario maps
    }
    
    ## output plots for future version
    
    # sub9s=aggregate(Risk~Index_LR+Month+ThreatMod, Stage9s[ ,c("Index_LR", "Month", "ThreatMod", "Risk")], sum); summary(sub9s)
    # names(sub9s)=c("Index_LR", "Month", "ThreatMod", "sRisk")
    # sub9d=aggregate(Risk~Index_LR+Month+ThreatMod, Stage9d[ ,c("Index_LR", "Month", "ThreatMod", "Risk")], sum); summary(sub9d)
    # names(sub9d)=c("Index_LR", "Month", "ThreatMod", "dRisk")
    # 
    # ByRisk=merge(sub9s, sub9d); summary(ByRisk)
    # ByRisk$ThreatMod=factor(ByRisk$ThreatMod, levels=c("Threat_Lower", "Threat_Upper", "CoOccurrence", "Threat"))
    # 
    # xyplot(sRisk~dRisk|ThreatMod, ByRisk, 
    #        layout=c(2,2), pch=20,
    #        xlab="Default Risk Values",
    #        ylab="Scenario Risk Values",
    #        scales=list(y=list(relation="free"),
    #                    x=list(relation="free")),
    #        panel=function(x,y,...){
    #          panel.xyplot(x,y)
    #          panel.abline(0,1)
    #        })
    
    ###############################################################################--
    Stage9dOutput=aggregate(Risk~ThreatMod+Month, Stage9d, sum); #Stage9dOutput
    Stage9dTotal=aggregate(Risk~ThreatMod, Stage9dOutput, sum); #Stage9dTotal
    Stage9dTotal$Month="Total"
    Stage9dOutput=rbind(Stage9dOutput, Stage9dTotal); #Stage9dOutput
    Stage9dOutput$Scenario="Default"
    
    if(TestScenario){
      Stage9sOutput=aggregate(Risk~ThreatMod+Month, Stage9s, sum); #Stage9sOutput
      Stage9sTotal=aggregate(Risk~ThreatMod, Stage9sOutput, sum); #Stage9sTotal
      Stage9sTotal$Month="Total"
      Stage9sOutput=rbind(Stage9sOutput, Stage9sTotal); #Stage9sOutput
      Stage9sOutput$Scenario="Scenario"
      
      Stage9Output=rbind(Stage9dOutput, Stage9sOutput); #Stage9Output
    } else {
      Stage9Output=Stage9dOutput; #Stage9Output
    }    
    
    Stage9Output$Variable=paste0("RelativeRisk_", Stage9Output$ThreatMod)
    Stage9Output=Stage9Output[ ,c("Variable", "Scenario", "Month", "Risk")]; #Stage9Output
    names(Stage9Output)=c("Variable", "Scenario", "Month", "Value")
    
    OutputData=rbind(OutputData, Stage9Output); #OutputData
    
    if(ManageMemory){
      CleanUp(InputList=c("OutputMapCall","WhaleModel","WhaleModel_Map","WhaleOutput","WhaleTotal","DefaultRisk","ScenarioRisk","CombinedRisk",
                          "Stage9dOutput","Stage9dTotal","Stage9Output","Stage9_PropRiskReduction_Px","Stage9_RiskReduction_Px","Stage9_WhaleHabitat_Px",
                          "Stage9_WhaleHabitatLog_Px","Stage9s_RiskScore_Px","Stage9s_RiskScore_Px_CoOc","Stage9s_RiskScoreLog_Px",
                          "Stage9s_RiskScoreLog_Px_CoOc","Stage9sOutput","Stage9sTotal"))
      gc();
    };
    
    if(ManageMemory & !WriteDetailedOutput){
      CleanUp(InputList = c("Stage8d", "Stage8s", "Stage9d", "Stage9s"))
    }
    
    
  } ## Whale habitat and risk maps
  
  #####################################################################--
  ## 10. Create output directory and write maps and files
  
  ScenariosDir=(paste(HD, "Scenarios", sep="/"));
  setwd(ScenariosDir)
  ## create and switch to subfolder if necessary
  if(HasSubfolder){ ## V2.2.5
    if(!dir.exists(Subfolder)) {dir.create(Subfolder)}
    setwd(Subfolder);
  }
  if(!dir.exists(OutputDir)) {dir.create(OutputDir)}
  setwd(OutputDir)

    WriteDir=getwd(); #2.2.6
  #######################################################################################################--
  
  OutputData_Wide=Long2Wide(OutputData, fName="Scenario", vName="Value"); ## OutputData_Wide
  
  if(TestScenario){
    OutputData_Wide$Reduction=with(OutputData_Wide, 1-Scenario/Default); ## OutputData_Wide
  } else {
    OutputData_Wide$Reduction=NA; ## OutputData_Wide
  } 
  # OutputData_Wide$Reduction[OutputData_Wide$Variable=="RopeDiameter"]=NA
  #######################################################################################################--
  
  ## plots to add
  # GearRemoved_Px
  # GearRemovedLog_Px
  # GearCapPercentRemoved_Px
  # GearRemov_GearDensity_Px
  # GearRemov_GearDensityLog_Px
  
  if(Fold) { ## writing output 
    message("Writing output")
    
    if(PrintDefaultMaps){
      message("Writing Default Maps")
      
      pdf(file=paste(OutputDir, "_DefaultFigures.pdf", sep=""),
          width=11,height=9.5,paper="special")
      plot(map1dGearDensity);
      plot(map1dGearDensityLog);
      
      # plot(map2dGearDensity); ## post Gear caps; redundant with map1d
      # plot(map2dGearDensityLog); ## post Gear caps
      
      # plot(map3dGearDensity); ## post closure
      # plot(map3dGearDensityLog); ## post closure
      
      plot(map4dStringLength);
      
      plot(map5dLineDensity);
      plot(map5dLineDensityLog);
      
      plot(map6dLineStrength);
      
      plot(map8dMeanThreat);
      plot(map8dTotalThreat);
      plot(map8dTotalThreatLog);
      
      plot(map9WhaleHabitat);
      plot(map9WhaleHabitatLog);
      
      plot(map9dRiskScore);
      plot(map9dRiskScoreLog);
      plot(map9dRiskScore_CoOc);
      plot(map9dRiskScoreLog_CoOc);
      
      if(exists("map9dRiskScoreLog_Abridged")) {plot(map9dRiskScoreLog_Abridged)}
      
      dev.off()
      
      ## save maps
      save(
        map1dGearDensity,
        map1dGearDensityLog,
        map4dStringLength,
        
        map5dLineDensity,
        map5dLineDensityLog,
        
        map6dLineStrength,
        
        map8dMeanThreat,
        map8dTotalThreat,
        map8dTotalThreatLog,
        
        map9WhaleHabitat,
        map9WhaleHabitatLog,
        
        map9dRiskScore,
        map9dRiskScoreLog,
        
        file=paste(OutputDir, "_DefaultMaps.Rdata", sep="")
      )
      
    } ## print default maps
    
    if(!CoOccurrence){ ## print threat model uncertainty 
      pdf(file=paste(OutputDir, "_ThreatDistributions.pdf", sep=""),
          width=11,height=9.5,paper="special")
      
      plot(plotRopeStrengthDistribution); 
      plot(plotThreatDistributions);
      plot(plotThreatDistributions_Lower);
      plot(plotThreatDistributions_Upper);
      dev.off()
    }
    
    if(TestScenario & PrintScenarioMaps){
      message("Writing Scenario Maps")
      
      pdf(file=paste(OutputDir, "_ScenarioFigures.pdf", sep=""),
          width=11,height=9.5,paper="special")
      
      plot(map1sGearDensity); ## post reduction
      plot(map1sGearDensityLog);
      
      plot(map2sGearDensity); ## post Gear caps
      plot(map2sGearDensityLog); ## post Gear caps
      
      if(exists("map2sGearRemoved")){
        plot(map2sGearRemoved); ## post Gear caps
        plot(map2sGearRemovedLog); ## post Gear caps
        plot(map2sGearCapPercentRemoved); ## percent reduction
      }
      
      if(exists("map3sGearRemovedGearDensity")){
        plot(map3sGearRemovedGearDensity);
        plot(map3sGearRemovedGearDensityLog);
      }
      
      plot(map3sGearDensity_PostClosure); ## post closures
      plot(map3sGearDensity_PostClosureLog); ## post closures
      
      plot(map4sStringLength);
      
      plot(map5sLineDensity);
      plot(map5sLineDensityLog);
      
      plot(map7sLineStrength);
      
      plot(map8sMeanThreat);
      plot(map8sTotalThreat);
      plot(map8sTotalThreatLog);
      plot(map9WhaleHabitat);
      plot(map9WhaleHabitatLog);
      plot(map9sRiskScore_CoOc);
      plot(map9sRiskScoreLog_CoOc);
      
      plot(map9sRiskScore);
      plot(map9sRiskScoreLog);
      
      plot(map9RiskReduction);
      plot(map9PropRiskReduction);
      
      if(exists("map9sRiskScoreLog_Abridged")){plot(map9sRiskScoreLog_Abridged)};
      
      dev.off()
      
      ## save maps
      save(
        map1sGearDensity,
        map1sGearDensityLog,
        
        map3sGearDensity_PostClosure, ## post closures
        map3sGearDensity_PostClosureLog, ## post closures
        
        map4sStringLength,
        
        map5sLineDensity,
        map5sLineDensityLog,
        
        map7sLineStrength,
        
        map8sMeanThreat,
        map8sTotalThreat,
        map8sTotalThreatLog,
        map9WhaleHabitat,
        map9WhaleHabitatLog,
        map9sRiskScore,
        map9sRiskScoreLog,
        map9sRiskScore_CoOc,
        map9sRiskScoreLog_CoOc,
        
        map9RiskReduction,
        map9PropRiskReduction,
        
        # map9sRiskScoreLog_Abridged,
        
        file=paste(OutputDir, "_ScenarioMaps.Rdata", sep="")
      )
      
    } ## print scenario maps
    
    if(exists("map3sRedistributedGearDensity")) {
      message("Writing Gear Redistribution Maps")
      
      pdf(file=paste(OutputDir, "_GearRedistributionFigures.pdf", sep=""),
          width=11,height=9.5,paper="special")
      
      plot(map3sRedistributedGearDensity);
      plot(map3sRedistributedGearDensityLog);
      
      if(exists("map3sGearRemovedGearDensity")) {
        plot(map3sGearRemovedGearDensity);
        plot(map3sGearRemovedGearDensityLog);
      }
      dev.off()
    }
    
    if(PrintTables){
      message("Writing Tables")
      
      pdf(file=paste(OutputDir, "_Tables.pdf", sep=""),
          width=15, height=8.5, paper="special")
      
      ## Enter Model configuration ##
      grid.draw(PrintTable(Tbl=ModelConfiguration,
                           Title="",
                           TitleFont=14)); grid.newpage()
      
      ## prior to 2.2.6       # if(nrow(ScenarioInputs)>0){
      #   SI=ScenarioInputs
      #   for(i in 1:ncol(SI)){
      #     SI[ ,i][is.na(SI[ ,i])]=""
      #   }
      #   grid.draw(PrintTable(Tbl=ScenarioInputs,
      #                        Title="Input Scenario Spreadsheet",
      #                        TitleFont=14)); grid.newpage()
      # }

      ## 2.2.6 
      SI=ScenarioInputs
        for(i in 1:ncol(SI)){
          SI[ ,i][is.na(SI[ ,i])]=""
        }
      grid.draw(PrintTable(Tbl=ScenarioInputs,
                           Title="Input Scenario Spreadsheet",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="GearFished_PostReduction", ], Digits=0),
                           Title="Gear Numbers - Post Reductions",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="GearFished_PostCap", ], Digits=0),
                           Title="Gear Numbers - Post GearCaps",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="GearFished_PostClosure", ], Digits=0),
                           Title="Gear Numbers - Post Closure",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="NumStrings", ], Digits=0),
                           Title="Total Strings",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="MeanStringLength", ], Digits=2),
                           Title="Mean String Length (Gear per String)",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="NumVerticalLines", ], Digits=0),
                           Title="Total Vertical Lines",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="RopeStrength", ], Digits=3),
                           Title="Mean Rope Strength",
                           TitleFont=14)); grid.newpage()
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="MeanLineThreat_Threat", ], Digits=2),
                             Title="Mean Threat Score per Vertical Line",
                             TitleFont=14)); grid.newpage()
      }
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="TotalGearThreat_CoOccurrence", ], Digits=0),
                           Title="Total Gear Threat Score - CoOccurrence",
                           TitleFont=14)); grid.newpage()
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="TotalGearThreat_Threat", ], Digits=0),
                             Title="Total Gear Threat Score - Mean Threat",
                             TitleFont=14)); grid.newpage()
      }
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="TotalGearThreat_Threat_Lower", ], Digits=0),
                             Title="Total Gear Threat Score - Threat Lower Bound",
                             TitleFont=14)); grid.newpage()
      }
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="TotalGearThreat_Threat_Upper", ], Digits=0),
                             Title="Total Gear Threat Score - Threat Upper Bound",
                             TitleFont=14)); grid.newpage()
      }
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="WhaleDensity", ], Digits=0),
                           Title="Total Whale Density",
                           TitleFont=14)); grid.newpage()
      
      grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk_CoOccurrence", ], Digits=0),
                           Title="Final Relative Risk Scores - CoOccurrence",
                           TitleFont=14)); grid.newpage()
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk_Threat", ], Digits=0),
                             Title="Final Relative Risk Scores - Mean Threat",
                             TitleFont=14)); grid.newpage()
      }
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk_Threat_Lower", ], Digits=0),
                             Title="Final Relative Risk Scores - Threat Lower Bound",
                             TitleFont=14)); grid.newpage()
      }
      
      if(!CoOccurrence){
        grid.draw(PrintTable(Tbl=TableFormat(OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk_Threat_Upper", ], Digits=0),
                             Title="Final Relative Risk Scores - Threat Upper Bound",
                             TitleFont=14)); 
      }
      dev.off()
      
    }  ## print tables
    
    ### Add list of maps
    
    if(WriteMapSources){
      message("Writing Map Sources")
      if(PrintDefaultMaps){
        save(
          Stage1d_GearDensity_Px,
          Stage1d_GearDensityLog_Px,
          
          Stage4d_StringLength_Px,
          Stage4s_StringLength_Px,
          
          Stage5d_LineDensity_Px,
          Stage5d_LineDensityLog_Px,
          
          ## no output from Stage5 yet
          
          Stage6d_LineStrength_Px,
          # Stage6s_LineDia_Px,
          
          ## no output from Stage7 yet
          
          Stage8d_MeanThreat_Px,
          Stage8d_TotalThreat_Px,
          Stage8d_TotalThreatLog_Px,
          
          Stage9_WhaleHabitat_Px,
          Stage9_WhaleHabitatLog_Px,
          Stage9d_RiskScore_Px,
          Stage9d_RiskScoreLog_Px,
          
          Stage9d_RiskScore_Px_CoOc,
          Stage9d_RiskScoreLog_Px_CoOc,
          
          # Stage9d_RiskScoreLog_Abridged_Px,
          
          file=paste(OutputDir, "_MapSources_Default.Rdata", sep="")
        )
      } 
      
      ## to be added    
      # GearRemoved_Px
      # GearRemovedLog_Px
      # GearCapPercentRemoved_Px
      
      
      if(PrintScenarioMaps){
        
        save(
          Stage1s_GearDensity_Px,
          Stage1s_GearDensityLog_Px,
          
          Stage2s_GearDensity_Px,
          Stage2s_GearDensityLog_Px,
          
          # Stage3s_GearDensity_Px,## commented V2.2.7
          # Stage3s_GearDensityLog_Px,## commented V2.2.7
          Stage3s_GearDensity_PostClosure_Px,## V2.2.7
          Stage3s_GearDensity_PostClosureLog_Px,## V2.2.7
          
          Stage4s_StringLength_Px,
          
          Stage5s_LineDensity_Px,
          Stage5s_LineDensityLog_Px,
          
          ## no output from Stage5 yet
          
          # Stage6s_LineStrength_Px, ## commented V2.2.7
          Stage7s_LineStrength_Px, ## V2.2.7
          
          ## no output from Stage7 yet
          
          Stage8s_MeanThreat_Px,
          Stage8s_TotalThreat_Px,
          Stage8s_TotalThreatLog_Px,
          
          Stage9_WhaleHabitat_Px,
          Stage9_WhaleHabitatLog_Px,
          Stage9s_RiskScore_Px,
          Stage9s_RiskScoreLog_Px,
          
          Stage9s_RiskScore_Px_CoOc,
          Stage9s_RiskScoreLog_Px_CoOc,
          
          Stage9_RiskReduction_Px,
          Stage9_PropRiskReduction_Px,
          
          # Stage9s_RiskScoreLog_Abridged_Px,
          
          file=paste(OutputDir, "_MapSources_Scenario.Rdata", sep="")
        )
        
        if(exists("GearCapPercentRemoved_Px")){
          save(GearCapPercentRemoved_Px, 
               file=paste(OutputDir, "_MapSources_Scenario_GearCapRemoved.Rdata", sep=""))
        }
        
        
        if(exists("Redist_GearDensity_Px")){
          save(Redist_GearDensity_Px, Redist_GearDensityLog_Px, 
               file=paste(OutputDir, "_MapSources_Scenario_RedistGear.Rdata", sep=""))
        }
        
        if(exists("GearRemov_GearDensity_Px")){
          save(GearRemov_GearDensity_Px, GearRemov_GearDensityLog_Px, 
               file=paste(OutputDir, "_MapSources_Scenario_RemovedGear.Rdata", sep=""))
        }
        
        
      } 
      
    }
    
    if(WriteOutputCsv) {
      message("Writing Output to .csv")
      
      write.csv(x=OutputData_Wide,
                file=paste(OutputDir, "_OutputData.csv", sep=""),
                row.names = FALSE)
    }  
    
    if(WriteDetailedOutput){
      message("Writing Extended Output")
      if(TestScenario){
        save(Stage1d, Stage1s,
             Stage2d, Stage2s,
             Stage3d, Stage3s,
             Stage4d, Stage4s,
             Stage5d, Stage5s,
             Stage7d, Stage7s,
             Stage8d, Stage8s,
             Stage9d, Stage9s,
             WhaleDensityModel,
             file=paste0(OutputDir, "_ExtendedOutput.Rdata"), 
             version=2)
      } else {
        save(Stage1d, 
             Stage2d, 
             Stage3d, 
             Stage4d, 
             Stage5d, 
             Stage7d, 
             Stage8d, 
             Stage9d, 
             WhaleDensityModel,
             file=paste0(OutputDir, "_ExtendedOutput.Rdata"), 
             version=2)
      }
    }
    
    if(ArchiveInputSpreadsheet){ ## V2.2.6
      SuccessfulCopy=try(
        file.copy(
          from =paste(HomeDir, "InputSpreadsheets", ## subdirectory
                      InputSpreadsheetName, ## file name
                      sep="/"),
          to=paste(WriteDir, InputSpreadsheetName, sep="/"),
          overwrite = TRUE
        )
      );
      if(SuccessfulCopy){
        file.remove(paste(HomeDir, "InputSpreadsheets", ## subdirectory
                        InputSpreadsheetName, ## file name
                        sep="/"))
      };
    }
    
  } ## writing output 
    
    if(PrintSummary){
      print(paste("Input Spreadsheet: ", InputSpreadsheetName));
      MeanThreatReduction=OutputData_Wide[OutputData_Wide$Variable=="RelativeRisk_Threat", ]
      MeanThreatReduction$Default=round(MeanThreatReduction$Default, 2);
      MeanThreatReduction$Scenario=round(MeanThreatReduction$Scenario, 2);
      MeanThreatReduction$Reduction=round(MeanThreatReduction$Reduction, 3);
      
      # message(MeanThreatReduction);
    }

    message(paste0("Model run completed. Runtime ", ## V2.3.2
                 round((as.numeric(Sys.time() - as.numeric(StartTime)))/60,2), " minutes"
    )  )
    
    
} ## end function


