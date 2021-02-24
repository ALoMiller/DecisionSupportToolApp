  rm(list=ls())
  
  ## set working directory
  # HD="//net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool"; setwd(HD)
  HD=here::here()
  ## source decision support function
  Version="DecisionSupportTool_V3.0.2.R"
  source(paste0(HD, "/function_", Version))
  
  ## call function
  Output=DecisionTool(
    HomeDir=HD, ## home directory for subdirectories
    ModelVersion=Version, ## model version
    
    ################## Select core inputs #####################################--
    InputSpreadsheetName="ScenarioTemplate_V3.0.0.csv", ## csv input file with specified criteria
    
    
    ## specify MapRef domain
    # MapRefDomain="MapRef_HR_V2.2.Rdata",
    MapRefDomain="MapRef_HR_Lobster_V3.0.0.Rdata",
    
    ## specify input data
    # GearMapName="GearMap_Lobster_V3.0.0.Rdata", ## default Gear map
    GearMapName="GearMap_Lobster_MassRMA_V3.0.0.Rdata", ## default Gear map with fishing in the cape cod closure
    
    ## TrawlLengthModel not specified for V2.2+
    
    # RopeStrengthModelName="LineStrengthModel_V2.0.Rdata", ## rope strength as a function of trawl length
    RopeStrengthModelName="LineStrengthModel_V2.1_60TrapThreshold.Rdata", ## rope strength as a function of trawl length; All trawls>35 traps being equal
  
    ThreatModel="ThreatMod_RW_Selectivity_Uncertainty.Rdata", ## gear threat model
    
    # WhaleInputModel="Duke_HumpbackWhaleModel_v10_DSTv3.Rdata", ##################### V3 ######################--
    # WhaleInputModel="Duke_HumpbackWhaleModel_v10_DSTv3_Expanded.Rdata", ##################### V3 ######################--
    # WhaleInputModel="Duke_RightWhaleModel_v10_0309.Rdata", ## only compatible with GearMap v3+
    # WhaleInputModel="Duke_RightWhaleModel_v10_0318.Rdata", ## only compatible with GearMap v3+
    WhaleInputModel="Duke_RightWhaleModel_v10_1018.Rdata", ## only compatible with GearMap v3+
    
    ################# select model options ################--
    CommentText="PrefAlt_MassRMA_v10_DST3_TotalMARiskReduction",
    
    TestScenario=TRUE, ## run a test scenario or only a default baseline?
    CoOccurrence=FALSE, ## run co-occurrence only; no gear threat model (speeds up model run time)
  
    HighResolution=FALSE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
  
    # AggregateTrawls=FALSE, ## option to aggregat trawl length distributions within vessel classes to decrease model run time.
    
    RelocationCostExp=1, ## Exponent applied to distance for calculate relocation cost around closures
    ExpressRedistribution=TRUE, ## spatially aggregate Gears in closures to decrease runtime with some loss of resolution
    
    UpdateEndlineStrengths=FALSE, ## Option to not recalculate endline strengths when changing trawl lengths as a result of a trawling up action
    
    RopeStrengthResolution=500, ## binning for rope strength, minimum of 100, higher values lose resolution but run faster
    
    ############### Output settings ############################--
    PrintTables=TRUE, ## print pdf tables of results
    PrintDefaultMaps=FALSE, ## print maps of default states; turned off to speed model run
    PrintScenarioMaps=TRUE, ## print maps fo scenario states; turned off to speed model run
    PrintRedistributionMaps=TRUE, ## maps of Gears that were moved or removed as a result of a closure
    WriteMapSources=FALSE, ## write output used for producing maps to .Rdata file. 
        ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
    WriteOutputCsv=TRUE,
    WriteDetailedOutput=FALSE,
    # WriteDetailedOutput=TRUE, ## write detailed data from individual stages to file for further analysis
    ArchiveInputSpreadsheet=FALSE ## v2.2.6 move input spreadsheet to output file?
  )
  Output
