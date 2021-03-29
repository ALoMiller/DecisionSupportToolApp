Version="DecisionSupportTool_V3.0.7.R"
HD=here::here()
run_decisiontool <- function(HD=here::here(),InputSpreadsheetName="ScenarioTemplate_V3.0.1.csv",
                             GearMapName="GearMap_Lobster_V3.0.0.Rdata",
                             MapRefDomain="MapRef_3.0.2.Rdata",
                             WhaleMapName="Duke_RightWhaleModel_v10_1018.Rdata",
                             CommentText="",TestScenario=TRUE,CoOccurrence=FALSE,
                             HighResolution=FALSE){
  DecisionTool(
    HomeDir=HD, ## home directory for subdirectories
    ModelVersion=Version, ## model version
    InputSpreadsheetName=InputSpreadsheetName, ## csv input file with specified criteria
    #MapRefDomain="MapRef_3.0.2.Rdata", ## MapRef for Gillnet, OtherTrapPot, etc.
    MapRefDomain=MapRefDomain, ## MapRef for Gillnet, OtherTrapPot, etc.
    GearMapName=GearMapName, ## default Gear map with fishing in the cape cod closure
    RopeStrengthModelName="LineStrengthModel_V2.1_60TrapThreshold.Rdata", ## rope strength as a function of trawl length; All trawls>35 traps being equal
    ThreatModel="ThreatMod_RW_Selectivity_Uncertainty.Rdata", ## gear threat model
    WhaleInputModel=WhaleMapName, ## only compatible with GearMap v3+
    
    ################# select model options ################--
    CommentText="SHINY V3.0.6 TEST",
    TestScenario=TRUE, ## run a test scenario or only a default baseline?
    CoOccurrence=FALSE, ## run co-occurrence only; no gear threat model (speeds up model run time)
    HighResolution=FALSE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
    PrintMapsInHighResolution=FALSE,  ##New for 3.0.3# AggregateTrawls=FALSE, ## option to aggregat trawl length distributions within vessel classes to decrease model run time.
    RelocationCostExp=1, ## Exponent applied to distance for calculate relocation cost around closures
    ExpressRedistribution=TRUE, ## spatially aggregate Gears in closures to decrease runtime with some loss of resolution
    UpdateEndlineStrengths=FALSE, ## Option to not recalculate endline strengths when changing trawl lengths as a result of a trawling up action
    RopeStrengthResolution=500, ## binning for rope strength, minimum of 100, higher values lose resolution but run faster
    ThreatBounds=FALSE, ## calculate upper and lower threat bounds
    
    ############### Output settings ############################--
    PrintTables=TRUE, ## print pdf tables of results
    PrintDefaultMaps=TRUE, ## print maps of default states; turned off to speed model run
    PrintScenarioMaps=TRUE, ## print maps fo scenario states; turned off to speed model run
    PrintRedistributionMaps=TRUE, ## maps of Gears that were moved or removed as a result of a closure
    WriteMapSources=FALSE, ## write output used for producing maps to .Rdata file. ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
    WriteOutputCsv=TRUE,
    WriteDetailedOutput=FALSE,
    PrintSummary=TRUE,
    ArchiveInputSpreadsheet=FALSE ## v2.2.6 move input spreadsheet to output file?
  )  
}
