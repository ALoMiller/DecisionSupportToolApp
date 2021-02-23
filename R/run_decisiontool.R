# This is run from shiny 
#
# The following lines need to be placed in shiny
#
#l ibrary(rgdal)
# library(sp)
# library(maps)
# library(maptools)
# library(grid)
# library(gtable)
# library(gridExtra)
# library(maptools)
# HD <- here::here()
# source(here::here("function_DecisionSupportTool_V1.2.R"))
#
Version="DecisionSupportTool_V3.0.2.R"

run_decisiontool <- function(HD=here::here(),InputSpreadsheetName="ScenarioTemplate_V3.0.0.csv"){
  DecisionTool(
    HomeDir=HD, ## home directory for subdirectories
    ModelVersion=Version, ## model version
    InputSpreadsheetName=InputSpreadsheetName, ## csv input file with specified criteria
    MapRefDomain="MapRef_HR_Lobster_V3.0.0.Rdata",
    GearMapName="GearMap_Lobster_MassRMA_V3.0.0.Rdata", ## default Gear map with fishing in the cape cod closure
    RopeStrengthModelName="LineStrengthModel_V2.1_60TrapThreshold.Rdata", ## rope strength as a function of trawl length; All trawls>35 traps being equal
    ThreatModel="ThreatMod_RW_Selectivity_Uncertainty.Rdata", ## gear threat model
    WhaleInputModel="Duke_RightWhaleModel_v10_1018.Rdata", ## only compatible with GearMap v3+
    CommentText="PrefAlt_MassRMA_v10_DST3_TotalMARiskReduction",
    TestScenario=TRUE, ## run a test scenario or only a default baseline?
    CoOccurrence=TRUE, ## run co-occurrence only; no gear threat model (speeds up model run time)
    RelocationCostExp=1, ## Exponent applied to distance for calculate relocation cost around closures
    ExpressRedistribution=TRUE, ## spatially aggregate Gears in closures to decrease runtime with some loss of resolution
    UpdateEndlineStrengths=FALSE, ## Option to not recalculate endline strengths when changing trawl lengths as a result of a trawling up action
    RopeStrengthResolution=500, ## binning for rope strength, minimum of 100, higher values lose resolution but run faster
    
    ############### Output settings ############################--
    PrintTables=TRUE, ## print pdf tables of results
    PrintDefaultMaps=TRUE, ## print maps of default states; turned off to speed model run
    PrintScenarioMaps=TRUE, ## print maps fo scenario states; turned off to speed model run
    PrintRedistributionMaps=TRUE, ## maps of Gears that were moved or removed as a result of a closure
    WriteMapSources=TRUE, ## write output used for producing maps to .Rdata file. 
    ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
    WriteOutputCsv=TRUE,
    WriteDetailedOutput=FALSE,
    # WriteDetailedOutput=TRUE, ## write detailed data from individual stages to file for further analysis
    ArchiveInputSpreadsheet=FALSE ## v2.2.6 move input spreadsheet to output file?
    
    )
  
}