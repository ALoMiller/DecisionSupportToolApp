rm(list=ls())

## set working directory
HD="//net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool"; setwd(HD)
HD=here::here()
## source decision support function
Version="DecisionSupportTool_V3.0.6.R"
source(paste0(HD, "/function_", Version))

## call function
Output=DecisionTool(
  HomeDir=HD, ## home directory for subdirectories
  ModelVersion=Version, ## model version
  
  ################## Select core inputs #####################################--
  # InputSpreadsheetName="BurtonsSubfolder/BS_BSB_Test.csv", ## csv input file with specified criteria
  InputSpreadsheetName="ScenarioTemplate_V3.0.1.csv", ## csv input file with specified criteria
  # InputSpreadsheetName="BS_OtherTrapPotTest.csv", ## csv input file with specified criteria
  # InputSpreadsheetName="BS_MonkfishTest.csv", ## csv input file with specified criteria
  # InputSpreadsheetName="BS_DogfishTest.csv", ## csv input file with specified criteria
  
  ## specify MapRef domain
  MapRefDomain="MapRef_3.0.2.Rdata", ## MapRef for Gillnet, OtherTrapPot, etc.
  # MapRefDomain="MapRef_HR_Lobster_V3.0.0.Rdata", ## MapRef for lobster fishery
  
  ## specify input data
  # GearMapName="GearMap_Lobster_V3.0.0.Rdata", ## default Gear map
  GearMapName="GearMap_Lobster_MassRMA_V3.0.0.Rdata", ## default Gear map with fishing in the cape cod closure
  # GearMapName="GearMap_Gillnet_IEC_V3.0.0.Rdata", ## default Gear map
  # GearMapName="GearMap_OtherTrapPot_IEC_V3.0.0.Rdata", ## default Gear map
  # GearMapName="GearMap_NewEnglMultispecies_V3.0.0.Rdata", ## proof-of-concept for multispecies groundfish
  # GearMapName="GearMap_Monkfish_V3.0.0.Rdata", ## proof-of-concept for multispecies groundfish
  # GearMapName="DST_BlackSeaBass_Federal_v3.0.0.Rdata", ## proof-of-concept for multispecies groundfish

  RopeStrengthModelName="LineStrengthModel_V2.1_60TrapThreshold.Rdata", ## rope strength as a function of trawl length; All trawls>35 traps being equal

  ThreatModel="ThreatMod_RW_Selectivity_Uncertainty.Rdata", ## gear threat model
  
  # WhaleInputModel="Duke_HumpbackWhaleModel_v10_DSTv3.Rdata", ##################### V3 ######################--
  # WhaleInputModel="Duke_HumpbackWhaleModel_v10_DSTv3_Expanded.Rdata", ##################### V3 ######################--
  # WhaleInputModel="Duke_FinWhaleModel_v11.Rdata", ## only compatible with DST v3+
  # WhaleInputModel="Duke_RightWhaleModel_v11_0309.Rdata", ## only compatible with GearMap v3+
  # WhaleInputModel="Duke_RightWhaleModel_v11_0318.Rdata", ## only compatible with GearMap v3+
  WhaleInputModel="Duke_RightWhaleModel_v11_1018.Rdata", ## only compatible with GearMap v3+
  
  ################# select model options ################--
  CommentText="BSB PrelimTest",
  TestScenario=FALSE, ## run a test scenario or only a default baseline?
  CoOccurrence=FALSE, ## run co-occurrence only; no gear threat model (speeds up model run time)
  HighResolution=TRUE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
  PrintMapsInHighResolution=TRUE,  ##New for 3.0.3# AggregateTrawls=FALSE, ## option to aggregat trawl length distributions within vessel classes to decrease model run time.
  RelocationCostExp=1, ## Exponent applied to distance for calculate relocation cost around closures
  ExpressRedistribution=TRUE, ## spatially aggregate Gears in closures to decrease runtime with some loss of resolution
  UpdateEndlineStrengths=FALSE, ## Option to not recalculate endline strengths when changing trawl lengths as a result of a trawling up action
  RopeStrengthResolution=500, ## binning for rope strength, minimum of 100, higher values lose resolution but run faster
  ThreatBounds=FALSE, ## calculate upper and lower threat bounds
  
  ############### Output settings ############################--
  PrintTables=TRUE, ## print pdf tables of results
  PrintDefaultMaps=TRUE, ## print maps of default states; turned off to speed model run
  PrintScenarioMaps=FALSE, ## print maps fo scenario states; turned off to speed model run
  PrintRedistributionMaps=FALSE, ## maps of Gears that were moved or removed as a result of a closure
  WriteMapSources=FALSE, ## write output used for producing maps to .Rdata file. ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
  WriteOutputCsv=TRUE,
  WriteDetailedOutput=FALSE,
  PrintSummary=TRUE,
  ArchiveInputSpreadsheet=FALSE ## v2.2.6 move input spreadsheet to output file?
)
Output
