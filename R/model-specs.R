Action <- c("Closure",
            "Constraint_Fishery",
            "Constraint_Spatial",
            "GearReduction",
            "GearCap",
            "MaxRopeStrength",
            "MaxGearSnglLn",
            "NoAction",
            "RopelessDevice",
            "StringLength")
LMA <- c("A1",
          "A2",
          "A2_3overlap",
          "A3",
          "OCC")
State <- c("ME",
            "NH",
            "MA")
Fishery <- c("NonExempt",
             "Exempt",
             "Midshelf_Lobster",
             "Midshelf_Crab",
             "Offshore_Lobster",
             "Offshore_Crab",
             "EverythingButExempt",
             "Gillnet",
             "BlueCrab",
             "OtherTrapPot",
             "Dogfish",
             "GB_SNE_CodPollock",
             "GOM_CodPollock",
             "GOM_Haddock",
             "Monkfish",
             "BlackSeaBass_Fed")
RopelessDevice <- c("TimedRelease",
                    "AcousticRelease")
# select multiple
StatArea <- c(464, 465, 467, 511, 512, 513, 514,
              515, 521, 522, 525, 526, 533, 534,
              537, 538, 539, 541, 542, 552, 561,
              562, 611, 612, 613, 614, 615, 616,
              621, 622, 623, 625, 626, 627, 631,
              632, 635, 636, 700, 701, 702, 706,
              707, 708, 709, 712, 713, 714, 715,
              717, 718, 719, 722, 723, 724, 727,
              728, 729, 732, 733, 736, 737, 740,
              741)
StringRegulation <- c("Min",
                     "Max",
                     "Exactly")

Months <- 1:12
BuoylineDevice <- ""
MaxGearSnglLn <- ""
MaxRopeStrength <- ""
Percentage <- ""
Shapefile <- ""
GearCap <- ""
StringLen = ""

#Data frame to hold 
DF <- data.frame(Action = as.character(rep(NA,10)),
                 LMA = as.character(rep(NA, 10)),
                 State = as.character(rep(NA, 10)),
                 StatArea = as.character(rep(NA, 10)),
                 Fishery = as.character(rep(NA, 10)),
                 Shapefile = as.character(rep(NA, 10)),
                 Months = as.character(rep(NA, 10)),
                 Percentage = as.character(rep(NA, 10)),
                 StringRegulation = as.character(rep(NA, 10)),
                 StringLen = as.character(rep(NA, 10)),
                 MaxRopeStrength = as.character(rep(NA, 10)),
                 BuoylineDevice = as.character(rep(NA, 10)),
                 RopelessDevice = as.character(rep(NA, 10)),
                 GearCap = as.character(rep(NA, 10)),
                 MaxGearSnglLn = as.character(rep(NA, 10)))

# #get existing scenarios for listing as scenaerio inputs
existing_input_csvs <- list.files(here::here("InputSpreadsheets"))
existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")

#Get list of shapefiles
shapefile_names <- unique(stringr::str_remove(list.files(here::here("InputShapefiles")),"\\..*$"))

