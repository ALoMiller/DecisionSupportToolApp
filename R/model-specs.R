Action <- c("Closure",
            "Constraint_Fishery",
            "Constraint_Spatial",
            "GearReduction",
            "GearCap",
            "MaxRopeStrength",
            "MaxTrapsWSingleLine",
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
             "EverythingButExempt")
RopelessDevice <- c("TimedRelease",
                    "AcousticRelease")
# select multiple
StatArea <- c(464, 465, 511,
              512, 513, 514,
              515, 521, 522,
              525, 526, 561,
              562, 537, 538,
              539)
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
existing_input_csvs <- list.files("InputSpreadsheets")
existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")

#Get list of shapefiles
shapefile_names <- unique(stringr::str_remove(list.files("InputShapefiles"),"\\..*$"))

