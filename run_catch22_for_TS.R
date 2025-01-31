library(tidyverse)
library(theft)
library(glue)

problem_list <- c("Beef", "DodgerLoopWeekend", "ECG5000", "GunPointOldVersusYoung",
                  "InlineSkate", "InsectEPGRegularTrain", "ItalyPowerDemand", "PowerCons",
                  "Wafer")
real_data_path <- "real/"

# Make directories as needed
icesTAF::mkdir(glue("{real_data_path}/catch24_res"))

extract_features_by_problem <- function(data_path, theproblem){
  
  # Load in data for this problem
  problem_data <- read.csv(glue("{data_path}/preprocessed_TS/{theproblem}_TS.csv")) %>% select(-X)
  
  # Calculate catch22 features + mean/SD, for catch24
  catch24_res <- calculate_features(problem_data, id_var = "id", time_var = "timepoint", 
                             values_var = "values", group_var = "target", 
                             feature_set = "catch22", catch24 = TRUE, seed = 123)
  
  # Save catch24 results
  write.csv(catch24_res, glue("{data_path}/catch24_res/{theproblem}_catch24_res.csv"), row.names = F)
}

# Run the function
problem_list %>%
  purrr::map(~ extract_features_by_problem(data_path = data_path, theproblem = .x))