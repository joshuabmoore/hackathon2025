library(tidyverse)
library(theft)
library(glue)

problem_list <- c("Beef", "DodgerLoopWeekend", "ECG5000", "GunPointOldVersusYoung",
                  "InlineSkate", "InsectEPGRegularTrain", "ItalyPowerDemand", "PowerCons",
                  "Wafer")
data_path <- getwd()

extract_features_by_problem <- function(data_path, theproblem){
  
  # Load in data for this problem
  problem_data <- read.csv(glue("{data_path}/{theproblem}_TS.csv")) %>% select(-X)
  
  # Calculate catch22 features + mean/SD, for catch24
  catch24_res <- calculate_features(problem_data, id_var = "id", time_var = "timepoint", 
                             values_var = "values", group_var = "target", 
                             feature_set = "catch22", catch24 = TRUE, seed = 123)
  
  # Catch cases where appended NAs cause errors (i.e., different time series have different lengths)
  # We do this by mapping over IDs to a modified feature calculation function that drops NAs by ID
  
  if(length(unique(tmp$id)) != length(unique(outs$id))){
    outs <- unique(tmp$id) %>%
      purrr::map_dfr(~ calculate_features2(tmp, id_var = "id", time_var = "timepoint", 
                                           values_var = "values", group_var = "target", 
                                           catch24 = TRUE, seed = 123, the_id = .x))
  }
  
  save(outs, file = paste0("data/feature-calcs/", theproblem, ".Rda"))
}

# Run the function

unique(TimeSeriesData$problem)[!unique(TimeSeriesData$problem) %in% c("AllGestureWiimoteX", "AllGestureWiimoteY", "AllGestureWiimoteZ", "PLAID")] %>%
  purrr::map(~ extract_features_by_problem(data = TimeSeriesData, theproblem = .x))