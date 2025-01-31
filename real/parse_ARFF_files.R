library(tidyverse)
library(glue)

directories <- list.dirs(path = "./", full.names = TRUE, recursive = TRUE)

# Remove non-folders
directories <- directories[directories != "./"]

# Helper function (copied from Trent, thank you Trent)

tidy_arff_files <- function(x){
  
  #-----------------------------
  # Grab the train and test data
  #-----------------------------
  
  # Shorten filepath to problem name
  
  probName <- gsub(".*\\/", "\\1", x)
  
  # Retrieve TRAIN and TEST files
  
  train <- foreign::read.arff(paste0(x, "/", probName, "_TRAIN.arff")) %>%
    mutate(id = row_number()) %>%
    mutate(set_split = "Train")
  
  themax <- max(train$id) # To add in test set to avoid duplicate IDs
  
  test <- foreign::read.arff(paste0(x, "/", probName, "_TEST.arff")) %>%
    mutate(id = row_number() + themax) %>% # Adjust relative to train set to stop double-ups
    mutate(set_split = "Test")
  
  #----------------------------
  # Wrangle data to long format
  #----------------------------
  
  # Train
  
  thecolstr <- colnames(train)
  keepcolstr <- thecolstr[!thecolstr %in% c("target", "id", "set_split")]
  
  train2 <- train %>%
    mutate(problem = probName) %>%
    pivot_longer(cols = all_of(keepcolstr), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  # Test
  
  thecolste <- colnames(test)
  keepcolste <- thecolste[!thecolste %in% c("target", "id", "set_split")]
  
  test2 <- test %>%
    mutate(problem = probName) %>%
    pivot_longer(cols = all_of(keepcolste), names_to = "timepoint", values_to = "values") %>%
    mutate(timepoint = as.numeric(gsub(".*?([0-9]+).*", "\\1", timepoint)))
  
  #------------
  # Merge files
  #------------
  
  tmp <- bind_rows(train2, test2) %>%
    mutate(id = paste0(id, "_", problem))
  
  return(tmp)
}

# Get time-series data

TimeSeriesData <- directories %>%
  purrr::map_df(~ tidy_arff_files(x = .x))

problem_split <- TimeSeriesData %>% group_by(problem) %>% group_split()
problem_names <- unique(TimeSeriesData$problem)

for (i in 1:length(problem_split)) {
  problem_data <- problem_split[[i]]
  write.csv(problem_data, glue("{problem_names[i]}_TS.csv"))
}