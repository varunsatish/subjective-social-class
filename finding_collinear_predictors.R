# identifiying collinear variables explanatory variables

library(tidyverse)
library(tidyr)
library(foreach)
library(doParallel)

# set up data
directory_path <- "/scratch/network/vs3041/social_class_lasso"
#directory_path <- "/Users/vs3041/Dropbox (Princeton)/Social Class/"

data_path <- "data"
#data_path <- "cleaned_data"

results_path <- "results"

vars_to_test_for_collinearity <- read_csv(paste(directory_path, data_path, "ML_training_temp.csv", sep = "/"), show_col_types = F) 

vars_to_test_for_collinearity <- vars_to_test_for_collinearity %>% 
  mutate_all(~replace(., is.na(.), -99))

column_names <- colnames(vars_to_test_for_collinearity)

number_of_variables <- length(column_names)

cl <- makeCluster(4)  # Create a cluster with 2 cores
registerDoParallel(cl)  # Register the parallel backend

# Function to process each element of the list
calculate_correlation <- function(i, j) {
  
  variable_name_i <- column_names[i]
  variable_name_j <- column_names[j]

  # pulling variables from dataset
  variable_i <- vars_to_test_for_collinearity %>%
    select(variable_name_i) %>%
    pull()
  
  variable_j <- vars_to_test_for_collinearity %>%
    select(variable_name_j) %>%
    pull()
  
  # calculating correlation
  estimated_correlation <- cor(variable_i, variable_j, use = "complete.obs")
  
  # returning a tibble that tracks pair and correlation
  tibble(index_i = i,
         index_j = j,
         pair_i = variable_name_i, 
         pair_j = variable_name_j, 
         correlation = estimated_correlation)
}

# Outer foreach loop
correlation_by_combination <- foreach(i = 1:number_of_variables,
                    .combine="bind_rows", 
                    .packages=c("foreach", "tidyverse")) %dopar% {
  # Inner foreach loop
  inner_loop <- foreach(j = i:length(column_names)) %dopar% {
    calculate_correlation(i,j)
  }
  
  inner_loop
}

current_date <- format(Sys.Date(), "%d%m%y")
write_csv(correlation_by_combination, file=paste(directory_path, results_path, paste0("correlation_by_combination", "_", current_date, ".csv"), sep = "/"))


