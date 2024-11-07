# generating classification tree results 
library(tree)
library(tidyverse)

# which directory?
HOME <- 0
SLURM <- 1

# which sets of results?
FULL_SAMPLE <- 1
SUBGROUPS <- 0

if (HOME == 1) {
  directory_path <- "/Users/vs3041/Dropbox (Princeton)/Social Class"
  data_path <- "cleaned_data"
  class_levels <- "class_2"
  number_of_bags <- 10

}

if (SLURM == 1) {
  directory_path <- "home/vs3041/social_class"
  data_path <- "data"
  class_levels <- c("class_1", "class_2", "class_3", "class_4")
  number_of_bags <- 10000
  
}

setwd(directory_path)



create_grouping_vector <- function(x) {
  lengths <- rle(x)$lengths
  values <- rle(x)$values
  
  number_of_values <- length(values)
  
  grouping_vector <- c()
  block_vector <- c()
  
  for (i in 1:number_of_values) {
    
    ones_or_zeroes <- values[i]              # value that is being repeated
    number_of_ones_or_zeros <- lengths[i]           # number of times repeated
    
    if (ones_or_zeroes == 0) {
      
      # within each block of 0s, assign the first one 2, the next one 3, and so on... 
      
      grouping_vector <- c(grouping_vector, seq(1, number_of_ones_or_zeros))   # if not a leaf, put into group
      
    } else {
      grouping_vector <- c(grouping_vector, rep(NA, number_of_ones_or_zeros))        # if leaf, put NA
    }
    
  }
  
  # The first block has some complications. It needs to have terminal node equal to 0,
  # which means we have to minus 1 from all rows in the first block
  number_in_first_block <- lengths[1]
  
  grouping_vector[1: number_in_first_block] <- grouping_vector[1: number_in_first_block] - 1
  
  # normalizing so that terminal node is 1 and everything else follows
  grouping_vector <- grouping_vector + 1
  
  return(grouping_vector)
  
}



obtaining_tree_structure <- function(fitted_tree) {
  
  # The issue with going from a tree to a table is that they are fundamentally different
  # data structures, this code converts the tree to a table with some relevant information
  
  tree_structure <- fitted_tree$frame     # information from tree diagream 
  
  tree_structure <- cbind(id = rownames(tree_structure), tree_structure)   # ordering 
  
  tree_structure <- tree_structure %>%
    mutate(is_leaf = ifelse(var == "<leaf>", 1, 0))
  
  tree_structure$cutoff <- as.numeric(gsub("<", "", tree_structure$splits[, 1]))      # getting rid of <, turning to numeric
  
  tree_structure$tree_levels <- create_grouping_vector(tree_structure$is_leaf)        # creating groups
  
  tree_structure <- tree_structure %>%
    filter(is_leaf == 0) %>%
    select(var, tree_levels, cutoff) 
  
  return(tree_structure)
  
}








# running the decision tree algorithm 

data_for_ML_training <- read_csv(paste(directory_path, data_path, "ML_training.csv", sep = "/"), show_col_types = F)
data_for_ML_test <- read_csv(paste(directory_path, data_path, "ML_test.csv", sep = "/"), show_col_types = F)

if (FULL_SAMPLE == 1) {
  
  set.seed(08542)
  
  tree_data <- tibble()
  
  for (class_level in class_levels) {
    
    # obtaining outcomes which need to be merged again 
    y_training <- data_for_ML_training %>%
      select(!!sym(class_level)) %>%
      pull()
    
    y_test <- data_for_ML_test %>%
      select(!!sym(class_level)) %>%
      pull()
    
    # getting rid of class variables 
    x_training <- data_for_ML_training %>%
      select(., !starts_with("class")) %>%
      select(., !ends_with("NA"))
    
    x_test <- data_for_ML_test %>%
      select(., !starts_with("class")) %>%
      select(., !ends_with("NA"))
    
    
    # merging outcomes and predictors 
    training_data <- cbind(y_training, x_training) 
    
    test_data <- cbind(y_test, x_test)
    
    # bagging procedure
    
    number_in_bag_sample <- nrow(training_data)
    
    for (bag_id in 1:number_of_bags) {
      
  
      training_data_bag <- sample_n(training_data, 
                                    size = number_in_bag_sample, 
                                    replace = TRUE)
      
      bag_fit <- tree(as.factor(y_training) ~ .,
                      data=training_data_bag)
      
      # obtaining variable selected, cutoff and level for a given bagged sample
      tree_structure_for_bag <- obtaining_tree_structure(bag_fit)
      
      tree_structure_for_bag <- cbind(tree_structure_for_bag, bag_id, class_level)
      
      tree_data <- rbind(tree_data, tree_structure_for_bag)
      
    }
  
  }
  
  current_date <- format(Sys.Date(), "%d%m%y")
  write_csv(tree_data, file=paste(directory_path, "results", 
                                paste0("classification_tree_results", "_", current_date, ".csv"), sep = "/"))

}


## To-do: update code for subgroups.

if (SUBGROUPS == 1) {
  
  data_for_ML_training <- data_for_ML_training %>%
    mutate(years_past_2000 = ifelse(year >= 2000, 1, 0))
  
  data_for_ML_test <- data_for_ML_test %>%
    mutate(years_past_2000 = ifelse(year >= 2000, 1, 0))

  subgroups <- "years_past_2000"
  
  set.seed(08542)
  
  for (subgroup in subgroups) {
    
    subgroup_categories <- data_for_ML_training %>%
      select(!!sym(subgroup)) %>%
      pull() %>%
      unique()
    
    tree_data <- tibble()
  
    for (subgroup_category in subgroup_categories) {
      
      subgroup_label <- paste(subgroup, subgroup_category, sep = "_")
      
      subgroup_training <- data_for_ML_training %>%
        filter(!!sym(subgroup) == subgroup_category)
      
      results_across_all_classes <- tibble()
      
      for (class_level in class_levels) {
        
        # obtaining outcomes which need to be merged again 
        y_training <- data_for_ML_training %>%
          select(!!sym(class_level)) %>%
          pull()
        
        y_test <- data_for_ML_test %>%
          select(!!sym(class_level)) %>%
          pull()
        
        # getting rid of class variables 
        x_training <- data_for_ML_training %>%
          select(., !starts_with("class")) %>%
          select(., !ends_with("NA"))
        
        x_test <- data_for_ML_test %>%
          select(., !starts_with("class")) %>%
          select(., !ends_with("NA"))
        
        
        # merging outcomes and predictors 
        training_data <- cbind(y_training, x_training) 
        
        test_data <- cbind(y_test, x_test)
        
        # bagging procedure
        
        number_in_bag_sample <- nrow(training_data)
        
        for (bag_id in 1:number_of_bags) {
          
          
          training_data_bag <- sample_n(training_data, 
                                        size = number_in_bag_sample, 
                                        replace = TRUE)
          
          bag_fit <- tree(as.factor(y_training) ~ .,
                          data=training_data_bag)
          
          
          # obtaining variable selected, cutoff and level for a given bagged sample
          tree_structure_for_bag <- obtaining_tree_structure(bag_fit)
          
          tree_structure_for_bag <- cbind(tree_structure_for_bag, bag_id, class_level, subgroup, subgroup_category)
          
          tree_data <- rbind(tree_data, tree_structure_for_bag)
          

        }   # bagged sample 

      } # classes
      

    } # subgroup category
    

  }  # subgroup
      
    current_date <- format(Sys.Date(), "%d%m%y")
    write_csv(tree_data, file=paste(directory_path, "results", 
                                                     paste0("classification_tree_subgroups_results", "_", current_date, ".csv"), sep = "/"))
  
}  # running statement



  




