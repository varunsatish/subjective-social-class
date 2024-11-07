### Calculating r-squared from bivariate regression of outcomes on each 
### individual variable
library(tidyverse)
library(multidplyr)

# which directory?
HOME <- 1

directory_path <- "/Users/vs3041/Princeton Dropbox/Varun Satish/Social Class"
data_path <- "cleaned_data"

GSS_data <- read_csv(paste(directory_path, data_path, "GSS_cross_section_clean.csv", sep = "/"), show_col_types = F)

# generating class indicators 
GSS_data <- GSS_data %>%
  mutate(
    class_1 = ifelse(class == 1, 1, 0),
    class_2 = ifelse(class == 2, 1, 0),
    class_3 = ifelse(class == 3, 1, 0),
    class_4 = ifelse(class == 4, 1, 0),
    
    class_1_2 = case_when(
      class == 1 ~ 0,
      class == 2 ~ 1
    ),
    class_2_3 = case_when(
      class == 2 ~ 0,
      class == 3 ~ 1
    ),
    class_3_4 = case_when(
      class == 3 ~ 0,
      class == 4 ~ 1
    ))

## dropping variables not asked in at least 16 years 

# first, getting data at the variable level
proportion_variables_missing_by_year <- GSS_data %>%
  group_by(year) %>%
  summarise_all(~mean(is.na(.)))

proportion_variables_missing_by_year_long <- proportion_variables_missing_by_year %>%
  pivot_longer(cols = -c(year), names_to = "variable", values_to = "proportion_missing") 

number_years_missing_by_variable <- proportion_variables_missing_by_year_long %>%
  group_by(year) %>%
  mutate(all_obs_missing_in_year = ifelse(proportion_missing == 1, 1, 0),
         most_obs_missing_in_year = ifelse(proportion_missing >= 0.8, 1, 0)) %>%
  group_by(variable) %>%
  summarise(years_all_missing = sum(all_obs_missing_in_year),
            years_mostly_missing = sum(most_obs_missing_in_year))

# creating vector with relevant variables
variables_to_pass <- number_years_missing_by_variable %>%
  filter(!grepl("class", variable)) %>%    # excluding "class" indicators
  filter(!grepl("_c", variable)) %>%    # excluding categorical dummies used for descriptives
  filter(years_mostly_missing  <= 16) %>%         # arbitrarily chosen
  select(variable) %>%
  pull()


## Table with 1 vs. all splits 

# creating a dataset for each comparison

class_1_df <- GSS_data %>%
  select(class_1, variables_to_pass) 

class_2_df <- GSS_data %>%
  select(class_2, variables_to_pass) 

class_3_df <- GSS_data %>%
  select(class_3, variables_to_pass) 

class_4_df <- GSS_data %>%
  select(class_4, variables_to_pass) 

# running regressions on each dataset

class_1_regressions <- class_1_df %>%
  map(~lm(class_1 ~ .x, data=class_1_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_1") %>%
  mutate(comparison = "class_1")

class_2_regressions <- class_2_df %>%
  map(~lm(class_2 ~ .x, data=class_2_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_2") %>%
  mutate(comparison = "class_2")

class_3_regressions <- class_3_df %>%
  map(~lm(class_3 ~ .x, data=class_3_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_3") %>%
  mutate(comparison = "class_3")

class_4_regressions <- class_4_df %>%
  map(~lm(class_4 ~ .x, data=class_4_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_4") %>%
  mutate(comparison = "class_4")


regression_results_1_v_all <- rbind(class_1_regressions,
                            class_2_regressions,
                            class_3_regressions,
                            class_4_regressions) %>%
  group_by(comparison) %>%
  mutate(r2_rank = rank(-r2)) %>% # descending orer
  arrange(comparison, r2_rank) %>%
  ungroup() %>%
  filter(r2_rank <= 20)

results_wide_1_v_all <- pivot_wider(regression_results_1_v_all, names_from = "comparison", 
                            values_from = c("variable", "r2"),
                            id_cols = "r2_rank") %>%
  select(c("r2_rank", 
           "variable_class_1", "r2_class_1",
           "variable_class_2", "r2_class_2",
           "variable_class_3", "r2_class_3",
           "variable_class_4", "r2_class_4"))    # reordering columns

latex_table_1_v_all <- results_wide_1_v_all %>%
  kable(format = "latex", 
        digits=2,
        booktabs = TRUE,
        col.names = c("Rank", rep(c("Variable", "R2"), 4)),
        linesep = "",
        align = "clrlrlrlr") %>%
  add_header_above(c(" ", "Lower Class" = 2, 
                     "Working Class" = 2,
                     "Middle Class" = 2,
                     "Upper Class" = 2))



## Table with adjacent splits 

# creating a dataset for each comparison

class_1_2_df <- GSS_data %>%
  select(class_1_2, variables_to_pass) 

class_2_3_df <- GSS_data %>%
  select(class_2_3, variables_to_pass) 

class_3_4_df <- GSS_data %>%
  select(class_3_4, variables_to_pass) 

# running regressions on each dataset

class_1_2_regressions <- class_1_2_df %>%
  map(~lm(class_1_2 ~ .x, data=class_1_2_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_1_2") %>%
  mutate(comparison = "class_1_2")

class_2_3_regressions <- class_2_3_df %>%
  map(~lm(class_2_3 ~ .x, data=class_2_3_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_2_3") %>%
  mutate(comparison = "class_2_3")

class_3_4_regressions <- class_3_4_df %>%
  map(~lm(class_3_4 ~ .x, data=class_3_4_df)) %>%
  map_dbl(~summary(.)$r.squared) %>%
  enframe(., name = "variable", value = "r2") %>%
  filter(variable != "class_3_4") %>%
  mutate(comparison = "class_3_4")

regression_results_splits <- rbind(class_1_2_regressions,
                            class_2_3_regressions,
                            class_3_4_regressions) %>%
  group_by(comparison) %>%
  mutate(r2_rank = rank(-r2)) %>% # descending orer
  arrange(comparison, r2_rank) %>%
  ungroup() %>%
  filter(r2_rank <= 20)

results_wide_splits <- pivot_wider(regression_results_splits, names_from = "comparison", 
                            values_from = c("variable", "r2"),
                            id_cols = "r2_rank") %>%
  select(c("r2_rank", 
           "variable_class_1_2", "r2_class_1_2",
           "variable_class_2_3", "r2_class_2_3",
           "variable_class_3_4", "r2_class_3_4"))    # reordering columns

latex_table_splits <- results_wide %>%
  kable(format = "latex", 
        digits=2,
        booktabs = TRUE,
        col.names = c("Rank", rep(c("Variable", "R2"), 3)),
        linesep = "",
        align = "clrlrlr") %>%
  add_header_above(c(" ", "Lower vs. Working" = 2, 
                     "Working vs. Middle" = 2,
                     "Middle vs. Upper" = 2))
  


