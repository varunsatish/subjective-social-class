library(tidyverse)
library(haven)

# Setting the directory
path_to_read_raw_data <- "/scratch/network/vs3041/subjective-social-class/raw_data/current_GSS_extract.dta"
path_to_save_cleaned_data <- "/scratch/network/vs3041/subjective-social-class/cleaned_data/GSS_cross_section_clean.csv"

GSS_data_raw <- read_dta(path_to_read_raw_data)

map_to_goldthorpe_numeric <- function(occupation_variable) {
  case_when(
    # Service Class (1-2999)
    occupation_variable %in% 1:2999 ~ 1,
    # Intermediate Class (3000-4999)
    occupation_variable %in% 3000:4999 ~ 2,
    # Routine Non-Manual Class (5000-6999)
    occupation_variable %in% 5000:6999 ~ 3,
    # Routine Manual Class (7000-8999)
    occupation_variable %in% 7000:8999 ~ 4,
    # Self-Employed Class (9000-9999)
    occupation_variable %in% 9000:9999 ~ 5,
    # Agricultural Class (10000)
    occupation_variable == 10000 ~ 6,
    # Never Worked or Long-term Unemployed (10001)
    occupation_variable == 10001 ~ 7
  )
}

map_to_education_categories <- function(education_variable) {
  case_when(
    education_variable %in% 1:12 ~ 1,
    education_variable %in% 13: 15 ~ 2,
    education_variable >= 16 ~ 3
  )
}

to_decile <- function(numeric_variable) {
  case_when(
    numeric_variable >= quantile(numeric_variable, 0.9, na.rm = TRUE) ~ 10,
    numeric_variable >= quantile(numeric_variable, 0.8, na.rm = TRUE) ~ 9,
    numeric_variable >= quantile(numeric_variable, 0.7, na.rm = TRUE) ~ 8,
    numeric_variable >= quantile(numeric_variable, 0.6, na.rm = TRUE) ~ 7,
    numeric_variable >= quantile(numeric_variable, 0.5, na.rm = TRUE) ~ 6,
    numeric_variable >= quantile(numeric_variable, 0.4, na.rm = TRUE) ~ 5,
    numeric_variable >= quantile(numeric_variable, 0.3, na.rm = TRUE) ~ 4,
    numeric_variable >= quantile(numeric_variable, 0.2, na.rm = TRUE) ~ 3,
    numeric_variable >= quantile(numeric_variable, 0.1, na.rm = TRUE) ~ 2,
    numeric_variable >= quantle(numeric_variable, 0, na.rm = TRUE) ~ 1
  )
}

to_quintile <- function(numeric_variable) {
  case_when(
    numeric_variable >= quantile(numeric_variable, 0.8, na.rm = TRUE) ~ 5,
    numeric_variable >= quantile(numeric_variable, 0.6, na.rm = TRUE) ~ 4,
    numeric_variable >= quantile(numeric_variable, 0.4, na.rm = TRUE) ~ 3,
    numeric_variable >= quantile(numeric_variable, 0.2, na.rm = TRUE) ~ 2,
    numeric_variable >= quantile(numeric_variable, 0, na.rm = TRUE) ~ 1
  )
}

to_quartile <- function(numeric_variable) {
  case_when(
    numeric_variable >= quantile(numeric_variable, 0.75, na.rm = TRUE) ~ 4,
    numeric_variable >= quantile(numeric_variable, 0.5, na.rm = TRUE) ~ 3,
    numeric_variable >= quantile(numeric_variable, 0.25, na.rm = TRUE) ~ 2,
    numeric_variable >= quantile(numeric_variable, 0, na.rm = TRUE) ~ 1
  )
}


GSS_data <- GSS_data_raw %>% 
  
  # inclusion criteria 
  filter(age >= 25 & age <= 65) %>%       # working age population (see Hout, 2008)
  filter(year >= 1972 & year <= 2018) %>% # 1972-2018
  filter(!is.na(class)) %>%               # observed social class identification
  mutate_all(as.numeric) %>%              # all variables to numeric
  mutate(across(contains("occ"), ~ map_to_goldthorpe_numeric(.))) %>%
  rename_with(~ paste0(., "_c"), contains("occ")) %>% 
  mutate(across(contains("realinc"), ~ to_quartile(.))) %>%
  mutate(across(contains("coninc"), ~ to_quartile(.))) %>%
  mutate(across(contains("prestg10"), ~ to_quartile(.))) %>%
  rename_with(~ paste0(., "_c"), contains("realinc")) %>%
  rename_with(~ paste0(., "_c"), contains("coninc")) %>%
  rename_with(~ paste0(., "_c"), contains("prestg10")) %>%
  mutate(across(contains("educ"), ~ map_to_education_categories(.))) %>%
  rename_with(~ paste0(., "_c"), contains("educ"))
  
  

write_csv(GSS_data, file=path_to_save_cleaned_data)

