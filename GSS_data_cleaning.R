library(tidyverse)
library(haven)

# Setting the directory
path_to_read_raw_data <- "/scratch/network/vs3041/subjective-social-class/raw_data/current_GSS_extract.dta"
path_to_save_cleaned_data <- "/scratch/network/vs3041/subjective-social-class/cleaned_data/GSS_cross_section_clean.csv"

GSS_data_raw <- read_dta(path_to_read_raw_data)


# Numbers from the Morgan (2017) crosswalk (https://osf.io/xb2yz)
map_to_goldthorpe_numeric <- function(occupation_variable) {
case_when(
  as.character(occupation_variable) %in% c("10", "30", "110", "230", "300", "350", "360", "710", "800", "820", "840", "1005", "1020", "1200", "1210", "1220", "1230", "1240", "1300", "1320", "1330", "1340", "1350", "1360", "1400", "1410", "1420", "1430", "1440", "1450", "1460", "1500", "1510", "1520", "1530", "1600", "1610", "1640", "1650", "1660", "1700", "1710", "1720", "1740", "1760", "1800", "1820", "1840", "2100", "2105", "2110", "2200", "3000", "3010", "3040", "3050", "3060", "3120", "3250") ~ 1,
  as.character(occupation_variable) %in% c("20", "40", "50", "60", "120", "135", "136", "137", "140", "150", "325", "400", "420", "425", "430", "500", "530", "565", "640", "650", "700", "740", "850", "860", "900", "950", "1006", "1007", "1010", "1060", "1105", "1106", "1107", "1310", "1815", "1830", "1860", "2000", "2010", "2025", "2040", "2050", "2310", "2320", "2330", "2400", "2430", "2550", "2710", "2810", "2825", "2830", "2840", "2850", "3030", "3110", "3140", "3150", "3160", "3210", "3230", "3235", "3245", "3255", "3256", "3257", "3258", "3260", "3540", "4820", "4930", "9030") ~ 2,
  as.character(occupation_variable) %in% c("100", "340", "540", "630", "725", "810", "830", "910", "930", "940", "1030", "1050", "2016", "2145", "2160", "2340", "2440", "2540", "3200", "3220", "3500", "3510", "4540", "4800", "4810", "4830", "4840", "4850", "4965", "5000", "5010", "5020", "5030", "5100", "5110", "5120", "5140", "5150", "5160", "5165", "5200", "5210", "5220", "5230", "5240", "5250", "5260", "5300", "5310", "5320", "5330", "5340", "5350", "5360", "5410", "5420", "5500", "5520", "5530", "5540", "5600", "5630", "5700", "5800", "5810", "5820", "5830", "5840", "5860", "5900", "5910", "5920", "5940", "9040", "9050") ~ 3,
  as.character(occupation_variable) %in% c("2060", "2300", "3600", "3610", "3620", "3630", "3640", "3645", "3646", "3647", "3648", "3649", "3655", "3900", "3930", "3945", "3955", "4010", "4040", "4050", "4060", "4110", "4120", "4130", "4150", "4160", "4200", "4240", "4300", "4320", "4350", "4400", "4410", "4420", "4430", "4460", "4465", "4500", "4510", "4520", "4530", "4600", "4610", "4620", "4640", "4650", "4700", "4720", "4740", "4750", "4760", "4900", "4940", "4950", "5130", "5400", "5510", "7510", "9110", "9120", "9340", "9350", "9360", "9415", "9420") ~ 4,
  as.character(occupation_variable) == "205" ~ 7,
  as.character(occupation_variable) %in% c("160", "220", "330", "410", "600", "1540", "1550", "1560", "1900", "1910", "1920", "1930", "1940", "1950", "1965", "2015", "2600", "2630", "2700", "2720", "2740", "2750", "2760", "2800", "2860", "2900", "2910", "2920", "2960", "4710", "6200", "6660", "6700", "7000", "7020", "7030", "7040", "7050", "7100", "7110", "7120", "7420", "7700", "7900", "9000", "9240", "9310", "9410") ~ 8,
  as.character(occupation_variable) %in% c("6210", "6220", "6230", "6240", "6310", "6320", "6330", "6355", "6400", "6420", "6430", "6440", "6500", "6720", "6830", "7130", "7140", "7200", "7210", "7220", "7240", "7300", "7315", "7320", "7330", "7340", "7350", "7360", "7410", "7430", "7440", "7520", "7540", "7550", "7600", "7630", "7740", "8030", "8060", "8100", "8130", "8250", "8255", "8350", "8500", "8520", "8550", "8600", "8610", "8620", "8630", "8750", "8760", "8910", "8920", "9200", "9230", "9260", "9300", "9330", "9510") ~ 9,
  as.character(occupation_variable) %in% c("3940", "4020", "4030", "4140", "4220", "4230", "4250", "5550", "5560", "5610", "5620", "5850", "6130", "6250", "6260", "6300", "6360", "6460", "6515", "6540", "6600", "6710", "6730", "6740", "6750", "6765", "6800", "6820", "6840", "6910", "6920", "6930", "6940", "7150", "7160", "7260", "7560", "7610", "7710", "7720", "7730", "7750", "7800", "7810", "7830", "7840", "7850", "7855", "7920", "7930", "7940", "7950", "7960", "8000", "8010", "8020", "8040", "8120", "8140", "8150", "8160", "8200", "8210", "8220", "8256", "8300", "8310", "8320", "8330", "8340", "8360", "8400", "8410", "8420", "8430", "8440", "8450", "8460", "8510", "8530", "8540", "8640", "8650", "8710", "8720", "8730", "8740", "8800", "8810", "8830", "8840", "8850", "8860", "8900", "8930", "8940", "8950", "8965", "9130", "9140", "9150", "9500", "9520", "9560", "9600", "9610", "9620", "9630", "9640", "9650", "9720", "9730", "9740", "9750") ~ 10,
  as.character(occupation_variable) %in% c("6005", "6010", "6020", "6040", "6050", "6100", "6110") ~ 11,
  as.character(occupation_variable) %in% c("9800", "9810", "9820", "9830") ~ 12,
  TRUE ~ NA_real_
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
  mutate(across(contains("occ10"), ~ map_to_goldthorpe_numeric(.))) %>%
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

