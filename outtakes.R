
## creating classification tree table (adjacent)

c_tree_adjacent <- read_csv(paste("slurm_results", "classification_tree_results_020524.csv", sep = "/"))

c_tree_adjacent_long <- c_tree_adjacent %>%
  group_by(var, bag_id, class_level) %>%
  mutate(selected_levels_within_tree = n()) %>% # counting instances of var across levels for a given tree
  mutate(selected_at_least_once = ifelse(row_number() == 1, 1, 0)) %>%   # equals 1 if first instance of variable in a tree
  ungroup() %>%
  group_by(var, class_level) %>%
  summarize(median_cutoff = median(cutoff),
            median_tree_level = median(tree_levels),
            n_selected = sum(selected_at_least_once)) %>%         # n_selected can be greater than 10000 because sometimes variables are selected multiple times in a given tree
  arrange(class_level, desc(n_selected)) %>%
  filter(n_selected >= 5000)


c_tree_adjacent_wide  <- pivot_wider(c_tree_adjacent_long, names_from = "class_level", 
                                      values_from = c("median_cutoff", "median_tree_level", "n_selected"),
                                      id_cols = c("var")) %>%
  select(c("var", 
           "median_cutoff_class_1_2", "median_tree_level_class_1_2", "n_selected_class_1_2",
           "median_cutoff_class_2_3", "median_tree_level_class_2_3", "n_selected_class_2_3",
           "median_cutoff_class_3_4", "median_tree_level_class_3_4", "n_selected_class_3_4")) %>%
  mutate_all(as.character)  %>%  # turning into strings for visual reasons
  mutate(across(everything(), ~ replace_na(.x, "-")))   # replacing NA with "-" 


c_tree_adjacent_latex <- c_tree_adjacent_wide %>%
  kable(format = "latex", 
        digits=2,
        booktabs = TRUE,
        col.names = c("Variable", rep(c("Cutoff", "Level", "Selected"), 3)),
        linesep = "",
        align = "lcccccccccccc") %>%
  add_header_above(c(" ", "Lower vs. Working" = 3, 
                     "Working vs. Middle" = 3,
                     "Middle vs. Upper" = 3))
  
## creating classification tree table (one-vs-all)

c_tree_one_v_all<- read_csv(paste("slurm_results", "classification_tree_results_010624.csv", sep = "/"))

c_tree_one_v_all_long <- c_tree_one_v_all %>%
  group_by(var, bag_id, class_level) %>%
  mutate(selected_levels_within_tree = n()) %>% # counting instances of var across levels for a given tree
  mutate(selected_at_least_once = ifelse(row_number() == 1, 1, 0)) %>%   # equals 1 if first instance of variable in a tree
  ungroup() %>%
  group_by(var, class_level) %>%
  summarize(median_cutoff = median(cutoff),
            median_tree_level = median(tree_levels),
            n_selected = sum(selected_at_least_once)) %>%         # n_selected can be greater than 10000 because sometimes variables are selected multiple times in a given tree
  arrange(class_level, desc(n_selected)) %>%
  filter(n_selected >= 5000)
  

c_tree_one_v_all_wide  <- pivot_wider(c_tree_one_v_all_long, names_from = "class_level", 
                                        values_from = c("median_cutoff", "median_tree_level", "n_selected"),
                                        id_cols = c("var")) %>%
  select(c("var", 
           "median_cutoff_class_1", "median_tree_level_class_1", "n_selected_class_1",
           "median_cutoff_class_2", "median_tree_level_class_2", "n_selected_class_2",
           "median_cutoff_class_3", "median_tree_level_class_3", "n_selected_class_3",
           "median_cutoff_class_4", "median_tree_level_class_4", "n_selected_class_4")) %>%
  mutate_all(as.character)  %>%  # turning into strings for visual reasons
  mutate(across(everything(), ~ replace_na(.x, "-")))   # replacing NA with "-" %>%
  

c_tree_one_v_all_latex <- c_tree_one_v_all_wide %>%
  kable(format = "latex", 
        digits=2,
        booktabs = TRUE,
        col.names = c("Variable", rep(c("Cutoff", "Level", "Selected"), 4)),
        linesep = "",
        align = "lcccccccccccc") %>%
  add_header_above(c(" ", "Lower" = 3, 
                     "Working" = 3,
                     "Middle" = 3,
                     "Upper" = 3))


# Fix for all variables regression
all_vars_formula <- as.formula(paste(
  outcome_var, 
  "~", 
  paste("factor(", explanatory_variables, ")", collapse = " + ")
))

full_results <- data %>%
  group_by(year) %>%
  summarize(
    r_squared = summary(lm(all_vars_formula, data = cur_group()))$r.squared,
    explanatory_variable = "all_variables",
    model_type = "full",
    .groups = 'drop'
  )

# Combine results
bind_rows(individual_results, full_results) %>%
  mutate(
    r_squared = round(r_squared, 3),
    outcome_variable = outcome_var
  )