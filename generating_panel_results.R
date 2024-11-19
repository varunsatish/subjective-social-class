library(tidyverse)
library(ggplot2)


install.packages('gssr', repos =
                   c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))
library(gssr)


# Setting the directory
directory_path <- "/Users/vs3041/Dropbox (Princeton)/Social Class"
data_path <- "cleaned_data"
figure_path <- "figures"
setwd(directory_path)

# setting ggplot theme
scale_colour_brewer_d <- function(..., palette = "Set1") {
  scale_colour_brewer(..., palette = palette )
}

scale_fill_brewer_d <- function(..., palette = "Set1") {
  scale_fill_brewer(..., palette = palette)
}

options(
  ggplot2.discrete.colour = scale_colour_brewer_d,
  ggplot2.discrete.fill = scale_fill_brewer_d
)



# setting some parameters
axis_title_size = 12
axis_tick_size = 10


# consult: https://kjhealy.github.io/gssr/

data("gss_panel06_long")

data("gss_panel08_long")

data("gss_panel10_long")


# firstid x wave gives cases

# wave 1 to wave 2
data_from_2006 <- gss_panel06_long %>%
  select(c(firstid, class, wave))%>%
  #filter(wave == 1 | wave == 3) %>%
  pivot_wider(., names_from = wave, names_prefix = "wave_", values_from = class) %>%
  select(wave_1, wave_2, wave_3) %>%
  mutate(year = 2006)

data_from_2008 <- gss_panel08_long %>%
  select(c(firstid, class, wave))%>%
  #filter(wave == 1 | wave == 3) %>%
  pivot_wider(., names_from = wave, names_prefix = "wave_", values_from = class) %>%
  select(wave_1, wave_2, wave_3) %>%
  mutate(year = 2008)

data_from_2010 <- gss_panel10_long %>%
  select(c(firstid, class, wave))%>%
  #filter(wave == 1 | wave == 3) %>%
  pivot_wider(., names_from = wave, names_prefix = "wave_", values_from = class) %>%
  select(wave_1, wave_2, wave_3) %>%
  mutate(year = 2010)

combined_data <- rbind(data_from_2006, data_from_2008, data_from_2010)

combined_data_long <- combined_data %>%
  pivot_longer(cols = starts_with("wave_"),
                                    names_to = "wave",
                                    values_to = "class")



proportion_class_panel <- combined_data_long %>%
  filter(!is.na(class)) %>%
  group_by(class, wave) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(wave) %>%
  mutate(mean = count/sum(count))


# how many times was there a switch?

wave_1_to_wave_2 <- combined_data %>%
  select(wave_1, wave_2) %>%
  rename("state_1" = "wave_1",
         "state_2" = "wave_2")

wave_2_to_wave_3 <- combined_data %>%
  select(wave_2, wave_3) %>%
  rename("state_1" = "wave_2",
         "state_2" = "wave_3")

transition_by_person_level <- rbind(wave_1_to_wave_2, wave_2_to_wave_3)
  


barplot_data_coming <- transition_by_person_level %>%
  group_by(state_1, state_2) %>%
  summarize(cell_count = n()) %>%
  ungroup() %>%
  filter(!is.na(state_1) & !is.na(state_2)) %>%
  group_by(state_2) %>%
  mutate(sum_class_state_2 = sum(cell_count)) %>%
  ungroup() %>%
  mutate(proportion = cell_count/sum_class_state_2,
         se = sqrt(proportion * (1-proportion) / cell_count)) 

  
# where are people combing from?
ggplot(barplot_data_coming, aes(x = as.factor(state_2), 
                                y = proportion, 
                                fill = as.factor(state_1))) +
  geom_bar(stat="identity", position = "dodge", colour="black") + 
  geom_errorbar(aes(ymin=proportion-se, 
                    ymax=proportion+se), width=0.4, 
                colour="black", alpha=0.9, size=1,
                position = position_dodge(0.9)) +
  labs(x = "Subjective Class in State 2",
       y = "Proportion Within State 2 Class",
       fill = "Subjective Class in State 1") +
  guides(color = guide_legend(override.aes=list(fill=NA), # getting rid of grey background 
                            nrow=1),
       shape = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = axis_tick_size)) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=axis_tick_size)) +
  theme(axis.title = element_text(size = axis_title_size)) +
  theme(legend.text = element_text(size = axis_title_size)) + 
  theme(legend.title = element_text(size = axis_title_size))  +
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.title.x.top = element_text(size = axis_title_size, family = "Times New Roman")) +
  scale_x_discrete(labels = c("Lower", "Working", "Middle","Upper")) +
  scale_fill_manual(labels = c("Lower", "Working", "Middle","Upper"), values = c('chartreuse3', 'dodgerblue3', "firebrick3", "gold3")) + 
  guides(fill = guide_legend(nrow = 2))

ggsave(filename=paste(directory_path, figure_path, "panel_state2_from_state1.png", sep = "/"), width=4.36, height=4.36)


# Statistics

# number of switches missing

missing_switches <- transition_by_person_level %>%
  mutate(switch = ifelse(state_1 != state_2, 1, 0)) %>%
  select(switch) %>%
  pull() %>%
  sum(is.na(.), na.rm=T)

# number of no swtiching 

proportion_switches <- transition_by_person_level %>%
  mutate(switch = ifelse(state_1 != state_2, 1, 0)) %>%
  filter(!is.na(switch)) %>%
  select(switch) %>%
  pull() %>%
  mean(., na.rm=T)

proportion_non_switches <- transition_by_person_level %>%
  mutate(switch = ifelse(state_1 != state_2, 1, 0),
         non_switch = ifelse(state_1 == state_2, 1, 0)) %>%
  filter(!is.na(non_switch)) %>%
  select(non_switch) %>%
  pull() %>%
  mean(., na.rm=T)

# check for differential attrition across wave 1 vs wave 2

attrition_regression_1 <- gss_panel06_long %>%
  pivot_wider(., names_from = wave, names_prefix = "wave_", values_from = class) %>%
  select(firstid, wave_1, wave_2, wave_3) %>%
  mutate(wave_1_na = ifelse(is.na(wave_1), 1, 0),
         wave_2_na = ifelse(is.na(wave_2), 1, 0),
         wave_3_na = ifelse(is.na(wave_3), 1, 0)) %>%
  pivot_longer(., cols = c("wave_1", "wave_2", "wave_3"), 
               names_to = "wave",
               values_to = "class") %>%
  filter(wave == "wave_1") %>%
  select(c(wave_2_na, class)) %>%
  mutate(x = as.factor(class),
         y = wave_2_na) %>%
  lm(y ~ x, data = .) %>%
  summary()


attrition_regression_2 <- gss_panel06_long %>%
  pivot_wider(., names_from = wave, names_prefix = "wave_", values_from = class) %>%
  select(firstid, wave_1, wave_2, wave_3) %>%
  mutate(wave_1_na = ifelse(is.na(wave_1), 1, 0),
         wave_2_na = ifelse(is.na(wave_2), 1, 0),
         wave_3_na = ifelse(is.na(wave_3), 1, 0)) %>%
  pivot_longer(., cols = c("wave_1", "wave_2", "wave_3"), 
               names_to = "wave",
               values_to = "class") %>%
  filter(wave == "wave_2") %>%
  select(c(wave_3_na, class)) %>%
  mutate(x = as.factor(class),
         y = wave_3_na) %>%
  lm(y ~ x, data = .) %>%
  summary()



