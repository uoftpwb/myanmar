#title: "Myanmar Project Analysis"
#author: Phyllis Lun
#date: '2025-06-18'

#Loading libraries
p_list <- c("data.table",  "Hmisc", "jtools", "sf", "ggplot2", "ggpubr", "ggrepel", "patchwork", "lubridate", "tidyverse", "EValue")

# Check if packages are installed, install if needed, then load
for (pkg in p_list) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Customised functions ---------------------------------------------------------

# Data -------------------------------------------------------------------------

## Load data ------------------------------------------------------------------
gallup_world_raw <- readRDS("data/(250701)GWP_cleaned_MyanmarProject.rds")

dim(gallup_world_raw)
objects(gallup_world_raw)

## Processing data -------------------------------------------------------------
gallup_world <- gallup_world_raw %>%
  mutate(YEAR_INTERVIEW = as.numeric(as.character(YEAR_INTERVIEW))) %>%
  filter(YEAR_INTERVIEW >= 2014 & YEAR_INTERVIEW <= 2024) %>%
  mutate(YEAR_SINCE_EVENT = case_when(YEAR_INTERVIEW < 2021 ~ "Pre-coup",
                                      YEAR_INTERVIEW == 2021 ~ "2021",
                                      YEAR_INTERVIEW == 2022 ~ "2022",
                                      YEAR_INTERVIEW == 2023 ~ "2023",
                                      YEAR_INTERVIEW == 2024 ~ "2024")) %>%
  mutate(YEAR_SINCE_EVENT = factor(YEAR_SINCE_EVENT, levels = c("Pre-coup", "2021", "2022", "2023", "2024"))) %>%
  mutate(AGE_GROUP = factor(cut(WP1220, 
                                breaks = c(0, 40, 60, Inf),
                                labels = c('Under 40', '40-60', 'Above 60'),
                                include.lowest = TRUE),
                            levels = c('Under 40', '40-60', 'Above 60'))) %>%
  mutate(INCOME_5 = case_when(INCOME_5 == 1 ~ "Poorest 20%",
                              INCOME_5 == 2 ~ "Second 20%",
                              INCOME_5 == 3 ~ "Middle 20%",
                              INCOME_5 == 4 ~ "Fourth 20%",
                              INCOME_5 == 5 ~ "Richest 20%")) %>%
  mutate(INCOME_5 = factor(INCOME_5, levels = c("Poorest 20%", "Second 20%", "Middle 20%", "Fourth 20%", "Richest 20%"))) %>%

#Changing some variables to factors for analyses 
  mutate(WP1219=case_when(WP1219==0~ "Female",
                          WP1219==1~ "Male",
                          TRUE~ NA_character_)) %>% 
  mutate(WP1219=factor(WP1219, levels=c("Female", "Male"))) %>%
  mutate(WP1223=case_when(WP1223==1~ "Single/Never Married",
                          WP1223==2~ "Married",
                          WP1223%in% c(3,4)~ "Separated/Divorced",
                          WP1223==5~ "Widowed",
                          WP1223==8~ "Domestic Partner",
                          TRUE~ NA_character_)) %>% 
  mutate(WP1223=factor(WP1223, levels=c("Single/Never Married", "Married", "Separated/Divorced", "Widowed", "Domestic Partner"))) %>%
  mutate(WP14=case_when(WP14 %in% c(1,2)~ "Rural",
                        WP14 %in% c(3,6)~ "Urban",
                        TRUE~ NA_character_)) %>% 
  mutate(WP14=factor(WP14, levels=c("Rural", "Urban"))) %>%
  mutate(WP134=case_when(WP134==1~ "Satisfied",
                         WP134==0~ "Dissatisfied",
                         TRUE~ NA_character_)) %>% 
  mutate(WP134=factor(WP134, levels=c("Dissatisfied", "Satisfied"))) %>%
  mutate(WP150=case_when(WP150==1~ "Approved",
                         WP150==0~ "Disapproved",
                         TRUE~ NA_character_)) %>% 
  mutate(WP150=factor(WP150, levels=c("Disapproved", "Approved"))) %>%

#Confidence in military, judicial system, national government, and police
  mutate(WP137=case_when(WP137==1~ "Yes",
                         WP137==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP137=factor(WP137, levels=c("No", "Yes"))) %>%
  mutate(WP138=case_when(WP138==1~ "Yes",
                         WP138==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP138=factor(WP138, levels=c("No", "Yes"))) %>%
  mutate(WP139=case_when(WP139==1~ "Yes",
                         WP139==0~ "No",
                         TRUE~ NA_character_)) %>% 
#Confidence in electoral system
  mutate(WP144=case_when(WP144==1~ "Yes",
                         WP144==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP144=factor(WP144, levels=c("No", "Yes"))) %>%
#Confidence in business
  mutate(WP145=case_when(WP145==1~ "Yes",
                         WP145==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP145=factor(WP145, levels=c("No", "Yes"))) %>%
#Confidence in government
  mutate(WP146=case_when(WP146==1~ "Yes",
                         WP146==0~ "No",
                         TRUE~ NA_character_)) %>% 
#Necessity: Not enough food
  mutate(WP40=case_when(WP40==1~ "Yes",
                         WP40==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP40=factor(WP40, levels=c("No", "Yes"))) %>%
#Necessity: Not enough shelter
  mutate(WP43=case_when(WP43==1~ "Yes",
                         WP43==0~ "No",
                         TRUE~ NA_character_)) %>% 
  mutate(WP43=factor(WP43, levels=c("No", "Yes"))) %>%
#Good affordable housing
  mutate(WP98=case_when(WP98==1~ "Satisfied",
                         WP98==0~ "Dissatisfied",
                         TRUE~ NA_character_)) %>% 
  mutate(WP98=factor(WP98, levels=c("Dissatisfied", "Satisfied"))) 


#Extracting Myanmar data---------
objects(gallup_world)

gallup_myanmar <- gallup_world %>%
  mutate(REGION_MMR = case_when(
    REGION_MMR == 1 ~ "Chin State",
    REGION_MMR == 2 ~ "Kachin State",
    REGION_MMR == 3 ~ "Kayah State",
    REGION_MMR == 4 ~ "Kayin State",
    REGION_MMR == 5 ~ "Mon State",
    REGION_MMR == 6 ~ "Rakhine State",
    REGION_MMR == 7 ~ "Shan State",
    REGION_MMR == 8 ~ "Ayeyarwady Region",
    REGION_MMR == 9 ~ "Bago Region",
    REGION_MMR == 10 ~ "Magway Region",
    REGION_MMR == 11 ~ "Mandalay Region",
    REGION_MMR == 12 ~ "Sagaing Region",
    REGION_MMR == 13 ~ "Tanintharyi Region",
    REGION_MMR == 14 ~ "Yangon Region",
    REGION_MMR == 15 ~ "Naypyidaw Union Territory",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(REGION_MMR))
                  
gallup_myanmar %>% group_by(YEAR_INTERVIEW) %>% summarise(sum(WGT, na.rm = TRUE))

saveRDS(gallup_myanmar, "data/GWP_myanmar_2014_2024.rds")
gallup_myanmar <- readRDS("data/GWP_myanmar_2014_2024.rds") 
nrow(gallup_myanmar) #11760

#Creating the summary table for WP16 and WP18
# Analysis ---------------------------------------------------------------------

SWB_myanmar <- gallup_myanmar %>% group_by(YEAR_INTERVIEW) %>% 
    summarise(
        mid_date = mean(WP4, na.rm = TRUE),
        WP16_mean = stats::weighted.mean(WP16, WGT, na.rm = TRUE),
        WP16_se = sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
        WP18_mean = stats::weighted.mean(WP18, WGT, na.rm = TRUE),
        WP18_se = sqrt(Hmisc::wtd.var(WP18, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))) %>%
    mutate(WP16_lowci = WP16_mean - 1.96 * WP16_se,
           WP16_upci = WP16_mean + 1.96 * WP16_se,
           WP18_lowci = WP18_mean - 1.96 * WP18_se,
           WP18_upci = WP18_mean + 1.96 * WP18_se) %>%
    select(YEAR_INTERVIEW, mid_date, WP16_mean, WP16_lowci, WP16_upci, WP18_mean, WP18_lowci, WP18_upci)

SWB_myanmar

#Pivoting the data to long format
SWB_myanmar_long <- SWB_myanmar %>%
  pivot_longer(
    cols = c(WP16_mean, WP16_lowci, WP16_upci, WP18_mean, WP18_lowci, WP18_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(WP16|WP18)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )
View(SWB_myanmar_long)

#Plotting the data

myanmar_SWB_plot <-  SWB_myanmar_long %>%
  ggplot(aes(x = mid_date, y = mean, ymin = lowci, ymax = upci, color = variable, group = variable)) +
   
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 9.2, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
 
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 9.2, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
 
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(fill = variable), size=0) +
  scale_color_manual(values = c("#e76f51", "#2a9d8f"), labels = c("Life Satisfaction", "Hope"), name = "") +
  scale_fill_manual(values = c("#e76f51", "#2a9d8f"), labels = c("Life Satisfaction", "Hope"), name = "") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2014-01-01", "2024-12-31")), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  theme_classic() +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.1), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 14, face = 'bold'), 
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Mean Well-being Score (0-10)", title = "Mean Life Satisfaction and Hope in Myanmar, 2014-2024\n")

myanmar_SWB_plot

ggsave("figures/myanmar_SWB_plot.png", myanmar_SWB_plot, width = 12, height = 8)


# Affective well-being ---------------------------------------------------------
Affective_myanmar <- gallup_myanmar %>% group_by(YEAR_INTERVIEW) %>% 
    summarise(
        mid_date = mean(WP4, na.rm = TRUE),
        smile_mean = stats::weighted.mean(WP63, WGT, na.rm = TRUE), #Happiness
        smile_se = sqrt(Hmisc::wtd.var(WP63, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),

        enjoy_mean = stats::weighted.mean(WP67, WGT, na.rm = TRUE), #Joy
        enjoy_se = sqrt(Hmisc::wtd.var(WP67, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
        
        worry_mean = stats::weighted.mean(WP69, WGT, na.rm = TRUE), #Worry
        worry_se = sqrt(Hmisc::wtd.var(WP69, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
        
        sad_mean = stats::weighted.mean(WP70, WGT, na.rm = TRUE), #Sadness
        sad_se = sqrt(Hmisc::wtd.var(WP70, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
        
        stress_mean = stats::weighted.mean(WP71, WGT, na.rm = TRUE), #Stress
        stress_se = sqrt(Hmisc::wtd.var(WP71, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
        
        anger_mean = stats::weighted.mean(WP74, WGT, na.rm = TRUE), #Anger
        anger_se = sqrt(Hmisc::wtd.var(WP74, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
        ) %>%
    mutate(smile_lowci = smile_mean - 1.96 * smile_se,
           smile_upci = smile_mean + 1.96 * smile_se,
           enjoy_lowci = enjoy_mean - 1.96 * enjoy_se,
           enjoy_upci = enjoy_mean + 1.96 * enjoy_se,
           worry_lowci = worry_mean - 1.96 * worry_se,
           worry_upci = worry_mean + 1.96 * worry_se,
           sad_lowci = sad_mean - 1.96 * sad_se,
           sad_upci = sad_mean + 1.96 * sad_se,
           stress_lowci = stress_mean - 1.96 * stress_se,
           stress_upci = stress_mean + 1.96 * stress_se,
           anger_lowci = anger_mean - 1.96 * anger_se,
           anger_upci = anger_mean + 1.96 * anger_se) %>%
     select(YEAR_INTERVIEW, mid_date, smile_mean, smile_lowci, smile_upci, enjoy_mean, enjoy_lowci, enjoy_upci, worry_mean, worry_lowci, worry_upci, sad_mean, sad_lowci, sad_upci, stress_mean, stress_lowci, stress_upci, anger_mean, anger_lowci, anger_upci)

Affective_myanmar

#Pivoting the data to long format
Affective_myanmar_long <- Affective_myanmar %>%
  pivot_longer(
    cols = c(smile_mean, smile_lowci, smile_upci, 
      enjoy_mean, enjoy_lowci, enjoy_upci, 
      worry_mean, worry_lowci, worry_upci, 
      sad_mean, sad_lowci, sad_upci, 
      stress_mean, stress_lowci, stress_upci, 
      anger_mean, anger_lowci, anger_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(smile|enjoy|worry|sad|stress|anger)_(mean|lowci|upci)",
    values_to = "value"
  ) %>% pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>% 
  mutate (variable = case_when(variable == "smile" ~ "Smiling",
                               variable == "enjoy" ~ "Enjoyment",
                               variable == "worry" ~ "Worry",
                               variable == "sad" ~ "Sadness",
                               variable == "stress" ~ "Stress",
                               variable == "anger" ~ "Anger")) %>%
  mutate(variable = factor(variable, levels = c("Smiling", "Enjoyment", "Worry", "Sadness", "Stress", "Anger")))

myanmar_Affective_plot <-  Affective_myanmar_long %>% mutate(mean = mean * 100, lowci = lowci * 100, upci = upci * 100) %>%
  ggplot(aes(x = mid_date, y = mean, ymin = lowci, ymax = upci, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 92, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 92, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
   geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(fill = variable), size=0) +
  scale_color_manual(values = c("#f4a261", "#2a9d8f", "#e9c46a", "#264653", "#e76f51", "#d62828"), labels = c("Smiling", "Enjoyment", "Worry", "Sadness", "Stress", "Anger"), name = "Affect") +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f", "#e9c46a", "#264653", "#e76f51", "#d62828"), labels = c("Smiling", "Enjoyment", "Worry", "Sadness", "Stress", "Anger"), name = "Affect") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2014-01-01", "2024-12-31")), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Feeling Each Affect", title = "Affective Well-being in Myanmar, 2014-2024\n")

myanmar_Affective_plot

ggsave("figures/myanmar_Affective_plot.png", myanmar_Affective_plot, width = 12, height = 8)


##Descriptives of moderators ---------------------------------------------------
objects(gallup_myanmar)


### Distribution of religions (WP1233) in 2014 ----------------------------------
#Get the weighted distribution of religions in 2014
#multiple the frequency by the weight
myanmar_desc_distribution_of_religions_plot_2014<- gallup_myanmar%>% filter(YEAR_INTERVIEW==2014) %>% filter(!is.na(WP1233)) %>% 
  group_by(WP1233) %>% 
  summarise(religion_sum = sum(WGT)) %>%
  mutate(WP1233 = as.numeric(WP1233)) %>%
  mutate(WP1233 = case_when(WP1233 == 0 ~ "Other",
                            WP1233 == 1 ~ "Christianity\n(Roman Catholic,\nCatholic)",
                            WP1233 == 2 ~ "Christianity\n(Protestant,\nAnglican,\nEvangelical,\netc.)",
                            WP1233 == 3 ~ "Christianity\n(Eastern Orthodox,\nOrthodoxy,\netc.)",
                            WP1233 == 4 ~ "Islam/Muslim",
                            WP1233 == 5 ~ "Islam/Muslim\n(Shiite)",
                            WP1233 == 6 ~ "Islam/Muslim\n(Sunni)",
                            WP1233 == 7 ~ "Druze",
                            WP1233 == 8 ~ "Hinduism",
                            WP1233 == 9 ~ "Buddhism",
                            WP1233 == 10 ~ "Primal-indigenous/African Traditional/Animist",
                            WP1233 == 11 ~ "Chinese Traditional Religion/Confucianism",
                            WP1233 == 12 ~ "Sikhism",
                            WP1233 == 13 ~ "Juche",
                            WP1233 == 14 ~ "Spiritism",
                            WP1233 == 15 ~ "Judaism",
                            WP1233 == 16 ~ "Baha'i",
                            WP1233 == 17 ~ "Jainism",
                            WP1233 == 18 ~ "Shinto",
                            WP1233 == 19 ~ "Cao Dai",
                            WP1233 == 20 ~ "Zoroastrianism",
                            WP1233 == 21 ~ "Tenrikyo",
                            WP1233 == 22 ~ "Neo-Paganism",
                            WP1233 == 23 ~ "Unitarian-Universalism",
                            WP1233 == 24 ~ "Rastafarianism",
                            WP1233 == 25 ~ "Scientology",
                            WP1233 == 26 ~ "Secular\n(Nonreligious,\nAgnostic,\nAtheist,\nNone)",
                            WP1233 == 28 ~ "Christian\n(not specified)",
                            WP1233 == 29 ~ "Taoism/Daoism",
                            TRUE ~ "Other")) %>%
  ggplot(aes(x = WP1233, y = religion_sum)) +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, 100)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 10), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Religion", y = "Proportion of Respondents", 
  title = "Distribution of Religions in Myanmar, 2014\n")

ggsave("figures/myanmar_desc_distribution_of_religions_plot_2014.png", myanmar_desc_distribution_of_religions_plot_2014, width = 12, height = 8)

#2024
myanmar_desc_distribution_of_religions_plot_2024<- gallup_myanmar%>% filter(YEAR_INTERVIEW==2024) %>% filter(!is.na(WP1233)) %>% 
  group_by(WP1233) %>% 
  summarise(religion_sum = sum(WGT)) %>%
  mutate(WP1233 = as.numeric(WP1233)) %>%
  mutate(WP1233 = case_when(WP1233 == 0 ~ "Other",
                            WP1233 == 1 ~ "Christianity\n(Roman Catholic,\nCatholic)",
                            WP1233 == 2 ~ "Christianity\n(Protestant,\nAnglican,\nEvangelical,\netc.)",
                            WP1233 == 3 ~ "Christianity\n(Eastern Orthodox,\nOrthodoxy,\netc.)",
                            WP1233 == 4 ~ "Islam/Muslim",
                            WP1233 == 5 ~ "Islam/Muslim\n(Shiite)",
                            WP1233 == 6 ~ "Islam/Muslim\n(Sunni)",
                            WP1233 == 7 ~ "Druze",
                            WP1233 == 8 ~ "Hinduism",
                            WP1233 == 9 ~ "Buddhism",
                            WP1233 == 10 ~ "Primal-indigenous/African Traditional/Animist",
                            WP1233 == 11 ~ "Chinese Traditional Religion/Confucianism",
                            WP1233 == 12 ~ "Sikhism",
                            WP1233 == 13 ~ "Juche",
                            WP1233 == 14 ~ "Spiritism",
                            WP1233 == 15 ~ "Judaism",
                            WP1233 == 16 ~ "Baha'i",
                            WP1233 == 17 ~ "Jainism",
                            WP1233 == 18 ~ "Shinto",
                            WP1233 == 19 ~ "Cao Dai",
                            WP1233 == 20 ~ "Zoroastrianism",
                            WP1233 == 21 ~ "Tenrikyo",
                            WP1233 == 22 ~ "Neo-Paganism",
                            WP1233 == 23 ~ "Unitarian-Universalism",
                            WP1233 == 24 ~ "Rastafarianism",
                            WP1233 == 25 ~ "Scientology",
                            WP1233 == 26 ~ "Secular\n(Nonreligious,\nAgnostic,\nAtheist,\nNone)",
                            WP1233 == 28 ~ "Christian\n(not specified)",
                            WP1233 == 29 ~ "Taoism/Daoism",
                            TRUE ~ "Other")) %>%
  ggplot(aes(x = WP1233, y = religion_sum)) +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, 100)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 10), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Religion", y = "Proportion of Respondents", 
  title = "Distribution of Religions in Myanmar, 2024\n")

ggsave("figures/myanmar_desc_distribution_of_religions_plot_2024.png", myanmar_desc_distribution_of_religions_plot_2024, width = 12, height = 8)


  
###Freedom of Media ------------------------------------------------------------
myanmar_desc_freedom_of_media_plot<- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    freedom_of_media_mean = stats::weighted.mean(WP10251, WGT, na.rm = TRUE),
    freedom_of_media_se = sqrt(Hmisc::wtd.var(WP10251, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(freedom_of_media_lowci = freedom_of_media_mean - 1.96 * freedom_of_media_se,
         freedom_of_media_upci = freedom_of_media_mean + 1.96 * freedom_of_media_se) %>%
  select(YEAR_INTERVIEW, freedom_of_media_mean, freedom_of_media_lowci, freedom_of_media_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = freedom_of_media_mean*100)) +
  geom_point() +
  geom_errorbar(aes(ymin = freedom_of_media_lowci*100, ymax = freedom_of_media_upci*100), width = 0.2) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Feeling Freedom of Media", 
  title = "Freedom of Media in Myanmar, 2014-2024\n")

ggsave("figures/myanmar_desc_freedom_of_media_plot.png", myanmar_desc_freedom_of_media_plot, width = 12, height = 8)

###Racial/Ethnic Minorities ----------------------------------------------------

objects(gallup_myanmar)

myanmar_desc_racial_ethnic_minorities_plot<- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    racial_ethnic_minorities_mean = stats::weighted.mean(WP103, WGT, na.rm = TRUE),
    racial_ethnic_minorities_se = sqrt(Hmisc::wtd.var(WP103, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(racial_ethnic_minorities_lowci = racial_ethnic_minorities_mean - 1.96 * racial_ethnic_minorities_se,
         racial_ethnic_minorities_upci = racial_ethnic_minorities_mean + 1.96 * racial_ethnic_minorities_se) %>%
  select(YEAR_INTERVIEW, racial_ethnic_minorities_mean, racial_ethnic_minorities_lowci, racial_ethnic_minorities_upci) %>%
   #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = racial_ethnic_minorities_mean*100)) +
  geom_point() +
  geom_errorbar(aes(ymin = racial_ethnic_minorities_lowci*100, ymax = racial_ethnic_minorities_upci*100), width = 0.2) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Friendliness of Their City/ Area to Racial/Ethnic Minorities in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_racial_ethnic_minorities_plot.png", myanmar_desc_racial_ethnic_minorities_plot, width = 12, height = 8)

###Importance of Religion (WP109) ------------------------------------------------------

myanmar_desc_importance_religion_plot<- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    importance_of_religion_mean = stats::weighted.mean(WP109, WGT, na.rm = TRUE),
    importance_of_religion_se = sqrt(Hmisc::wtd.var(WP109, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(importance_of_religion_lowci = importance_of_religion_mean - 1.96 * importance_of_religion_se,
         importance_of_religion_upci = importance_of_religion_mean + 1.96 * importance_of_religion_se) %>%
  select(YEAR_INTERVIEW, importance_of_religion_mean, importance_of_religion_lowci, importance_of_religion_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = importance_of_religion_mean*100)) +
  geom_point() +
  geom_errorbar(aes(ymin = importance_of_religion_lowci*100, ymax = importance_of_religion_upci*100), width = 0.2) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 12 , face = 'bold'), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Importance of Religion in Myanmar, 2014-2024\n") 
ggsave("figures/myanmar_desc_importance_religion_plot.png", myanmar_desc_importance_religion_plot, width = 12, height = 8)

##Benevolence of People --------------------------------------------------------

###Donating Money (WP108), Volunteering (WP109), Helping Strangers (WP110) ------------------------------------------------------

myanmar_desc_benevolence_plot<- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    donating_money_mean = stats::weighted.mean(WP108, WGT, na.rm = TRUE),
    donating_money_se = sqrt(Hmisc::wtd.var(WP108, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    volunteering_mean = stats::weighted.mean(WP109, WGT, na.rm = TRUE),
    volunteering_se = sqrt(Hmisc::wtd.var(WP109, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    helping_others_mean = stats::weighted.mean(WP110, WGT, na.rm = TRUE),
    helping_others_se = sqrt(Hmisc::wtd.var(WP110, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(donating_money_lowci = donating_money_mean - 1.96 * donating_money_se,
         donating_money_upci = donating_money_mean + 1.96 * donating_money_se,
         volunteering_lowci = volunteering_mean - 1.96 * volunteering_se,
         volunteering_upci = volunteering_mean + 1.96 * volunteering_se,
         helping_others_lowci = helping_others_mean - 1.96 * helping_others_se,
         helping_others_upci = helping_others_mean + 1.96 * helping_others_se) %>%
  select(YEAR_INTERVIEW, donating_money_mean, donating_money_lowci, donating_money_upci, volunteering_mean, 
  volunteering_lowci, volunteering_upci, helping_others_mean, helping_others_lowci, helping_others_upci) %>%
  #Plotting all three variables on the same plot
  pivot_longer(
    cols = c(donating_money_mean, donating_money_lowci, donating_money_upci, 
      volunteering_mean, volunteering_lowci, volunteering_upci, 
      helping_others_mean, helping_others_lowci, helping_others_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(donating_money|volunteering|helping_others)_(mean|lowci|upci)",
    values_to = "value"
  ) %>% pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>% 
  mutate (variable = case_when(variable == "donating_money" ~ "Donating Money",
                               variable == "volunteering" ~ "Volunteering",
                               variable == "helping_others" ~ "Helping Strangers")) %>%
  mutate(variable = factor(variable, labels = c("Donating Money", "Volunteering", "Helping Strangers"))) %>%
  ggplot(aes(x = YEAR_INTERVIEW, y = mean*100, ymin = lowci*100, ymax = upci*100, color = variable, group = variable)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  geom_line() +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.1), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Benevolence of People in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_benevolence_plot.png", myanmar_desc_benevolence_plot, width = 12, height = 8)

##Would like to live in a different country (WP1325) ------------------------------------------------------

myanmar_desc_different_country_plot<- gallup_myanmar %>%
  filter(!is.na(WP1325)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    would_like_to_live_in_different_country_mean = stats::weighted.mean(WP1325, WGT, na.rm = TRUE),
    would_like_to_live_in_different_country_se = sqrt(Hmisc::wtd.var(WP1325, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(would_like_to_live_in_different_country_lowci = would_like_to_live_in_different_country_mean - 1.96 * would_like_to_live_in_different_country_se,
         would_like_to_live_in_different_country_upci = would_like_to_live_in_different_country_mean + 1.96 * would_like_to_live_in_different_country_se) %>%
  select(YEAR_INTERVIEW, would_like_to_live_in_different_country_mean, would_like_to_live_in_different_country_lowci, would_like_to_live_in_different_country_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = would_like_to_live_in_different_country_mean*100)) +
  geom_point() +
  geom_errorbar(aes(ymin = would_like_to_live_in_different_country_lowci*100, ymax = would_like_to_live_in_different_country_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents", 
  title = "Would like to live in a different country in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_different_country_plot.png", myanmar_desc_different_country_plot, width = 12, height = 8)


###Confidence in military (WP137), 2015-2020------------------------------------------------------

table(gallup_myanmar$WP137)


myanmar_desc_confidence_in_military_plot<- gallup_myanmar %>%
  filter(!is.na(WP137)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP137=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Having Confidence in Military in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_confidence_in_military_plot.png", myanmar_desc_confidence_in_military_plot, width = 12, height = 8)


###Confidence in judicial system (WP138) ------------------------------------------------------


myanmar_desc_confidence_in_judicial_system_plot<- gallup_myanmar %>%
  filter(!is.na(WP138)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP138=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Having Confidence in Judicial System in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_confidence_in_judicial_system_plot.png", myanmar_desc_confidence_in_judicial_system_plot, width = 12, height = 8)


### Confidence in national government (WP139) ------------------------------------------------------


myanmar_desc_confidence_in_national_government_plot<- gallup_myanmar %>%
  filter(!is.na(WP139)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP139=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data  
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Having Confidence in National Government in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_confidence_in_national_government_plot.png", myanmar_desc_confidence_in_national_government_plot, width = 12, height = 8)


### Confidence in honesty of election (WP144) ------------------------------------------------------

myanmar_desc_confidence_in_honesty_of_election_plot<- gallup_myanmar %>%
  filter(!is.na(WP144)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP144=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents Agreeing", 
  title = "Having Confidence in Honesty of Election in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_confidence_in_honesty_of_election_plot.png", myanmar_desc_confidence_in_honesty_of_election_plot, width = 12, height = 8)

### Approval of job performance of head of state (WP150) ------------------------------------------------------

myanmar_desc_approval_of_head_of_state_plot<- gallup_myanmar %>%
  filter(!is.na(WP150)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP150=="Approved"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents", 
  title = "Approving Job Performance of country leadership in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_approval_of_head_of_state_plot.png", myanmar_desc_approval_of_head_of_state_plot, width = 12, height = 8)

###Not enough food (WP40) ------------------------------------------------------

myanmar_desc_not_enough_food_plot<- gallup_myanmar %>%
  filter(!is.na(WP40)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP40=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents", 
  title = "Not enough food at least one day in the past 12 monthsin Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_not_enough_food_plot.png", myanmar_desc_not_enough_food_plot, width = 12, height = 8)


##Not enough shelter (WP43) ------------------------------------------------------

myanmar_desc_not_enough_shelter_plot<- gallup_myanmar %>%
  filter(!is.na(WP43)) %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(prop=sum(WGT[WP43=="Yes"])/sum(WGT),prop_se=sqrt(prop*(1-prop)/sum(WGT))) %>%
  mutate(prop_lowci=prop-1.96*prop_se, prop_upci=prop+1.96*prop_se) %>%
  select(YEAR_INTERVIEW, prop, prop_lowci, prop_upci) %>%
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = prop*100)) +
  geom_point() + 
  geom_errorbar(aes(ymin = prop_lowci*100, ymax = prop_upci*100), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 12), 
    legend.title = element_blank(), 
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
  ) +
  labs(x = "Year", y = "Proportion of Respondents", 
  title = "Not enough shelter at least one day in the past 12 months in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_not_enough_shelter_plot.png", myanmar_desc_not_enough_shelter_plot, width = 12, height = 8)


##Life satisfaction and hope by regions (WP16 and WP18, REGION_MMR)--------------------------------

unique(gallup_myanmar$REGION_MMR)

myanmar_desc_LS_hope_by_regions_plot<-
  gallup_myanmar %>%
  filter(!is.na(WP16),!is.na(WGT), !is.na(WP18)) %>%
  group_by(YEAR_INTERVIEW, REGION_MMR) %>%
  summarize(LS_mean=Hmisc::wtd.mean(WP16, WGT, na.rm = TRUE),
            LS_se=sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
            Hope_mean=Hmisc::wtd.mean(WP18, WGT, na.rm = TRUE),
            Hope_se=sqrt(Hmisc::wtd.var(WP18, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))) %>%
  mutate(LS_lowci=LS_mean-1.96*LS_se, LS_upci=LS_mean+1.96*LS_se,
         Hope_lowci=Hope_mean-1.96*Hope_se, Hope_upci=Hope_mean+1.96*Hope_se) %>%
  pivot_longer(
    cols = c(LS_mean, LS_lowci, LS_upci, Hope_mean, Hope_lowci, Hope_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(LS|Hope)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>% 
  select(YEAR_INTERVIEW, REGION_MMR, variable, mean, lowci, upci) %>% 
  #Plotting the data
  ggplot(aes(x = YEAR_INTERVIEW, y = mean, color = variable, group = variable)) +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = lowci, ymax = upci), width = 0.15) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  theme_bw(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(color = 'black', size = 14, angle = 45, hjust = 1), 
    axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14), 
    axis.text.x.top = element_text(size = 14, face = 'bold'), 
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), 'lines'), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 14, face = 'bold'), 
    plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, color = "gray50"),
    panel.spacing = unit(1 , "lines") 
  )+
  facet_wrap(~REGION_MMR, nrow = 3)+
  labs(x = "Year", y = "Life Satisfaction and Hope", 
  title = "Life Satisfaction and Hope by Regions in Myanmar, 2014-2024\n") 

ggsave("figures/myanmar_desc_LS_hope_by_regions_plot.png", myanmar_desc_LS_hope_by_regions_plot, width = 20, height =15)


gallup_myanmar %>%
  filter(!is.na(WP16)&!is.na(WGT)) %>%
  group_by(YEAR_INTERVIEW, REGION_MMR) %>%
  summarize(total_n=sum(WGT, na.rm = TRUE),
            LS_mean=Hmisc::wtd.mean(WP16, WGT, na.rm = TRUE),
            LS_se=sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))) %>%
  mutate(LS_lowci=LS_mean-1.96*LS_se, LS_upci=LS_mean+1.96*LS_se) %>%
  select(YEAR_INTERVIEW, REGION_MMR, total_n,LS_mean, LS_lowci, LS_upci) %>% 
  filter(REGION_MMR %in% c("Chin State", "Kachin State", "Kayah State")) %>% 
  arrange(REGION_MMR)
