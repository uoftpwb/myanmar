---
title: "Myanmar Project Analysis"
author: Phyllis Lun
date: '2025-06-18'
---

# Libraries --------------------------------------------------------------------
# List of required packages
packages <- c("data.table",  "Hmisc", "jtools", "sf", 
              "ggplot2", "ggpubr", "ggrepel", "patchwork", "lubridate", "tidyverse", "EValue")

# Check if packages are installed, install if needed, then load
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Customised functions ---------------------------------------------------------

## Create function to calculate confidence interval--------------------------------
calculate_ci <- function(x, weights, conf.level = 0.95) {
  x <- na.omit(x) # Remove NA values from the data vector
  weights <- weights[!is.na(x)] # Align weights with the non-NA data values
  nx <- length(x) # Calculate the number of non-NA observations
  df <- nx - 1 # Calculate degrees of freedom
  vx <- Hmisc::wtd.var(x, weights) # Calculate weighted variance without normalizing weights
  mx <- stats::weighted.mean(x, weights) # Calculate the weighted mean of the data
  stderr <- sqrt(vx/nx) # Calculate the standard error of the weighted mean
  alpha <- 1 - conf.level # Calculate the alpha value (significance level) for the confidence interval
  cint <- qt(1 - alpha/2, df) * stderr # Determine the critical t value for the CI, multiplied by standard error
  lower_ci <- mx - cint # Calculate the lower bound of the confidence interval
  upper_ci <- mx + cint # Calculate the upper bound of the confidence interval
  return(data.frame(lower_ci = lower_ci, upper_ci = upper_ci)) # Return a data frame with lower and upper CI bounds
}

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
  filter(!is.na(REGION_MMR))

gallup_myanmar %>% group_by(YEAR_INTERVIEW) %>% summarise(sum(WGT, na.rm = TRUE))


#Creating the summary table for WP16 and WP18

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

print(SWB_myanmar_long, n = 100)


#Plotting the data

SWB_myanmar_long %>%
  ggplot(aes(x = mid_date, y = mean, ymin = lowci, ymax = upci, color = variable)) +
  geom_pointrange() +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_minimal() +
  labs(x = "Year", y = "Mean", title = "Myanmar SWB")


# Analysis ---------------------------------------------------------------------

Trend of life satisfaction and hope
