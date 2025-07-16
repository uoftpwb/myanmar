# Name: Conflict data extraction and analysis
# Author: Phyllis Lun
# Date: 2025-07-04
# Description: This script extracts conflict data from a database and analyzes it.

# Load the necessary libraries
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(tidytext)
library(tidyquant)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

#From all the regional ACLED data from the data folder
acled_asia_pacific <- read.csv("data/acled/Asia-Pacific_2018-2025_Jul04.csv")
dim(acled_asia_pacific)
objects(acled_asia_pacific)

#Extracting relevant columns
acled_asia_pacific_extract <- 
  acled_asia_pacific %>%
  filter(year >= 2014 & year <= 2024) %>%
  select(event_id_cnty, event_date, year, disorder_type, event_type, actor1, actor2, 
  iso, country, admin1, admin2, admin3, location, fatalities) %>%
  mutate(event_date = as.Date(event_date)) 


#Filtering for Myanmar and only conflicts
acled_mmr<-acled_asia_pacific_extract %>% 
  filter(country == "Myanmar")  %>% #nrow() 92156
  filter(!disorder_type %in% c("Demonstrations", "Strategic developments")) #%>% nrow() #nrow 57615

saveRDS(acled_mmr, "data/acled/acled_mmr.rds")

acled_mmr <- readRDS("data/acled/acled_mmr.rds")
dim(acled_mmr)
objects(acled_mmr)

##DISORDER--------------------------

###Data visualization:Distribution of conflicts by year

#This includes battles, excessive force against protesters, mob violence in riots, explosions/ remote violence, violence against civilians. 
acled_mmr_disorder_plot <- acled_mmr %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_bar(stat = "identity", fill = "#450920") +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0), breaks = seq(0, 15000, 1000)) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    title = "Frequency of conflicts in Myanmar by year (2014-2024)",
    x = "Year",
    y = "Number of events",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence,\nviolence against civilians."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
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
    plot.caption = element_text(size = 10, hjust = 0, color = "gray50")
  )

ggsave("figures/acled_mmr_disorder_plot.png", acled_mmr_disorder_plot, width = 10, height = 6)


###Data visualization:Distribution of conflicts by region and year 

head(acled_mmr)

unique(acled_mmr$admin1) #18 regions

acled_mmr_disorder_region_plot <- acled_mmr %>%
  group_by(year, admin1) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 1000)) +
  facet_wrap(~ admin1, ncol = 3, nrow = 6) +
  theme_light() +
  labs(
    title = "Distribution of conflicts in Myanmar by region and year (2014-2024)\n",
    x = "\nYear\n",
    y = "\nNumber of events\n",
    caption = "Notes: \n1. Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence, violence against civilians.\n2. The dotted line represents the year 2021, when the military coup d'état occurred."
  ) +
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
    panel.spacing = unit(2 , "lines") 
  )

ggsave("figures/acled_mmr_disorder_region_plot.png", acled_mmr_disorder_region_plot, width = 16, height = 14)

##FATALITIES--------------------------

#Frequency of reported fatalities by year
acled_mmr_fatalities_plot <- acled_mmr %>%
  group_by(year) %>%
  summarise(n = sum(fatalities)) %>%
  ggplot(aes(x = year, y = n), color = "#432818") +
  geom_bar(stat = "identity", fill = "#432818") +
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0), breaks = seq(0, 30000, 5000)) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    title = "Numberof reported fatalities in Myanmar by year (2014-2024)",
    x = "Year",
    y = "Number of fatalities",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence,\nviolence against civilians."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = 'black'),
    axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14), 
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
    plot.caption = element_text(size = 10, hjust = 0, color = "gray50")
  )

ggsave("figures/acled_mmr_fatalities_plot.png", acled_mmr_fatalities_plot, width = 10, height = 6)


#Distribution of fatalities by region and year
head(acled_mmr)
unique(acled_mmr$admin1) #18 regions

acled_mmr_fatalities_region_plot <- acled_mmr %>%
  group_by(year, admin1) %>%
  summarise(n = sum(fatalities)) %>% 
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 1000)) +
  facet_wrap(~ admin1, ncol = 3, nrow = 6) +
  theme_light() +
  labs(
    title = "Distribution of fatalities in Myanmar by region and year (2014-2024)\n",
    x = "\nYear\n",
    y = "\nNumber of fatalities\n",
    caption = "Notes: \n1. Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence, violence against civilians.\n2. The dotted line represents the year 2021, when the military coup d'état occurred."
  ) +
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
    panel.spacing = unit(2 , "lines") 
  )

ggsave("figures/acled_mmr_fatalities_region_plot.png", acled_mmr_fatalities_region_plot, width = 16, height = 14)



#UPPSALA CONFLICT DATASET--------------------------
#
#uppsala_conflict_dataset <- readRDS("data/uppsala/GEDEvent_v25_1.rds")
#dim(uppsala_conflict_dataset)
#objects(uppsala_conflict_dataset)
#
#unique(uppsala_conflict_dataset$country)
#
#uppsala_conflict_dataset_extract<- uppsala_conflict_dataset %>%
#  filter(year >= 2014 & year <= 2024) %>%
#  select(id, year, active_year, type_of_violence, conflict_new_id, dyad_new_id, dyad_name, side_a_new_id, side_a, side_b_new_id, side_b, 
#    adm_1, adm_2, country, country_id, region, event_clarity, date_prec, date_start, date_end, deaths_a, deaths_b, deaths_civilians, deaths_unknown, best, low) 
#nrow(uppsala_conflict_dataset_extract) # 213606
#
#uppsala_conflict_dataset_mmr <- uppsala_conflict_dataset_extract %>%
#  filter(country == "Myanmar (Burma)")
#nrow(uppsala_conflict_dataset_mmr) # 6261




