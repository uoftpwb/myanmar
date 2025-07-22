# Name: Conflict data extraction and analysis
# Author: Phyllis Lun
# Date: 2025-07-04
# Description: This script extracts conflict data from a database and analyzes it.

# Load the necessary libraries
p_list <- c("tidyverse", "readxl", "lubridate", "tidytext", "tidyquant", "httr", "jsonlite", "dplyr", "lubridate", "styler")

# Check if packages are installed, install if needed, then load
for (pkg in p_list) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Style the code
style_file("Testing.R")

# From all the regional ACLED data from the data folder
acled_asia_pacific <- read.csv("data/acled/Asia-Pacific_2018-2025_Jul04.csv")
dim(acled_asia_pacific)
objects(acled_asia_pacific)

# Extracting relevant columns
acled_asia_pacific_extract <-
  acled_asia_pacific %>%
  filter(year >= 2014 & year <= 2024) %>%
  select(
    event_id_cnty, event_date, year, disorder_type, event_type, actor1, actor2,
    iso, country, admin1, admin2, admin3, location, fatalities
  ) %>%
  mutate(event_date = as.Date(event_date))

# Filtering for Myanmar and only conflicts
acled_mmr <- acled_asia_pacific_extract %>%
  filter(country == "Myanmar") %>%
  mutate(admin1 = case_when(
    admin1 == "Chin" ~ "Chin State",
    admin1 == "Kachin" ~ "Kachin State",
    admin1 == "Kayah" ~ "Kayah State",
    admin1 == "Kayin" ~ "Kayin State",
    admin1 == "Mon" ~ "Mon State",
    admin1 == "Rakhine" ~ "Rakhine State",
    admin1 == "Shan-South" ~ "Shan State",
    admin1 == "Shan-North" ~ "Shan State",
    admin1 == "Shan-East" ~ "Shan State",
    admin1 == "Ayeyarwady" ~ "Ayeyarwady Region",
    admin1 == "Bago-East" ~ "Bago Region",
    admin1 == "Bago-West" ~ "Bago Region",
    admin1 == "Magway" ~ "Magway Region",
    admin1 == "Mandalay" ~ "Mandalay Region",
    admin1 == "Sagaing" ~ "Sagaing Region",
    admin1 == "Tanintharyi" ~ "Tanintharyi Region",
    admin1 == "Yangon" ~ "Yangon Region",
    admin1 == "Nay Pyi Taw" ~ "Naypyidaw Union Territory",
    TRUE ~ admin1
  )) %>%
  filter(!is.na(admin1)) %>%
  filter(!disorder_type %in% c("Demonstrations", "Strategic developments")) # %>% nrow() #nrow 57615
nrow(acled_mmr) # 50311

saveRDS(acled_mmr, "data/acled/acled_mmr.rds")

acled_mmr <- readRDS("data/acled/acled_mmr.rds")
dim(acled_mmr)
objects(acled_mmr)

# Calculating the number of conflicts and death since the coup in 2021
acled_mmr %>%
  filter(year >= 2021) %>%
  summarise(n = n(), fatalities = sum(fatalities)) #


## DISORDER--------------------------

### Data visualization:Distribution of conflicts by year

# This includes battles, excessive force against protesters, mob violence in riots, explosions/ remote violence, violence against civilians.
acled_mmr_disorder_plot <- acled_mmr %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_bar(stat = "identity", fill = "#450920") +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0), breaks = seq(0, 15000, 1000), labels = scales::comma) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    title = "Number of conflicts in Myanmar by year (2014-2024)\n (Armed Conflict Location & Event Data)",
    x = "Year",
    y = "Number of events",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots,\nexplosions/ remote violence, violence against civilians."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, color = "gray50")
  )
acled_mmr_disorder_plot

ggsave("figures/acled_mmr_disorder_plot.png", acled_mmr_disorder_plot, width = 10, height = 6)


### Data visualization:Distribution of conflicts by region and year

head(acled_mmr)
unique(acled_mmr$admin1) # 15 regions
acled_mmr_disorder_region_plot <- acled_mmr %>%
  group_by(year, admin1) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 1000), labels = scales::comma) +
  facet_wrap(~admin1, ncol = 3, nrow = 5) +
  theme_light() +
  labs(
    title = "Distribution of conflicts in Myanmar by region and year (2014-2024)\n(Armed Conflict Location & Event Data)",
    x = "\nYear\n",
    y = "\nNumber of events\n",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence, violence\nagainst civilians. The dotted line represents the year 2021, when the military coup d'état occurred.\n"
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 18),
    axis.text.x.top = element_text(size = 18, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(size = 18)
  )
acled_mmr_disorder_region_plot

ggsave("figures/acled_mmr_disorder_region_plot.png", acled_mmr_disorder_region_plot, width = 16, height = 20)

## FATALITIES--------------------------

# Frequency of reported fatalities by year
acled_mmr_fatalities_plot <- acled_mmr %>%
  group_by(year) %>%
  summarise(n = sum(fatalities)) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_bar(stat = "identity", fill = "#450920") +
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0), breaks = seq(0, 30000, 5000), labels = scales::comma) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    title = "Number of fatalities due to conflicts in Myanmar by year (2014-2024)\n (Armed Conflict Location & Event Data)",
    x = "Year",
    y = "Number of fatalities",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots,\nexplosions/ remote violence, violence against civilians."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, color = "gray50")
  )
acled_mmr_fatalities_plot
ggsave("figures/acled_mmr_fatalities_plot.png", acled_mmr_fatalities_plot, width = 10, height = 6)


# Distribution of fatalities by region and year
head(acled_mmr)
unique(acled_mmr$admin1) # 18 regions

acled_mmr_fatalities_region_plot <- acled_mmr %>%
  group_by(year, admin1) %>%
  summarise(n = sum(fatalities)) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 11000), breaks = seq(0, 11000, 2000), labels = scales::comma) +
  facet_wrap(~admin1, ncol = 3, nrow = 6) +
  theme_light() +
  labs(
    title = "Distribution of fatalities in Myanmar by region and year (2014-2024)\n(Armed Conflict Location & Event Data)",
    x = "\nYear\n",
    y = "\nNumber of fatalities\n",
    caption = "Note: Conflicts include battles, excessive force against protesters in protests, mob violence in riots, explosions/ remote violence, violence\nagainst civilians. The dotted line represents the year 2021, when the military coup d'état occurred.\n"
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 18),
    axis.text.x.top = element_text(size = 18, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(size = 18)
  )

acled_mmr_fatalities_region_plot
ggsave("figures/acled_mmr_fatalities_region_plot.png", acled_mmr_fatalities_region_plot, width = 16, height = 20)

# UPPSALA CONFLICT DATASET--------------------------

uppsala_conflict_dataset <- readRDS("data/uppsala/GEDEvent_v25_1.rds")
dim(uppsala_conflict_dataset)
objects(uppsala_conflict_dataset)

unique(uppsala_conflict_dataset$country)

uppsala_conflict_dataset_extract <- uppsala_conflict_dataset %>%
  filter(year >= 2014 & year <= 2024) %>%
  select(
    id, year, active_year, type_of_violence, conflict_new_id, dyad_new_id, dyad_name, side_a_new_id, side_a, side_b_new_id, side_b,
    adm_1, adm_2, country, country_id, region, event_clarity, date_prec, date_start, date_end, deaths_a, deaths_b, deaths_civilians, deaths_unknown, best, low
  )
nrow(uppsala_conflict_dataset_extract) # 213606

uppsala_conflict_dataset_mmr <- uppsala_conflict_dataset_extract %>%
  filter(country == "Myanmar (Burma)") %>%
  mutate(adm_1 = case_when(
    adm_1 == "Chin state" ~ "Chin State",
    adm_1 == "Kachin state" ~ "Kachin State",
    adm_1 == "Kayah state" ~ "Kayah State",
    adm_1 == "Kayin state" ~ "Kayin State",
    adm_1 == "Mon state" ~ "Mon State",
    adm_1 == "Rakhine state" ~ "Rakhine State",
    adm_1 == "Shan state" ~ "Shan State",
    adm_1 == "Ayeyarwady region" ~ "Ayeyarwady Region",
    adm_1 == "Bago region" ~ "Bago Region",
    adm_1 == "Magway region" ~ "Magway Region",
    adm_1 == "Mandalay region" ~ "Mandalay Region",
    adm_1 == "Sagaing region" ~ "Sagaing Region",
    adm_1 == "Tanintharyi region" ~ "Tanintharyi Region",
    adm_1 == "Yangon region" ~ "Yangon Region",
    adm_1 == "Naypyidaw Union territory" ~ "Naypyidaw Union Territory",
    TRUE ~ adm_1
  )) %>%
  filter(!is.na(adm_1))

nrow(uppsala_conflict_dataset_mmr) # 6212

# Calculating the number of conflicts and death since the coup in 2021
uppsala_conflict_dataset_mmr %>%
  filter(year >= 2021) %>%
  summarise(n = n(), fatalities = sum(deaths_a, deaths_b, deaths_civilians, deaths_unknown))

# Distribution of conflicts by year
uppsala_conflict_dataset_mmr_disorder_plot <- uppsala_conflict_dataset_mmr %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n), color = "#283618") +
  geom_bar(stat = "identity", fill = "#283618") +
  scale_y_continuous(limits = c(0, 2500), expand = c(0, 0), breaks = seq(0, 2500, 500), labels = scales::comma) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    title = "Number of conflicts in Myanmar by year (2014-2024)\n(Uppsala Conflict Data Program)",
    x = "Year",
    y = "Number of events",
    caption = "Note: Conflicts include state-based conflict, non-state conflict, one-sided violence. State-based and\nnon-state conflicts which resulted in at least 25 (battle-related) deaths in a year are included.\nOne-sided violence is defined as violence that resulted in at least 25 deaths."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, color = "gray50")
  )

uppsala_conflict_dataset_mmr_disorder_plot
ggsave("figures/uppsala_conflict_dataset_mmr_disorder_plot.png", uppsala_conflict_dataset_mmr_disorder_plot, width = 10, height = 6)

# Distribution of fatalities by year
uppsala_conflict_dataset_mmr_fatalities_plot <- uppsala_conflict_dataset_mmr %>%
  group_by(year) %>%
  summarise(n = sum(deaths_a, deaths_b, deaths_civilians, deaths_unknown)) %>%
  ggplot(aes(x = year, y = n), color = "#283618") +
  geom_bar(stat = "identity", fill = "#283618") +
  scale_y_continuous(limits = c(0, 3500), expand = c(0, 0), breaks = seq(0, 3500, 500), labels = scales::comma) +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  theme_classic() +
  labs(
    x = "Year",
    y = "Number of fatalities",
    title = "Number of fatalities from recorded conflicts in Myanmar by year (2014-2024)\n(Uppsala Conflict Data Program)",
    caption = "Note: Conflicts include state-based conflict, non-state conflict, one-sided violence. State-based and\nnon-state conflicts which resulted in at least 25 (battle-related) deaths in a year are included.\nOne-sided violence is defined as violence that resulted in at least 25 deaths."
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, color = "gray50")
  )

uppsala_conflict_dataset_mmr_fatalities_plot
ggsave("figures/uppsala_conflict_dataset_mmr_fatalities_plot.png", uppsala_conflict_dataset_mmr_fatalities_plot, width = 10, height = 6)


# Distributions of conflicts by admin1 region and year
unique(uppsala_conflict_dataset_mmr$adm_1)

uppsala_conflict_dataset_mmr %>%
  group_by(year, adm_1) %>%
  summarise(n = n()) %>%
  filter(!is.na(adm_1)) %>%
  filter(adm_1 %in% c("Ayeyarwady region", "Magway region", "Naypyidaw Union territory")) %>%
  arrange(desc(n))

uppsala_conflict_dataset_mmr_disorder_region_plot <- uppsala_conflict_dataset_mmr %>%
  group_by(year, adm_1) %>%
  summarise(n = n()) %>%
  filter(!is.na(adm_1)) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200), labels = scales::comma) +
  facet_wrap(~adm_1, ncol = 3, nrow = 6) +
  theme_light() +
  labs(
    title = "Distribution of conflicts in Myanmar by region and year (2014-2024)\n(Uppsala Conflict Data Program)",
    x = "\nYear\n",
    y = "\nNumber of events\n",
    caption = " Note: Conflicts include state-based conflict, non-state conflict, one-sided violence. State-based and non-state conflicts which resulted in at\nleast 25 (battle-related) deaths in a year are included. One-sided violence is defined as violence that resulted in at least 25 deaths. The\ndotted line represents the year 2021, when the military coup d'état occurred."
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 18),
    axis.text.x.top = element_text(size = 18, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(size = 18)
  ) +
  facet_wrap(~adm_1, ncol = 3, nrow = 5)

uppsala_conflict_dataset_mmr_disorder_region_plot
ggsave("figures/uppsala_conflict_dataset_mmr_disorder_region_plot.png", uppsala_conflict_dataset_mmr_disorder_region_plot, width = 16, height = 20)


### Distributions of fatalities by admin1 region and year-----------------------

objects(uppsala_conflict_dataset_mmr)

uppsala_conflict_dataset_mmr_fatalities_region_plot <- uppsala_conflict_dataset_mmr %>%
  group_by(year, adm_1) %>%
  summarise(n = sum(deaths_a, deaths_b, deaths_civilians, deaths_unknown)) %>%
  filter(!is.na(adm_1)) %>%
  ggplot(aes(x = year, y = n), color = "#450920") +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "black", size = 0.8, alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#450920") +
  scale_x_continuous(limits = c(2013, 2025), expand = c(0, 0), breaks = seq(2013, 2025, 1)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 500), labels = scales::comma) +
  facet_wrap(~adm_1, ncol = 3, nrow = 6) +
  theme_light() +
  labs(
    title = "Distribution of fatalities in Myanmar by region and year (2014-2024)\n(Uppsala Conflict Data Program)",
    x = "\nYear\n",
    y = "\nNumber of fatalities\n",
    caption = "Note: Conflicts include state-based conflict, non-state conflict, one-sided violence. State-based and non-state conflicts which resulted in at\nleast 25 (battle-related) deaths in a year are included. One-sided violence is defined as violence that resulted in at least 25 deaths. The\ndotted line represents the year 2021, when the military coup d'état occurred."
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 18),
    axis.text.x.top = element_text(size = 18, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(size = 18)
  ) +
  facet_wrap(~adm_1, ncol = 3, nrow = 5)

uppsala_conflict_dataset_mmr_fatalities_region_plot

ggsave("figures/uppsala_conflict_dataset_mmr_fatalities_region_plot.png", uppsala_conflict_dataset_mmr_fatalities_region_plot, width = 16, height = 20)
