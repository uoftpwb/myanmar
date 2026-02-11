# title: "Myanmar Project Analysis"
# author: Phyllis Lun
# date: '2025-06-18'

# Loading libraries
p_list <- c("data.table", "Hmisc", "jtools", "sf", "ggplot2", "ggpubr", "ggrepel", "patchwork", "lubridate", "tidyverse", "EValue", "sf", "ggtext", "shadowtext")

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
gallup_world_cleaned <- readRDS("data/(250918)GWP_cleaned_MyanmarProject.rds")

## Processing data -------------------------------------------------------------
gallup_world <- gallup_world_cleaned %>%
  mutate(AGE_GROUP = factor(
    cut(WP1220,
      breaks = c(0, 30, 45, 60, Inf),
      labels = c("Under 30", "30-44", "45-59", "Above 60"), # 15–29, 30–44, 45–59, and 60 years old and above.
      include.lowest = TRUE
    ),
    levels = c("Under 30", "30-44", "45-59", "Above 60")
  )) %>%
  mutate(INCOME_5 = case_when(
    INCOME_5 == 1 ~ "Poorest 20%",
    INCOME_5 == 2 ~ "Second 20%",
    INCOME_5 == 3 ~ "Middle 20%",
    INCOME_5 == 4 ~ "Fourth 20%",
    INCOME_5 == 5 ~ "Richest 20%"
  )) %>%
  mutate(INCOME_5 = factor(INCOME_5, levels = c("Poorest 20%", "Second 20%", "Middle 20%", "Fourth 20%", "Richest 20%"))) %>%
  # Changing some variables to factors for analyses
  mutate(WP1219 = case_when(
    WP1219 == 0 ~ "Female",
    WP1219 == 1 ~ "Male",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP1219 = factor(WP1219, levels = c("Female", "Male"))) %>%
  mutate(WP1223 = case_when(
    WP1223 == 1 ~ "Single/Never Married",
    WP1223 == 2 ~ "Married",
    WP1223 %in% c(3, 4) ~ "Separated/Divorced",
    WP1223 == 5 ~ "Widowed",
    WP1223 == 8 ~ "Domestic Partner",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP1223 = factor(WP1223, levels = c("Single/Never Married", "Married", "Separated/Divorced", "Widowed", "Domestic Partner"))) %>%
  mutate(WP14 = case_when(
    WP14 %in% c(1, 2) ~ "Rural",
    WP14 %in% c(3, 6) ~ "Urban",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP14 = factor(WP14, levels = c("Rural", "Urban"))) %>%
  mutate(WP134 = case_when(
    WP134 == 1 ~ "Satisfied",
    WP134 == 0 ~ "Dissatisfied",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP134 = factor(WP134, levels = c("Dissatisfied", "Satisfied"))) %>%
  mutate(WP150 = case_when(
    WP150 == 1 ~ "Approved",
    WP150 == 0 ~ "Disapproved",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP150 = factor(WP150, levels = c("Disapproved", "Approved"))) %>%
  # Confidence in military, judicial system, national government, and police
  mutate(WP137 = case_when(
    WP137 == 1 ~ "Yes",
    WP137 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP137 = factor(WP137, levels = c("No", "Yes"))) %>%
  mutate(WP138 = case_when(
    WP138 == 1 ~ "Yes",
    WP138 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP138 = factor(WP138, levels = c("No", "Yes"))) %>%
  mutate(WP139 = case_when(
    WP139 == 1 ~ "Yes",
    WP139 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  # Confidence in electoral system
  mutate(WP144 = case_when(
    WP144 == 1 ~ "Yes",
    WP144 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP144 = factor(WP144, levels = c("No", "Yes"))) %>%
  # Confidence in business
  mutate(WP145 = case_when(
    WP145 == 1 ~ "Yes",
    WP145 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP145 = factor(WP145, levels = c("No", "Yes"))) %>%
  # Confidence in government
  mutate(WP146 = case_when(
    WP146 == 1 ~ "Yes",
    WP146 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  # Social support
  mutate(WP27 = case_when(
    WP27 == 1 ~ "Yes",
    WP27 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP27 = factor(WP27, levels = c("No", "Yes"))) %>%
  # Necessity: Satisfaction with standard of living
  mutate(WP30 = case_when(
    WP30 == 1 ~ "Satisfied",
    WP30 == 0 ~ "Dissatisfied",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP30 = factor(WP30, levels = c("Dissatisfied", "Satisfied"))) %>%
  # Necessity: Not enough food
  mutate(WP40 = case_when(
    WP40 == 1 ~ "Yes",
    WP40 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP40 = factor(WP40, levels = c("No", "Yes"))) %>%
  # Necessity: Not enough shelter
  mutate(WP43 = case_when(
    WP43 == 1 ~ "Yes",
    WP43 == 0 ~ "No",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP43 = factor(WP43, levels = c("No", "Yes"))) %>%
  # Good affordable housing
  mutate(WP98 = case_when(
    WP98 == 1 ~ "Satisfied",
    WP98 == 0 ~ "Dissatisfied",
    TRUE ~ NA_character_
  )) %>%
  mutate(WP98 = factor(WP98, levels = c("Dissatisfied", "Satisfied"))) %>%
  # Religion
  mutate(BUDDHISM = case_when(
    WP1233RECODED %in% c(1, 2, 3, 5, 6, 7) ~ "Other regions/Secular",
    WP1233RECODED == 4 ~ "Buddhism",
    TRUE ~ NA_character_
  )) %>%
  mutate(BENEVOLENCE = rowSums(select(., WP108, WP109, WP110), na.rm = TRUE))

#################################################################################################################################
# Create composite variables for positive affect and negative affect
# First let's check the number of these affect iterms were asked in each country, every year

# For each item, calculate the proportion of affirmative answers (1),
# accounting for potential missing questions by using the valid denominator.

affect_items <- list(
  POSITIVE = c("WP59", "WP60", "WP61", "WP63", "WP64", "WP65", "WP67", "WP76"), # WP59, WP64, WP76 are asked in less than 15% of the country-years so they are taken out
  NEGATIVE = c("WP68", "WP69", "WP70", "WP71", "WP74")
)

# First, check which affect columns actually exist in the dataset
available_affect_cols <- intersect(unlist(affect_items), names(gallup_world))
cat("Available affect columns:", paste(available_affect_cols, collapse = ", "), "\n")
cat("Missing affect columns:", paste(setdiff(unlist(affect_items), names(gallup_world)), collapse = ", "), "\n")

# First, extract number of PA/NA questions asked at country-year level
affect_items

# Check which PA and NA variables are consistently asked across all country-years
consistent_affect_items <- affect_questions_asked %>%
  group_by(AFFECT_TYPE) %>%
  summarise(
    total_country_years = n_distinct(paste(COUNTRYNEW, YEAR_INTERVIEW)),
    .groups = "drop"
  )

# Get the total number of unique country-years in the dataset
total_country_years <- gallup_world %>%
  distinct(COUNTRYNEW, YEAR_INTERVIEW) %>%
  nrow()

cat("Total country-years in dataset:", total_country_years, "\n")
print(consistent_affect_items)

# Check consistency by individual items - need to check where questions were NOT missing
item_consistency <- gallup_world %>%
  select(COUNTRYNEW, YEAR_INTERVIEW, all_of(available_affect_cols)) %>%
  pivot_longer(
    cols = all_of(available_affect_cols),
    names_to = "AFFECT_ITEM",
    values_to = "RESPONSE"
  ) %>%
  mutate(
    AFFECT_TYPE = case_when(
      AFFECT_ITEM %in% affect_items$POSITIVE ~ "POSITIVE_AFFECT",
      AFFECT_ITEM %in% affect_items$NEGATIVE ~ "NEGATIVE_AFFECT"
    ),
    was_asked = !is.na(RESPONSE) # TRUE if question was asked (not missing)
  ) %>%
  group_by(AFFECT_ITEM, AFFECT_TYPE) %>%
  summarise(
    n_country_years_asked = n_distinct(paste(COUNTRYNEW[was_asked], YEAR_INTERVIEW[was_asked])),
    n_total_country_years = n_distinct(paste(COUNTRYNEW, YEAR_INTERVIEW)),
    prop_coverage = round(n_country_years_asked / total_country_years, 5),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_coverage))

# Now extract number of PA/NA questions asked at country-year level
affect_questions_asked <- gallup_world %>%
  select(COUNTRYNEW, YEAR_INTERVIEW, any_of(available_affect_cols)) %>%
  tidyr::pivot_longer(
    cols = available_affect_cols,
    names_to = "AFFECT_ITEM",
    values_to = "RESPONSE"
  ) %>%
  mutate(
    AFFECT_TYPE = case_when(
      AFFECT_ITEM %in% affect_items$POSITIVE ~ "POSITIVE_AFFECT",
      AFFECT_ITEM %in% affect_items$NEGATIVE ~ "NEGATIVE_AFFECT"
    )
  ) %>%
  group_by(COUNTRYNEW, YEAR_INTERVIEW, AFFECT_TYPE, AFFECT_ITEM) %>%
  summarise(
    n_asked = sum(!is.na(RESPONSE)),
    .groups = "drop"
  ) %>%
  filter(n_asked > 0) %>% # Only keep items that were actually asked
  group_by(COUNTRYNEW, YEAR_INTERVIEW, AFFECT_TYPE) %>%
  summarise(
    n_items_asked = n(),
    items_asked = paste(AFFECT_ITEM, collapse = ", "),
    .groups = "drop"
  )

affect_questions_asked

# Check which PA and NA variables are consistently asked across all country-years
consistent_affect_items <- affect_questions_asked %>%
  group_by(AFFECT_TYPE) %>%
  summarise(
    total_country_years = n_distinct(paste(COUNTRYNEW, YEAR_INTERVIEW)),
    .groups = "drop"
  )

# Get the total number of unique country-years in the dataset
total_country_years <- gallup_world %>%
  distinct(COUNTRYNEW, YEAR_INTERVIEW) %>%
  nrow()

cat("Total country-years in dataset:", total_country_years, "\n")
print(consistent_affect_items)

# Check consistency by individual items - need to check where questions were NOT missing
item_consistency <- gallup_world %>%
  select(COUNTRYNEW, YEAR_INTERVIEW, all_of(available_affect_cols)) %>%
  pivot_longer(
    cols = all_of(available_affect_cols),
    names_to = "AFFECT_ITEM",
    values_to = "RESPONSE"
  ) %>%
  mutate(
    AFFECT_TYPE = case_when(
      AFFECT_ITEM %in% affect_items$POSITIVE ~ "POSITIVE_AFFECT",
      AFFECT_ITEM %in% affect_items$NEGATIVE ~ "NEGATIVE_AFFECT"
    ),
    was_asked = !is.na(RESPONSE) # TRUE if question was asked (not missing)
  ) %>%
  group_by(AFFECT_ITEM, AFFECT_TYPE) %>%
  summarise(
    n_country_years_asked = n_distinct(paste(COUNTRYNEW[was_asked], YEAR_INTERVIEW[was_asked])),
    n_total_country_years = n_distinct(paste(COUNTRYNEW, YEAR_INTERVIEW)),
    prop_coverage = round(n_country_years_asked / total_country_years, 5),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_coverage))

cat("\nItem-level consistency (proportion of country-years where each item was asked):\n")
print(item_consistency, n = Inf)
item_consistency %>% select(-n_total_country_years)

############################################################################################################
# Now let's create the composite variables for positive and negative affect
# (proportion of affirmative answers over the total number of questions asked)

# First we remove the items that were asked in less than 15% of the country-years
high_coverage_items <- item_consistency %>%
  filter(prop_coverage > 0.90) %>%
  pull(AFFECT_ITEM)

# Separate into positive and negative items with high coverage
positive_items_filtered <- intersect(affect_items$POSITIVE, high_coverage_items)
negative_items_filtered <- intersect(affect_items$NEGATIVE, high_coverage_items)

cat("High coverage positive affect items:", paste(positive_items_filtered, collapse = ", "), "\n") # WP60, WP61, WP63, WP65, WP67
cat("High coverage negative affect items:", paste(negative_items_filtered, collapse = ", "), "\n") # WP68, WP69, WP70, WP71, WP74

# Now create the composite variables for positive and negative affect at individual level
gallup_world <- gallup_world %>%
  mutate(
    # Count number of positive affect questions answered "yes" (1)
    POSITIVE_AFFECT_SUM = rowSums(select(., all_of(positive_items_filtered)) == 1, na.rm = TRUE),
    # Count number of positive affect questions that were asked (not NA)
    POSITIVE_AFFECT_N = rowSums(!is.na(select(., all_of(positive_items_filtered)))),
    # Proportion of positive affect
    POSITIVE_AFFECT = ifelse(POSITIVE_AFFECT_N > 0, POSITIVE_AFFECT_SUM / POSITIVE_AFFECT_N, NA),

    # Count number of negative affect questions answered "yes" (1)
    NEGATIVE_AFFECT_SUM = rowSums(select(., all_of(negative_items_filtered)) == 1, na.rm = TRUE),
    # Count number of negative affect questions that were asked (not NA)
    NEGATIVE_AFFECT_N = rowSums(!is.na(select(., all_of(negative_items_filtered)))),
    # Proportion of negative affect
    NEGATIVE_AFFECT = ifelse(NEGATIVE_AFFECT_N > 0, NEGATIVE_AFFECT_SUM / NEGATIVE_AFFECT_N, NA)
  )

# Check the results
gallup_world %>%
  select(
    COUNTRYNEW, YEAR_INTERVIEW, POSITIVE_AFFECT, NEGATIVE_AFFECT,
    POSITIVE_AFFECT_N, NEGATIVE_AFFECT_N
  ) %>%
  head(10)

# Extracting Myanmar data---------
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
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(REGION_MMR)) %>%
  mutate(DHS_regions = case_when(
    REGION_MMR %in% c("Chin State", "Kachin State", "Kayah State", "Kayin State", "Shan State") ~ "Hilly",
    REGION_MMR %in% c("Mon State", "Rakhine State", "Tanintharyi Region") ~ "Coastal",
    REGION_MMR %in% c("Ayeyarwady Region", "Bago Region", "Yangon Region") ~ "Delta",
    REGION_MMR %in% c("Mandalay Region", "Sagaing Region", "Magway Region", "Naypyidaw Union Territory") ~ "Central Plain",
    TRUE ~ NA_character_
  )) %>%
  mutate(
    YEAR_SINCE_COUP = case_when(
      YEAR_INTERVIEW < 2021 ~ "Pre-coup",
      YEAR_INTERVIEW == 2021 ~ "2021",
      YEAR_INTERVIEW == 2022 ~ "2022",
      YEAR_INTERVIEW == 2023 ~ "2023",
      YEAR_INTERVIEW == 2024 ~ "2024"
    ),
    AFTER_COUP = ifelse(YEAR_INTERVIEW >= 2021, "Post-coup", "Pre-coup")
  ) %>%
  mutate(YEAR_SINCE_COUP = factor(YEAR_SINCE_COUP, levels = c("Pre-coup", "2021", "2022", "2023", "2024"))) %>%
  # Creating a mid interview date variable based on the survey date
  group_by(YEAR_INTERVIEW) %>%
  mutate(mid_date = mean(WP4, na.rm = TRUE)) %>%
  ungroup()

nrow(gallup_myanmar) # 13800

table(gallup_myanmar$YEAR_INTERVIEW)

saveRDS(gallup_world, "data/GWP_world_2012_2024_20251120.rds")
saveRDS(gallup_myanmar, "data/GWP_myanmar_2012_2024_20251120.rds")

# Reading the data from the RDS files-------------------------------
gallup_myanmar <- readRDS("data/GWP_myanmar_2012_2024_20251120.rds")
gallup_world <- readRDS("data/GWP_world_2012_2024_20251120.rds")

# Sample sizes considerations---------
# Getting the total and average sample size for by AFTER_COUP
gallup_myanmar %>%
  group_by(AFTER_COUP) %>%
  summarise(
    total_sample_size = sum(WGT, na.rm = TRUE),
    no_of_years_surveyed = n_distinct(YEAR_INTERVIEW), mean_sample_size = total_sample_size / no_of_years_surveyed
  )

# Sample size by region and save as a table in csv
gallup_myanmar %>%
  group_by(REGION_MMR, AFTER_COUP) %>%
  summarise(total_sample_size = sum(WGT, na.rm = TRUE)) %>%
  pivot_wider(names_from = AFTER_COUP, values_from = total_sample_size) %>%
  mutate(
    total_sample_size = round(`Post-coup` + `Pre-coup`, 2),
    pre_coup_sample_size = round(`Pre-coup`, 2),
    post_coup_sample_size = round(`Post-coup`, 2)
  ) %>%
  select(REGION_MMR, total_sample_size, pre_coup_sample_size, post_coup_sample_size) %>%
  arrange(desc(post_coup_sample_size)) %>%
  write_csv("data/myanmar_sample_size_by_region.csv")

# Sample size by DHS regions and save as a table in csv
gallup_myanmar %>%
  group_by(DHS_regions, AFTER_COUP) %>%
  summarise(total_sample_size = sum(WGT, na.rm = TRUE)) %>%
  pivot_wider(names_from = AFTER_COUP, values_from = total_sample_size) %>%
  mutate(total_sample_size = round(`Post-coup` + `Pre-coup`, 2), pre_coup_sample_size = round(`Pre-coup`, 2), post_coup_sample_size = round(`Post-coup`, 2)) %>%
  select(DHS_regions, total_sample_size, pre_coup_sample_size, post_coup_sample_size) %>%
  arrange(desc(post_coup_sample_size)) %>%
  write_csv("data/myanmar_sample_size_by_DHS_regions.csv")

# Creating the summary table for WP16 and WP18
# Analysis ---------------------------------------------------------------------

SWB_myanmar <- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    mid_date = mean(WP4, na.rm = TRUE),
    WP16_mean = stats::weighted.mean(WP16, WGT, na.rm = TRUE),
    WP16_se = sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    WP18_mean = stats::weighted.mean(WP18, WGT, na.rm = TRUE),
    WP18_se = sqrt(Hmisc::wtd.var(WP18, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    WP16_lowci = WP16_mean - 1.96 * WP16_se,
    WP16_upci = WP16_mean + 1.96 * WP16_se,
    WP18_lowci = WP18_mean - 1.96 * WP18_se,
    WP18_upci = WP18_mean + 1.96 * WP18_se
  ) %>%
  select(YEAR_INTERVIEW, mid_date, WP16_mean, WP16_lowci, WP16_upci, WP18_mean, WP18_lowci, WP18_upci)

SWB_myanmar

write_csv(SWB_myanmar, "data/myanmar_LS_hope_summary_table.csv")

# Pivoting the data to long format
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

# Plotting the data
myanmar_SWB_plot <- SWB_myanmar_long %>%
  ggplot(aes(x = mid_date, y = mean, ymin = lowci, ymax = upci, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2012-04-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2012-04-20"), y = 8.5, label = "Myanmar By-Elections on April 1st, 2012\nThe NLD won 43 out of 45 parliamentary\nseats, including Aung San Suu Kyi.", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2015-11-28"), y = 9.0, label = "Myanmar General Elections on November 8th, 2015\nThe NLD won a supermajority.", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-02-20"), y = 9.0, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(fill = variable), size = 0) +
  scale_color_manual(values = c("WP16" = "#e76f51", "WP18" = "#2a9d8f"), labels = c("WP16" = "Life Satisfaction", "WP18" = "Hope"), name = "") +
  scale_fill_manual(values = c("WP16" = "#e76f51", "WP18" = "#2a9d8f"), labels = c("WP16" = "Life Satisfaction", "WP18" = "Hope"), name = "") +
  labs(x = "", y = "Mean Score (0-10)", title = "Mean Life Satisfaction and Hope in Myanmar, 2012-2024\n") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31")), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 10),
    expand = c(0, 0),
    breaks = seq(0, 10, 1),
    # labels = function(x) ifelse(x %% 1 == 0 & x >= 0 & x <= 10, as.character(x), "")
  ) +
  # ggbreak::scale_y_break(c(0.01, 2.5), expand = expansion(0), space = 0.3) +
  theme_classic(base_size = 16) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.91, 0.2),
    legend.margin = margin(5, 5, 5, 5),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
# myanmar_SWB_plot


ggsave("figures/myanmar_SWB_plot.png", myanmar_SWB_plot, width = 14, height = 8)
# ggsave("figures/myanmar_SWB_plot.pdf", myanmar_SWB_plot, width = 14, height = 8)

# Affective well-being ---------------------------------------------------------

# Now we estimated the weighted % of positive and negative affect at the country-year level
# using the total number of questions asked in each country-year as the denominator
myanmar_affective_data <- gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarise(
    POSITIVE_AFFECT_mean = stats::weighted.mean(POSITIVE_AFFECT, WGT, na.rm = TRUE),
    POSITIVE_AFFECT_se = sqrt(Hmisc::wtd.var(POSITIVE_AFFECT, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    NEGATIVE_AFFECT_mean = stats::weighted.mean(NEGATIVE_AFFECT, WGT, na.rm = TRUE),
    NEGATIVE_AFFECT_se = sqrt(Hmisc::wtd.var(NEGATIVE_AFFECT, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    YEAR_INTERVIEW = first(YEAR_INTERVIEW[!is.na(YEAR_INTERVIEW)]),
    mid_date = first(mid_date[!is.na(mid_date)])
  ) %>%
  mutate(
    POSITIVE_AFFECT_lowci = POSITIVE_AFFECT_mean - 1.96 * POSITIVE_AFFECT_se,
    POSITIVE_AFFECT_upci = POSITIVE_AFFECT_mean + 1.96 * POSITIVE_AFFECT_se,
    NEGATIVE_AFFECT_lowci = NEGATIVE_AFFECT_mean - 1.96 * NEGATIVE_AFFECT_se,
    NEGATIVE_AFFECT_upci = NEGATIVE_AFFECT_mean + 1.96 * NEGATIVE_AFFECT_se
  ) %>%
  select(YEAR_INTERVIEW, mid_date, POSITIVE_AFFECT_mean, POSITIVE_AFFECT_lowci, POSITIVE_AFFECT_upci, NEGATIVE_AFFECT_mean, NEGATIVE_AFFECT_lowci, NEGATIVE_AFFECT_upci) %>%
  pivot_longer(
    cols = c(POSITIVE_AFFECT_mean, POSITIVE_AFFECT_lowci, POSITIVE_AFFECT_upci, NEGATIVE_AFFECT_mean, NEGATIVE_AFFECT_lowci, NEGATIVE_AFFECT_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(POSITIVE_AFFECT|NEGATIVE_AFFECT)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(variable = case_when(
    variable == "POSITIVE_AFFECT" ~ "Positive Affect",
    variable == "NEGATIVE_AFFECT" ~ "Negative Affect"
  )) %>%
  mutate(variable = factor(variable, levels = c("Positive Affect", "Negative Affect")))

print(myanmar_affective_data)

write_csv(myanmar_affective_data, "data/myanmar_affective_summary_table.csv")

myanmar_Affective_plot <- myanmar_affective_data %>%
  mutate(mean = mean * 100, lowci = lowci * 100, upci = upci * 100) %>%
  ggplot(aes(x = mid_date, y = mean, ymin = lowci, ymax = upci, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2012-04-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2012-04-20"), y = 85, label = "Myanmar By-Elections on April 1st, 2012\nThe NLD won 43 out of 45 parliamentary\nseats, including Aung San Suu Kyi.", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2015-11-28"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe NLD won a supermajority.", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-02-20"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4.5, fontface = "bold") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(fill = variable), size = 0) +
  scale_color_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Positive Affect", "Negative Affect"), name = "Affect") +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Positive Affect", "Negative Affect"), name = "Affect") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31")), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 16) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.15),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(x = "", y = "Mean Proportion of Respondents Feeling Each Affect (%)", title = "Proportion of Respondents Feeling Positive and Negative Affect in Myanmar, 2012-2024\n")

myanmar_Affective_plot

ggsave("figures/myanmar_Affective_plot.png", myanmar_Affective_plot, width = 14, height = 8)


# Descriptives of moderators ---------------------------------------------------
objects(gallup_myanmar)

## Living necessities and living standards -------------------------------------

### Not enough food (WP40) ------------------------------------------------------

myanmar_desc_not_enough_food_shelter_plot <- gallup_myanmar %>%
  filter(!is.na(WP40) & !is.na(WP43) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarize(
    food_mean = sum(WGT[WP40 == "Yes"]) / sum(WGT), food_se = sqrt(food_mean * (1 - food_mean) / sum(WGT)),
    shelter_mean = sum(WGT[WP43 == "Yes"]) / sum(WGT), shelter_se = sqrt(shelter_mean * (1 - shelter_mean) / sum(WGT))
  ) %>%
  mutate(
    food_lowci = food_mean - 1.96 * food_se, food_upci = food_mean + 1.96 * food_se,
    shelter_lowci = shelter_mean - 1.96 * shelter_se, shelter_upci = shelter_mean + 1.96 * shelter_se
  ) %>%
  select(mid_date, food_mean, food_lowci, food_upci, shelter_mean, shelter_lowci, shelter_upci) %>%
  pivot_longer(
    cols = c(food_mean, food_lowci, food_upci, shelter_mean, shelter_lowci, shelter_upci), names_to = c("variable", "statistic"),
    names_pattern = "(food|shelter)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(variable = case_when(
    variable == "food" ~ "Not enough food",
    variable == "shelter" ~ "Not enough shelter"
  )) %>%
  mutate(variable = factor(variable, levels = c("Not enough food", "Not enough shelter"))) %>%
  ggplot(aes(x = mid_date, y = mean * 100, ymin = lowci, ymax = upci, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(ymin = lowci * 100, ymax = upci * 100, fill = variable), size = 0) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_color_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Not enough food", "Not enough shelter"), name = "") +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Not enough food", "Not enough shelter"), name = "") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents",
    title = "Not enough food or shelter at least one day in the past 12 monthsin Myanmar, 2012-2024\n"
  )

myanmar_desc_not_enough_food_shelter_plot

ggsave("figures/myanmar_desc_not_enough_food_shelter_plot.png", myanmar_desc_not_enough_food_shelter_plot, width = 14, height = 7)

# Mean over time of not enough food and shelter
gallup_myanmar %>%
  filter(!is.na(WP40) & !is.na(WP43) & !is.na(WGT)) %>%
  group_by(YEAR_SINCE_COUP) %>%
  summarize(
    food_mean = sum(WGT[WP40 == "Yes"]) / sum(WGT), food_se = sqrt(food_mean * (1 - food_mean) / sum(WGT)),
    shelter_mean = sum(WGT[WP43 == "Yes"]) / sum(WGT), shelter_se = sqrt(shelter_mean * (1 - shelter_mean) / sum(WGT))
  ) %>%
  mutate(
    food_mean = food_mean * 100, food_se = food_se * 100,
    shelter_mean = shelter_mean * 100, shelter_se = shelter_se * 100
  )

## Living standard (WP30) ------------------------------------------------------

myanmar_desc_living_standard_plot <- gallup_myanmar %>%
  filter(!is.na(WP30) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarize(mean = sum(WGT[WP30 == "Dissatisfied"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(lowci = mean - 1.96 * se, upci = mean + 1.96 * se) %>%
  select(mid_date, mean, lowci, upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#219ebc") +
  geom_line(size = 1, color = "#219ebc") +
  geom_ribbon(alpha = 0.2, aes(ymin = lowci * 100, ymax = upci * 100), fill = "#219ebc", size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents",
    title = "Dissatisfaction with living standards in Myanmar, 2012-2024\n"
  )

myanmar_desc_living_standard_plot

ggsave("figures/myanmar_desc_living_standard_plot.png", myanmar_desc_living_standard_plot, width = 14, height = 7)

# Mean over time of living standard
gallup_myanmar %>%
  filter(!is.na(WP30) & !is.na(WGT)) %>%
  group_by(YEAR_INTERVIEW) %>%
  # filter(YEAR_INTERVIEW > 2021) %>%
  summarize(mean = sum(WGT[WP30 == "Dissatisfied"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(mean = mean * 100, se = se * 100)


## Prosociality --------------------------------------------------------

### Donating Money (WP108), Volunteering (WP109), Helping Strangers (WP110) ------------------------------------------------------

myanmar_desc_benevolence_plot <- gallup_myanmar %>%
  filter(!is.na(WP108) & !is.na(WP109) & !is.na(WP110) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarise(
    WP108_mean = stats::weighted.mean(WP108, WGT, na.rm = TRUE),
    WP108_se = sqrt(WP108_mean * (1 - WP108_mean) / sum(WGT, na.rm = TRUE)),
    WP109_mean = stats::weighted.mean(WP109, WGT, na.rm = TRUE),
    WP109_se = sqrt(WP109_mean * (1 - WP109_mean) / sum(WGT, na.rm = TRUE)),
    WP110_mean = stats::weighted.mean(WP110, WGT, na.rm = TRUE),
    WP110_se = sqrt(WP110_mean * (1 - WP110_mean) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    WP108_lowci = WP108_mean - 1.96 * WP108_se,
    WP108_upci = WP108_mean + 1.96 * WP108_se,
    WP109_lowci = WP109_mean - 1.96 * WP109_se,
    WP109_upci = WP109_mean + 1.96 * WP109_se,
    WP110_lowci = WP110_mean - 1.96 * WP110_se,
    WP110_upci = WP110_mean + 1.96 * WP110_se
  ) %>%
  select(
    mid_date, WP108_mean, WP108_lowci, WP108_upci, WP109_mean,
    WP109_lowci, WP109_upci, WP110_mean, WP110_lowci, WP110_upci
  ) %>%
  # Plotting all three variables on the same plot
  pivot_longer(
    cols = c(
      WP108_mean, WP108_lowci, WP108_upci,
      WP109_mean, WP109_lowci, WP109_upci,
      WP110_mean, WP110_lowci, WP110_upci
    ),
    names_to = c("variable", "statistic"),
    names_pattern = "(WP108|WP109|WP110)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(variable = case_when(
    variable == "WP108" ~ "Donating Money",
    variable == "WP109" ~ "Volunteering",
    variable == "WP110" ~ "Helping Others"
  )) %>%
  mutate(variable = factor(variable, levels = c("Donating Money", "Helping Others", "Volunteering"))) %>%
  ggplot(aes(x = mid_date, y = mean * 100, ymin = lowci * 100, ymax = upci * 100, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(ymin = lowci * 100, ymax = upci * 100, fill = variable), size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c("#ee9b00", "#9a031e", "#005f73"), labels = c("Donating Money", "Helping Others", "Volunteering"), name = "") +
  scale_fill_manual(values = c("#ee9b00", "#9a031e", "#005f73"), labels = c("Donating Money", "Helping Others", "Volunteering"), name = "") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.1),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents",
    title = "Benevolence of People in Myanmar, 2012-2024\n"
  )

myanmar_desc_benevolence_plot

ggsave("figures/myanmar_desc_benevolence_plot.png", myanmar_desc_benevolence_plot, width = 14, height = 7)


# Let's do a version of any prosocial acts in the last 12 months

# myanmar_desc_any_benevolence_plot <-
gallup_myanmar %>%
  select(mid_date, WP108, WP109, WP110, WGT) %>%
  filter(!is.na(WP108) & !is.na(WP109) & !is.na(WP110) & !is.na(WGT)) %>%
  mutate(any_prosociality = ifelse(WP108 == 1 | WP109 == 1 | WP110 == 1, 1, 0)) %>%
  group_by(mid_date) %>%
  summarise(
    any_prosociality = sum(any_prosociality, na.rm = TRUE) / sum(WGT, na.rm = TRUE) * 100,
    any_prosociality_se = sqrt(any_prosociality * (1 - any_prosociality) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    any_prosociality_lowci = any_prosociality - 1.96 * any_prosociality_se,
    any_prosociality_upci = any_prosociality + 1.96 * any_prosociality_se
  ) %>%
  select(mid_date, any_prosociality, any_prosociality_lowci, any_prosociality_upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = any_prosociality * 100)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(ymin = any_prosoiality_lowci * 100, ymax = any_prosoiality_upci * 100), size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents",
    title = "Any Prosocial Acts in the Last 12 Months in Myanmar, 2012-2024\n"
  )


## Confidence in national institutions ------------------------------------------

### Confidence in judicial system (WP138), national government (WP139), honesty of election (WP144), head of state (WP150) ------------------------------------------------------


myanmar_desc_confidence_in_institutions_plot <- gallup_myanmar %>%
  filter(!is.na(WP138) & !is.na(WP139) & !is.na(WP144) & !is.na(WP150)) %>%
  group_by(mid_date) %>%
  summarize(
    WP138_mean = sum(WGT[WP138 == "Yes"]) / sum(WGT, na.rm = TRUE), WP138_se = sqrt(WP138_mean * (1 - WP138_mean) / sum(WGT, na.rm = TRUE)),
    WP139_mean = sum(WGT[WP139 == "Yes"]) / sum(WGT, na.rm = TRUE), WP139_se = sqrt(WP139_mean * (1 - WP139_mean) / sum(WGT, na.rm = TRUE)),
    WP144_mean = sum(WGT[WP144 == "Yes"]) / sum(WGT, na.rm = TRUE), WP144_se = sqrt(WP144_mean * (1 - WP144_mean) / sum(WGT, na.rm = TRUE)),
    WP150_mean = sum(WGT[WP150 == "Approved"]) / sum(WGT, na.rm = TRUE), WP150_se = sqrt(WP150_mean * (1 - WP150_mean) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    WP138_lowci = WP138_mean - 1.96 * WP138_se, WP138_upci = WP138_mean + 1.96 * WP138_se,
    WP139_lowci = WP139_mean - 1.96 * WP139_se, WP139_upci = WP139_mean + 1.96 * WP139_se,
    WP144_lowci = WP144_mean - 1.96 * WP144_se, WP144_upci = WP144_mean + 1.96 * WP144_se,
    WP150_lowci = WP150_mean - 1.96 * WP150_se, WP150_upci = WP150_mean + 1.96 * WP150_se
  ) %>%
  select(mid_date, WP138_mean, WP138_lowci, WP138_upci, WP139_mean, WP139_lowci, WP139_upci, WP144_mean, WP144_lowci, WP144_upci, WP150_mean, WP150_lowci, WP150_upci) %>%
  # Pivoting to long format for plotting
  pivot_longer(
    cols = c(WP138_mean, WP138_lowci, WP138_upci, WP139_mean, WP139_lowci, WP139_upci, WP144_mean, WP144_lowci, WP144_upci, WP150_mean, WP150_lowci, WP150_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(WP138|WP139|WP144|WP150)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(variable = case_when(
    variable == "WP138" ~ "Judicial System",
    variable == "WP139" ~ "National Government",
    variable == "WP144" ~ "Honesty of Election",
    variable == "WP150" ~ "Head of State"
  )) %>%
  mutate(variable = factor(variable, levels = c("Judicial System", "National Government", "Honesty of Election", "Head of State"))) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = mean * 100, ymin = lowci * 100, ymax = upci * 100, color = variable, group = variable)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 3, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 3, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(fill = variable), size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c("#e76f51", "#2a9d8f", "#264653", "#ffb703"), labels = c("Judicial System", "National Government", "Honesty of Election", "Head of State"), name = "") +
  scale_fill_manual(values = c("#e76f51", "#2a9d8f", "#264653", "#ffb703"), labels = c("Judicial System", "National Government", "Honesty of Election", "Head of State"), name = "") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.9),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Agreeing",
    title = "Confidence in National Institutions in Myanmar, 2012-2024\n"
  )

myanmar_desc_confidence_in_institutions_plot

ggsave("figures/myanmar_desc_confidence_in_institutions_plot.png", myanmar_desc_confidence_in_institutions_plot, width = 14, height = 7)

# Mean over time of confidence in judicial system, national government, and honesty of election,
gallup_myanmar %>%
  filter(!is.na(WP138) & !is.na(WP139) & !is.na(WP144) & !is.na(WP150) & !is.na(WGT)) %>%
  # group_by(YEAR_INTERVIEW) %>%
  group_by(YEAR_SINCE_COUP) %>%
  summarize(
    WP138_mean = sum(WGT[WP138 == "Yes"]) / sum(WGT), WP138_se = sqrt(WP138_mean * (1 - WP138_mean) / sum(WGT)),
    WP139_mean = sum(WGT[WP139 == "Yes"]) / sum(WGT), WP139_se = sqrt(WP139_mean * (1 - WP139_mean) / sum(WGT)),
    WP144_mean = sum(WGT[WP144 == "Yes"]) / sum(WGT), WP144_se = sqrt(WP144_mean * (1 - WP144_mean) / sum(WGT)),
    WP150_mean = sum(WGT[WP150 == "Approved"]) / sum(WGT), WP150_se = sqrt(WP150_mean * (1 - WP150_mean) / sum(WGT))
  ) %>%
  mutate(
    WP138_mean = WP138_mean * 100, WP138_se = WP138_se * 100,
    WP139_mean = WP139_mean * 100, WP139_se = WP139_se * 100,
    WP144_mean = WP144_mean * 100, WP144_se = WP144_se * 100,
    WP150_mean = WP150_mean * 100, WP150_se = WP150_se * 100
  ) %>%
  # select(YEAR_SINCE_COUP, WP138_mean, WP138_se, WP139_mean, WP139_se, WP144_mean, WP144_se, WP150_mean, WP150_se) %>%
  select(YEAR_SINCE_COUP, WP138_mean, WP139_mean, WP144_mean, WP150_mean, )


### Approval of job performance of head of state (WP150) ------------------------------------------------------
# myanmar_desc_approval_of_head_of_state_plot<- gallup_myanmar %>%
#   filter(!is.na(WP150) & !is.na(WGT)) %>%
#   group_by(mid_date) %>%
#   summarize(mean=sum(WGT[WP150=="Approved"])/sum(WGT),se=sqrt(mean*(1-mean)/sum(WGT))) %>%
#   mutate(lowci=mean-1.96*se, upci=mean+1.96*se) %>%
#   select(mid_date, mean, lowci, upci) %>%
#   #Plotting the data
#   ggplot(aes(x = mid_date, y = mean*100)) +
#   geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
#   annotate("text", x = as.Date("2016-01-08"), y = 5, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
#   geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
#   annotate("text", x = as.Date("2021-04-01"), y = 5, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
#   geom_point(size = 2, color = "#3d348b") +
#   geom_line(size = 1, color = "#3d348b") +
#   geom_ribbon(aes(ymin = lowci*100, ymax = upci*100), alpha = 0.2, fill = "#3d348b") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
#   scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.ticks.x = element_line(color = 'black'),
#     axis.text.x = element_text(hjust = 0.5, color = 'black', size = 14),
#     axis.text.y = element_text(hjust = 0.5, color = 'black', size = 14),
#     axis.text.x.top = element_text(size = 14, face = 'bold'),
#     plot.margin = unit(c(0.5, 1.2, -1, 0.3), 'lines'),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = c(0.9, 0.1),
#     axis.title = element_text(size = 14, face = 'bold'),
#     legend.text = element_text(size = 12),
#     legend.title = element_blank(),
#     legend.background = element_blank(),
#     plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)
#   ) +
#   labs(x = "Year", y = "Proportion of Respondents",
#   title = "Approving Job Performance of country leadership in Myanmar, 2012-2024\n")

# myanmar_desc_approval_of_head_of_state_plot

# ggsave("figures/myanmar_desc_approval_of_head_of_state_plot.png", myanmar_desc_approval_of_head_of_state_plot, width = 14, height = 8)

### Perception of corruption in government (WP146) ------------------------------------------------------

myanmar_desc_corruption_plot <- gallup_myanmar %>%
  filter(!is.na(WP146) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarize(mean = sum(WGT[WP146 == "Yes"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(lowci = mean - 1.96 * se, upci = mean + 1.96 * se) %>%
  select(mid_date, mean, lowci, upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#723d46") +
  geom_line(size = 1, color = "#723d46") +
  geom_ribbon(aes(ymin = lowci * 100, ymax = upci * 100), alpha = 0.2, fill = "#723d46") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.1),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Agreeing",
    title = "Perception of Widespread Corruption in Government in Myanmar, 2012-2024\n"
  )

myanmar_desc_corruption_plot

ggsave("figures/myanmar_desc_corruption_plot.png", myanmar_desc_corruption_plot, width = 14, height = 8)


# Mean of corruption over time
gallup_myanmar %>%
  filter(!is.na(WP146) & !is.na(WGT)) %>%
  group_by(YEAR_SINCE_COUP) %>%
  summarize(mean = sum(WGT[WP146 == "Yes"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(mean = mean * 100, se = se * 100)

### Satisfaction with freedom (WP134) ------------------------------------------------------

gallup_myanmar$WP134

myanmar_desc_satisfaction_with_freedom_plot <- gallup_myanmar %>%
  filter(!is.na(WP134) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarize(mean = sum(WGT[WP134 == "Satisfied"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(lowci = mean - 1.96 * se, upci = mean + 1.96 * se) %>%
  select(mid_date, mean, lowci, upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 5, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 5, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#415d43") +
  geom_line(size = 1, color = "#415d43") +
  geom_ribbon(aes(ymin = lowci * 100, ymax = upci * 100), alpha = 0.2, fill = "#415d43") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.1),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Satisfied",
    title = "Satisfaction with Freedom in Myanmar, 2012-2024\n"
  )

myanmar_desc_satisfaction_with_freedom_plot

ggsave("figures/myanmar_desc_satisfaction_with_freedom_plot.png", myanmar_desc_satisfaction_with_freedom_plot, width = 14, height = 8)

# Mean of satisfaction with freedom over time
gallup_myanmar %>%
  filter(!is.na(WP134) & !is.na(WGT)) %>%
  group_by(YEAR_SINCE_COUP) %>%
  # filter(mid_date < as.Date("2021-02-01")) %>%
  summarize(mean = sum(WGT[WP134 == "Satisfied"]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(mean = mean * 100, se = se * 100)

### Would like to live in a different country (WP1325) ------------------------------------------------------

gallup_myanmar$WP1325

myanmar_desc_different_country_plot <- gallup_myanmar %>%
  filter(!is.na(WP1325) & !is.na(WGT)) %>%
  group_by(mid_date) %>%
  summarise(
    different_country_mean = sum(WGT[WP1325 == 1]) / sum(WGT),
    different_country_se = sqrt(different_country_mean * (1 - different_country_mean) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    lowci = different_country_mean - 1.96 * different_country_se,
    upci = different_country_mean + 1.96 * different_country_se
  ) %>%
  select(mid_date, different_country_mean, lowci, upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = different_country_mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#05668d") +
  geom_line(size = 1, color = "#05668d") +
  geom_ribbon(aes(ymin = lowci * 100, ymax = upci * 100), alpha = 0.2, fill = "#05668d") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents",
    title = "Would like to live in a different country in Myanmar, 2012-2024\n"
  )

myanmar_desc_different_country_plot

ggsave("figures/myanmar_desc_different_country_plot.png", myanmar_desc_different_country_plot, width = 14, height = 8)


# Mean of would like to live in a different country over time
gallup_myanmar %>%
  filter(!is.na(WP1325) & !is.na(WGT)) %>%
  group_by(YEAR_SINCE_COUP) %>%
  summarize(mean = sum(WGT[WP1325 == 1]) / sum(WGT), se = sqrt(mean * (1 - mean) / sum(WGT))) %>%
  mutate(mean = mean * 100, se = se * 100)

### Distribution of religions (WP1233) in 2012 ----------------------------------
# Get the weighted distribution of religions in 2012
# multiple the frequency by the weight
myanmar_desc_distribution_of_religions_plot_2012 <- gallup_myanmar %>%
  filter(YEAR_INTERVIEW == 2012) %>%
  filter(!is.na(WP1233)) %>%
  group_by(WP1233) %>%
  summarise(religion_sum = sum(WGT)) %>%
  mutate(WP1233 = as.numeric(WP1233)) %>%
  mutate(WP1233 = case_when(
    WP1233 == 0 ~ "Other",
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
    TRUE ~ "Other"
  )) %>%
  ggplot(aes(x = WP1233, y = religion_sum)) +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, 100)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 10),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Religion", y = "Proportion of Respondents",
    title = "Distribution of Religions in Myanmar, 2012\n"
  )
myanmar_desc_distribution_of_religions_plot_2012

ggsave("figures/myanmar_desc_distribution_of_religions_plot_2012.png", myanmar_desc_distribution_of_religions_plot_2012, width = 14, height = 8)

# 2024
myanmar_desc_distribution_of_religions_plot_2024 <- gallup_myanmar %>%
  filter(YEAR_INTERVIEW == 2024) %>%
  filter(!is.na(WP1233)) %>%
  group_by(WP1233) %>%
  summarise(religion_sum = sum(WGT)) %>%
  mutate(WP1233 = as.numeric(WP1233)) %>%
  mutate(WP1233 = case_when(
    WP1233 == 0 ~ "Other",
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
    TRUE ~ "Other"
  )) %>%
  ggplot(aes(x = WP1233, y = religion_sum)) +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, 100)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 10),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Religion", y = "Proportion of Respondents",
    title = "Distribution of Religions in Myanmar, 2024\n"
  )
myanmar_desc_distribution_of_religions_plot_2024
ggsave("figures/myanmar_desc_distribution_of_religions_plot_2024.png", myanmar_desc_distribution_of_religions_plot_2024, width = 14, height = 8)

### Freedom of Media ------------------------------------------------------------
myanmar_desc_freedom_of_media_plot <- gallup_myanmar %>%
  group_by(mid_date) %>% # Creating a mid interview date variable based on the survey date
  summarise(
    freedom_of_media_mean = stats::weighted.mean(WP10251, WGT, na.rm = TRUE),
    freedom_of_media_se = sqrt(Hmisc::wtd.var(WP10251, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    freedom_of_media_lowci = freedom_of_media_mean - 1.96 * freedom_of_media_se,
    freedom_of_media_upci = freedom_of_media_mean + 1.96 * freedom_of_media_se
  ) %>%
  select(mid_date, freedom_of_media_mean, freedom_of_media_lowci, freedom_of_media_upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = freedom_of_media_mean * 100)) + # Creating a mid interview date variable based on the survey date
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 90, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 90, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#05668d") +
  geom_line(size = 1, color = "#05668d") +
  geom_ribbon(alpha = 0.2, aes(ymin = freedom_of_media_lowci * 100, ymax = freedom_of_media_upci * 100), fill = "#05668d") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Feeling Freedom of Media",
    title = "Freedom of Media in Myanmar, 2012-2024\n"
  )
myanmar_desc_freedom_of_media_plot

ggsave("figures/myanmar_desc_freedom_of_media_plot.png", myanmar_desc_freedom_of_media_plot, width = 14, height = 8)

### Racial/Ethnic Minorities ----------------------------------------------------

objects(gallup_myanmar)

myanmar_desc_racial_ethnic_minorities_plot <- gallup_myanmar %>%
  group_by(mid_date) %>% # Creating a mid interview date variable based on the survey date
  summarise(
    racial_ethnic_minorities_mean = stats::weighted.mean(WP103, WGT, na.rm = TRUE),
    racial_ethnic_minorities_se = sqrt(Hmisc::wtd.var(WP103, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    racial_ethnic_minorities_lowci = racial_ethnic_minorities_mean - 1.96 * racial_ethnic_minorities_se,
    racial_ethnic_minorities_upci = racial_ethnic_minorities_mean + 1.96 * racial_ethnic_minorities_se
  ) %>%
  select(mid_date, racial_ethnic_minorities_mean, racial_ethnic_minorities_lowci, racial_ethnic_minorities_upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = racial_ethnic_minorities_mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 10, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 10, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#05668d") +
  geom_line(size = 1, color = "#05668d") +
  geom_ribbon(alpha = 0.2, aes(ymin = racial_ethnic_minorities_lowci * 100, ymax = racial_ethnic_minorities_upci * 100), fill = "#05668d") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Feeling Friendliness",
    title = "Friendliness of Their City/ Area to Racial/Ethnic Minorities in Myanmar, 2012-2024\n"
  )
myanmar_desc_racial_ethnic_minorities_plot
ggsave("figures/myanmar_desc_racial_ethnic_minorities_plot.png", myanmar_desc_racial_ethnic_minorities_plot, width = 14, height = 8)

### Importance of Religion (WP109) ------------------------------------------------------

myanmar_desc_importance_religion_plot <- gallup_myanmar %>%
  group_by(mid_date) %>%
  summarise(
    importance_of_religion_mean = stats::weighted.mean(WP109, WGT, na.rm = TRUE),
    importance_of_religion_se = sqrt(Hmisc::wtd.var(WP109, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
  ) %>%
  mutate(
    importance_of_religion_lowci = importance_of_religion_mean - 1.96 * importance_of_religion_se,
    importance_of_religion_upci = importance_of_religion_mean + 1.96 * importance_of_religion_se
  ) %>%
  select(mid_date, importance_of_religion_mean, importance_of_religion_lowci, importance_of_religion_upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = importance_of_religion_mean * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 10, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 10, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#05668d") +
  geom_line(size = 1, color = "#05668d") +
  geom_ribbon(alpha = 0.2, aes(ymin = importance_of_religion_lowci * 100, ymax = importance_of_religion_upci * 100), fill = "#05668d") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Agreeing",
    title = "Importance of Religion in Myanmar, 2012-2024\n"
  )

myanmar_desc_importance_religion_plot

ggsave("figures/myanmar_desc_importance_religion_plot.png", myanmar_desc_importance_religion_plot, width = 14, height = 8)



### Confidence in military (WP137), 2015-2020------------------------------------------------------

# table(gallup_myanmar$WP137)


# myanmar_desc_confidence_in_military_plot <- gallup_myanmar %>%
#   filter(!is.na(WP137)) %>%
#   group_by(mid_date) %>%
#   summarize(prop = sum(WGT[WP137 == "Yes"]) / sum(WGT), prop_se = sqrt(prop * (1 - prop) / sum(WGT))) %>%
#   mutate(prop_lowci = prop - 1.96 * prop_se, prop_upci = prop + 1.96 * prop_se) %>%
#   select(mid_date, prop, prop_lowci, prop_upci) %>%
#   # Plotting the data
#   ggplot(aes(x = mid_date, y = prop * 100)) +
#   geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
#   annotate("text", x = as.Date("2016-01-08"), y = 10, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
#   geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
#   annotate("text", x = as.Date("2021-04-01"), y = 10, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
#   geom_point(size = 2, color = "#05668d") +
#   geom_line(size = 1, color = "#05668d") +
#   geom_ribbon(alpha = 0.2, aes(ymin = prop_lowci * 100, ymax = prop_upci * 100), fill = "#05668d") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
#   scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.ticks.x = element_line(color = "black"),
#     axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
#     axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
#     axis.text.x.top = element_text(size = 14, face = "bold"),
#     plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = c(0.9, 0.13),
#     axis.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12, face = "bold"),
#     legend.background = element_blank(),
#     plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
#   ) +
#   labs(
#     x = "Year", y = "Proportion of Respondents Agreeing",
#     title = "Having Confidence in Military in Myanmar, 2012-2024\n"
#   )

# myanmar_desc_confidence_in_military_plot

# ggsave("figures/myanmar_desc_confidence_in_military_plot.png", myanmar_desc_confidence_in_military_plot, width = 14, height = 8)

## Social Support (WP27)-----------------------
gallup_myanmar$WP27

table(gallup_myanmar$WP27)

myanmar_desc_social_support_plot <- gallup_myanmar %>%
  filter(!is.na(WP27)) %>%
  group_by(mid_date) %>%
  summarize(prop = sum(WGT[WP27 == "Yes"]) / sum(WGT), prop_se = sqrt(prop * (1 - prop) / sum(WGT))) %>%
  mutate(prop_lowci = prop - 1.96 * prop_se, prop_upci = prop + 1.96 * prop_se) %>%
  select(mid_date, prop, prop_lowci, prop_upci) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = prop * 100)) +
  geom_vline(xintercept = as.Date("2015-11-08"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2016-01-08"), y = 10, label = "Myanmar General Elections on November 8th, 2015\nThe National League for Democracy won a supermajority.", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date("2021-04-01"), y = 10, label = "The military launched the coup d'état\non February 1st, 2021", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +
  geom_point(size = 2, color = "#05668d") +
  geom_line(size = 1, color = "#05668d") +
  geom_ribbon(alpha = 0.2, aes(ymin = prop_lowci * 100, ymax = prop_upci * 100), fill = "#05668d") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  theme_classic(base_size = 14) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
    axis.text.x.top = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, -1, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.13),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Year", y = "Proportion of Respondents Agreeing",
    title = "Social Support in Myanmar, 2012-2024\n"
  )

myanmar_desc_social_support_plot

ggsave("figures/myanmar_desc_social_support_plot.png", myanmar_desc_social_support_plot, width = 14, height = 8)


# By region --------------------------------------------------------------------


# ## Life satisfaction and hope by regions (WP16 and WP18, REGION_MMR)--------------------------------

# unique(gallup_myanmar$REGION_MMR)

# myanmar_desc_LS_hope_by_regions_plot <-
#   gallup_myanmar %>%
#   filter(!is.na(WP16), !is.na(WGT), !is.na(WP18)) %>%
#   group_by(YEAR_INTERVIEW, REGION_MMR) %>%
#   summarize(
#     LS_mean = Hmisc::wtd.mean(WP16, WGT, na.rm = TRUE),
#     LS_se = sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
#     Hope_mean = Hmisc::wtd.mean(WP18, WGT, na.rm = TRUE),
#     Hope_se = sqrt(Hmisc::wtd.var(WP18, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
#   ) %>%
#   mutate(
#     LS_lowci = LS_mean - 1.96 * LS_se, LS_upci = LS_mean + 1.96 * LS_se,
#     Hope_lowci = Hope_mean - 1.96 * Hope_se, Hope_upci = Hope_mean + 1.96 * Hope_se
#   ) %>%
#   pivot_longer(
#     cols = c(LS_mean, LS_lowci, LS_upci, Hope_mean, Hope_lowci, Hope_upci),
#     names_to = c("variable", "statistic"),
#     names_pattern = "(LS|Hope)_(mean|lowci|upci)",
#     values_to = "value"
#   ) %>%
#   pivot_wider(
#     names_from = statistic,
#     values_from = value
#   ) %>%
#   select(YEAR_INTERVIEW, REGION_MMR, variable, mean, lowci, upci) %>%
#   # Plotting the data
#   ggplot(aes(x = YEAR_INTERVIEW, y = mean, color = variable, group = variable)) +
#   geom_point() +
#   geom_line() +
#   geom_errorbar(aes(ymin = lowci, ymax = upci), width = 0.15) +
#   scale_x_continuous(breaks = seq(2012, 2024, 1)) +
#   scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = seq(0, 10, 1)) +
#   theme_bw(base_size = 14) +
#   theme(
#     axis.ticks.x = element_line(color = "black"),
#     axis.text.x = element_text(color = "black", size = 14, angle = 45, hjust = 1),
#     axis.text.y = element_text(hjust = 0.5, color = "black", size = 14),
#     axis.text.x.top = element_text(size = 14, face = "bold"),
#     plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "none",
#     axis.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 14),
#     legend.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
#     plot.caption = element_text(size = 14, hjust = 0, color = "gray50"),
#     panel.spacing = unit(1, "lines")
#   ) +
#   facet_wrap(~REGION_MMR, nrow = 3) +
#   labs(
#     x = "Year", y = "Life Satisfaction and Hope",
#     title = "Life Satisfaction and Hope by Regions in Myanmar, 2012-2024\n"
#   )

# ggsave("figures/myanmar_desc_LS_hope_by_regions_plot.png", myanmar_desc_LS_hope_by_regions_plot, width = 20, height = 15)


# gallup_myanmar %>%
#   filter(!is.na(WP16) & !is.na(WGT)) %>%
#   group_by(YEAR_INTERVIEW, REGION_MMR) %>%
#   summarize(
#     total_n = sum(WGT, na.rm = TRUE),
#     LS_mean = Hmisc::wtd.mean(WP16, WGT, na.rm = TRUE),
#     LS_se = sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE))
#   ) %>%
#   mutate(LS_lowci = LS_mean - 1.96 * LS_se, LS_upci = LS_mean + 1.96 * LS_se) %>%
#   select(YEAR_INTERVIEW, REGION_MMR, total_n, LS_mean, LS_lowci, LS_upci) %>%
#   filter(REGION_MMR %in% c("Chin State", "Kachin State", "Kayah State")) %>%
#   arrange(REGION_MMR)


## Life satisfaction and hope by DHS regions (WP16 and WP18, DHS_regions)--------------------------------
unique(gallup_myanmar$DHS_regions)

myanmar_desc_LS_hope_by_DHS_regions_data <-
  gallup_myanmar %>%
  filter(!is.na(WP16), !is.na(WGT), !is.na(WP18)) %>%
  group_by(mid_date, DHS_regions) %>%
  summarize(
    LS_mean = Hmisc::wtd.mean(WP16, WGT, na.rm = TRUE),
    LS_se = sqrt(Hmisc::wtd.var(WP16, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    Hope_mean = Hmisc::wtd.mean(WP18, WGT, na.rm = TRUE),
    Hope_se = sqrt(Hmisc::wtd.var(WP18, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)),
    total_n = sum(WGT, na.rm = TRUE)
  ) %>%
  mutate(
    LS_lowci = LS_mean - 1.96 * LS_se, LS_upci = LS_mean + 1.96 * LS_se,
    Hope_lowci = Hope_mean - 1.96 * Hope_se, Hope_upci = Hope_mean + 1.96 * Hope_se
  ) %>%
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
  select(mid_date, DHS_regions, variable, mean, lowci, upci, total_n) %>%
  mutate(
    YEAR_SINCE_COUP = case_when(
      lubridate::year(mid_date) < 2021 ~ "Pre-coup",
      lubridate::year(mid_date) == 2021 ~ "2021",
      lubridate::year(mid_date) == 2022 ~ "2022",
      lubridate::year(mid_date) == 2023 ~ "2023",
      lubridate::year(mid_date) == 2024 ~ "2024"
    ),
    AFTER_COUP = ifelse(lubridate::year(mid_date) >= 2021, "Post-coup", "Pre-coup")
  ) %>%
  arrange(DHS_regions, variable, mid_date)

myanmar_desc_LS_hope_by_DHS_regions_data

write_csv(myanmar_desc_LS_hope_by_DHS_regions_data, "data/myanmar_desc_LS_hope_by_DHS_regions_data.csv")

myanmar_desc_LS_hope_by_DHS_regions_plot <-
  myanmar_desc_LS_hope_by_DHS_regions_data %>%
  filter(DHS_regions %in% c("Central Plain", "Coastal", "Delta", "Hilly")) %>%
  ggplot(aes(x = mid_date, y = mean, color = variable, group = variable, fill = variable)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(ymin = lowci, ymax = upci), size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_color_manual(values = c("#e76f51", "#2a9d8f"), labels = c("Life Satisfaction", "Hope"), name = "") +
  scale_fill_manual(values = c("#e76f51", "#2a9d8f"), labels = c("Life Satisfaction", "Hope"), name = "") +
  theme_classic(base_size = 16) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 16),
    axis.text.x.top = element_text(size = 16, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    strip.text = element_text(size = 18, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~DHS_regions, nrow = 2) +
  labs(
    x = "Year", y = "Mean Score (0-10)",
    title = "Life Satisfaction and Hope by Demography and Health Surveys(DHS) Program Regions in Myanmar, 2012-2024\n"
  )

myanmar_desc_LS_hope_by_DHS_regions_plot
ggsave("figures/myanmar_desc_LS_hope_by_DHS_regions_plot.png", myanmar_desc_LS_hope_by_DHS_regions_plot, width = 20, height = 15)


# Affective well-being by DHS regions------------------------------------------------------
myanmar_desc_Affective_well_being_by_DHS_regions_data <-
  gallup_myanmar %>%
  filter(!is.na(POSITIVE_AFFECT), !is.na(NEGATIVE_AFFECT), !is.na(WGT)) %>%
  group_by(mid_date, DHS_regions) %>%
  summarize(
    POSITIVE_AFFECT_mean = Hmisc::wtd.mean(POSITIVE_AFFECT, WGT, na.rm = TRUE) * 100,
    POSITIVE_AFFECT_se = sqrt(Hmisc::wtd.var(POSITIVE_AFFECT, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)) * 100,
    NEGATIVE_AFFECT_mean = Hmisc::wtd.mean(NEGATIVE_AFFECT, WGT, na.rm = TRUE) * 100,
    NEGATIVE_AFFECT_se = sqrt(Hmisc::wtd.var(NEGATIVE_AFFECT, WGT, na.rm = TRUE) / sum(WGT, na.rm = TRUE)) * 100,
    total_n = sum(WGT, na.rm = TRUE)
  ) %>%
  mutate(
    POSITIVE_AFFECT_lowci = POSITIVE_AFFECT_mean - 1.96 * POSITIVE_AFFECT_se, POSITIVE_AFFECT_upci = POSITIVE_AFFECT_mean + 1.96 * POSITIVE_AFFECT_se,
    NEGATIVE_AFFECT_lowci = NEGATIVE_AFFECT_mean - 1.96 * NEGATIVE_AFFECT_se, NEGATIVE_AFFECT_upci = NEGATIVE_AFFECT_mean + 1.96 * NEGATIVE_AFFECT_se
  ) %>%
  pivot_longer(
    cols = c(POSITIVE_AFFECT_mean, POSITIVE_AFFECT_lowci, POSITIVE_AFFECT_upci, NEGATIVE_AFFECT_mean, NEGATIVE_AFFECT_lowci, NEGATIVE_AFFECT_upci),
    names_to = c("variable", "statistic"),
    names_pattern = "(POSITIVE_AFFECT|NEGATIVE_AFFECT)_(mean|lowci|upci)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  select(mid_date, DHS_regions, variable, mean, lowci, upci, total_n) %>%
  mutate(
    YEAR_SINCE_COUP = case_when(
      lubridate::year(mid_date) < 2021 ~ "Pre-coup",
      lubridate::year(mid_date) == 2021 ~ "2021",
      lubridate::year(mid_date) == 2022 ~ "2022",
      lubridate::year(mid_date) == 2023 ~ "2023",
      lubridate::year(mid_date) == 2024 ~ "2024"
    ),
    AFTER_COUP = ifelse(lubridate::year(mid_date) >= 2021, "Post-coup", "Pre-coup")
  ) %>%
  arrange(DHS_regions, variable, mid_date)


myanmar_desc_Affective_well_being_by_DHS_regions_data
write_csv(myanmar_desc_Affective_well_being_by_DHS_regions_data, "data/myanmar_desc_Affective_well_being_by_DHS_regions_data.csv")

myanmar_desc_Affective_well_being_by_DHS_regions_plot <-
  myanmar_desc_Affective_well_being_by_DHS_regions_data %>%
  filter(DHS_regions %in% c("Central Plain", "Coastal", "Delta", "Hilly")) %>%
  # Plotting the data
  ggplot(aes(x = mid_date, y = mean, color = variable, group = variable, fill = variable)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_ribbon(alpha = 0.2, aes(ymin = lowci, ymax = upci), size = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2012-01-01", "2024-12-31"))) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Positive Affect", "Negative Affect"), name = "") +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f"), labels = c("Positive Affect", "Negative Affect"), name = "") +
  theme_classic(base_size = 16) +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black", size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 0.5, color = "black", size = 16),
    axis.text.x.top = element_text(size = 16, face = "bold"),
    plot.margin = unit(c(0.5, 1.2, 0.5, 0.3), "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 16, hjust = 0, color = "gray50"),
    strip.text = element_text(size = 18, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~DHS_regions, nrow = 2) +
  labs(
    x = "Year", y = "Mean Proportion of Respondents Feeling Each Affect (%)",
    title = "Affective Well-being by Demography and Health Surveys(DHS) Program Regions in Myanmar, 2012-2024\n"
  )

myanmar_desc_Affective_well_being_by_DHS_regions_plot
ggsave("figures/myanmar_desc_Affective_well_being_by_DHS_regions_plot.png", myanmar_desc_Affective_well_being_by_DHS_regions_plot, width = 20, height = 15)

# Map of Myanmar and DHS regions---------
gallup_myanmar$DHS_regions
gadm41_MMR_shp <- read_sf("data/gadm41_MMR_shp/gadm41_MMR_1.shp")

# Harmonized name of the regions and add the DHS regions
gadm41_MMR_shp <- gadm41_MMR_shp %>%
  mutate(NAME_1_harmonized = case_when(
    NAME_1 == "Ayeyarwady" ~ "Ayeyarwady Region",
    NAME_1 == "Bago" ~ "Bago Region",
    NAME_1 == "Chin" ~ "Chin State",
    NAME_1 == "Kachin" ~ "Kachin State",
    NAME_1 == "Kayah" ~ "Kayah State",
    NAME_1 == "Kayin" ~ "Kayin State",
    NAME_1 == "Magway" ~ "Magway Region",
    NAME_1 == "Mandalay" ~ "Mandalay Region",
    NAME_1 == "Mon" ~ "Mon State",
    NAME_1 == "Naypyidaw" ~ "Naypyidaw Union Territory",
    NAME_1 == "Rakhine" ~ "Rakhine State",
    NAME_1 == "Sagaing" ~ "Sagaing Region",
    NAME_1 == "Shan" ~ "Shan State",
    NAME_1 == "Tanintharyi" ~ "Tanintharyi Region",
    NAME_1 == "Yangon" ~ "Yangon Region"
  )) %>%
  left_join(gallup_myanmar %>% select(DHS_regions, REGION_MMR) %>% unique(), by = c("NAME_1_harmonized" = "REGION_MMR"))


myanmar_desc_DHS_regions_map <- gadm41_MMR_shp %>%
  filter(!is.na(DHS_regions)) %>%
  ggplot() +
  geom_sf(aes(fill = DHS_regions)) +
  # Shadow layer for auto-positioned labels (excluding custom positioned ones)
  geom_sf_text(
    data = gadm41_MMR_shp %>%
      filter(!is.na(DHS_regions) & !NAME_1 %in% c("Yangon", "Ayeyarwady", "Rakhine", "Mandalay", "Tanintharyi", "Magway", "Kayin", "Mon")),
    aes(label = NAME_1), size = 2.5, color = "#F5F5F5", fontface = "bold",
    nudge_x = 0.02, nudge_y = -0.02
  ) +
  # Main text layer for auto-positioned labels (excluding custom positioned ones)
  geom_sf_text(
    data = gadm41_MMR_shp %>%
      filter(!is.na(DHS_regions) & !NAME_1 %in% c("Yangon", "Ayeyarwady", "Rakhine", "Mandalay", "Tanintharyi", "Magway", "Kayin", "Mon")),
    aes(label = NAME_1),
    size = 2.5, color = "black", fontface = "bold"
  ) +
  # Custom positioned shadow for specific labels
  geom_text(
    data = data.frame(
      x = c(96.7, 93.25, 92.6, 95.55, 100.4, 95, 98, 97.27),
      y = c(16.2, 16.5, 19.6, 21.05, 12.4, 20.2, 16.9, 15.4),
      label = c("Yangon", "Ayeyarwady", "Rakhine", "Mandalay", "Tanintharyi", "Magway", "Kayin", "Mon")
    ), aes(x = x + 0.02, y = y - 0.02, label = label),
    size = 2.5, color = "#F5F5F5", fontface = "bold"
  ) +
  # Custom positioned main text for specific labels
  geom_text(
    data = data.frame(
      x = c(96.7, 93.25, 92.6, 95.55, 100.4, 95, 98, 97.27),
      y = c(16.2, 16.5, 19.6, 21.05, 12.4, 20.2, 16.9, 15.4),
      label = c("Yangon", "Ayeyarwady", "Rakhine", "Mandalay", "Tanintharyi", "Magway", "Kayin", "Mon")
    ), aes(x = x, y = y, label = label),
    size = 2.5, color = "black", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("#fff6e4", "#8bb8c7", "#add19c", "#c9bdb7"),
    labels = c("Central Plain", "Coastal", "Delta", "Hilly"), name = "Geographical Zones"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.spacing.x = unit(0.5, "cm")
  )

myanmar_desc_DHS_regions_map

ggsave("figures/myanmar_desc_DHS_regions_map.png", myanmar_desc_DHS_regions_map, width = 14, height = 8)


# Map of SWB change by DHS regions ------------

# First we calculate the change of LS and Hope by DHS regions between pre-coup and 2021

## Life satisfaction
print(myanmar_desc_LS_hope_by_DHS_regions_data, n = 100)


myanmar_desc_LS_change_by_DHS_regions <- myanmar_desc_LS_hope_by_DHS_regions_data %>%
  filter(variable == "LS" & (YEAR_SINCE_COUP == "Pre-coup")) %>%
  group_by(DHS_regions) %>%
  summarize(LS_pre_coup_mean = weighted.mean(mean, total_n, na.rm = TRUE)) %>%
  left_join(
    myanmar_desc_LS_hope_by_DHS_regions_data %>%
      filter(variable == "LS" & (YEAR_SINCE_COUP == "2021")) %>%
      group_by(DHS_regions) %>%
      summarize(LS_2021_mean = weighted.mean(mean, total_n, na.rm = TRUE)),
    by = "DHS_regions"
  ) %>%
  mutate(LS_change_prop = (LS_2021_mean - LS_pre_coup_mean) / LS_pre_coup_mean * 100)


# Hope
myanmar_desc_Hope_change_by_DHS_regions <- myanmar_desc_LS_hope_by_DHS_regions_data %>%
  filter(variable == "Hope" & (YEAR_SINCE_COUP == "Pre-coup")) %>%
  group_by(DHS_regions) %>%
  summarize(Hope_pre_coup_mean = weighted.mean(mean, total_n, na.rm = TRUE)) %>%
  left_join(
    myanmar_desc_LS_hope_by_DHS_regions_data %>%
      filter(variable == "Hope" & (YEAR_SINCE_COUP == "2021")) %>%
      group_by(DHS_regions) %>%
      summarize(Hope_2021_mean = weighted.mean(mean, total_n, na.rm = TRUE)),
    by = "DHS_regions"
  ) %>%
  mutate(Hope_change_prop = (Hope_2021_mean - Hope_pre_coup_mean) / Hope_pre_coup_mean * 100)


# Add the changes to the shapefile
gadm41_MMR_shp <- gadm41_MMR_shp %>%
  left_join(myanmar_desc_LS_change_by_DHS_regions %>% select(NAME_1_harmonized, LS_change_prop), by = "NAME_1_harmonized") %>%
  left_join(myanmar_desc_Hope_change_by_DHS_regions %>% select(NAME_1_harmonized, Hope_change_prop), by = "NAME_1_harmonized")


gadm41_MMR_shp$NAME_1_harmonized




# Descriptive statistics -----------------------------------

# weighted sample size, by year and total
gallup_myanmar %>%
  group_by(YEAR_INTERVIEW) %>%
  summarize(total_n = sum(WGT, na.rm = TRUE)) %>%
  pull(total_n) %>%
  sum()

nrow(gallup_myanmar)

# average age of the respondents (2012-2024)
summary(gallup_myanmar$WP1220)


# Weighted proportion of women and men
gallup_myanmar %>%
  summarize(prop_women = sum(WGT[WP1219 == "Female"], na.rm = TRUE) / sum(WGT, na.rm = TRUE) * 100)


# Weighted proportion in each geogrpahical zone
gallup_myanmar %>%
  group_by(DHS_regions) %>%
  summarize(prop_women = sum(WGT[WP1219 == "Female"], na.rm = TRUE) / sum(WGT, na.rm = TRUE) * 100)


# Weighted proportion in each DHS region
gallup_myanmar %>%
  group_by(DHS_regions) %>%
  summarize(prop_women = sum(WGT[WP1219 == "Female"], na.rm = TRUE) / sum(WGT, na.rm = TRUE) * 100)


## Affective well-being------------------------------------------------------------------------------------------------------------------
