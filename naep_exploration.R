# ==============================================================================
# Project: NAEP Learning Loss
# Scope: Digital Divide & Resilience Analysis
# Purpose: Interaction analysis of broadband access and post-2020 learning loss
# ==============================================================================

# --- 1. Load Libraries ---
library(tidyverse)
library(httr)
library(jsonlite)
library(tidycensus)
library(ggplot2)

# SET API KEY (Using established Census API key)
census_api_key("046fb55b77651b85fa665c605e7c35b7d89905ce", overwrite = TRUE)

# Analysis Window: Pre-pandemic (2017, 2019) and Post-pandemic (2022)
years <- c(2017, 2019, 2022)

# --- 2. Independent Data Ingestion (NAEP API) ---
# We fetch Grade 4 Reading scores directly from the National Report Card API.
get_na_data <- function(yr) {
  url <- "https://www.nationsreportcard.gov/Dataservice/GetAdhocData.aspx"
  res <- GET(url, query = list(
    Type = "Data", subject = "reading", grade = "4",
    year = yr, jurisdiction = "states", variable = "avgscore"
  ))
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  df <- as.data.frame(data) %>%
    rename(NAME = Jurisdiction, avg_naep_score = Value) %>%
    mutate(year = yr, NAME = toupper(NAME)) %>%
    select(NAME, year, avg_naep_score)
  return(df)
}

cat("Step 1: Fetching NAEP Scores via API...\n")
naep_df <- bind_rows(lapply(years, get_na_data))

# --- 3. Enhanced Census Data: The Digital Divide ---
# Unique Scope: Fetching Broadband households (B28002_002) vs Total (B28002_001)
get_enhanced_census <- function(yr) {
  get_acs(
    geography = "state",
    variables = c(
      total_households = "B28002_001",
      broadband_households = "B28002_002"
    ),
    year = yr,
    survey = "acs5"
  ) %>%
    select(NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(year = yr, 
           NAME = toupper(NAME),
           broadband_rate = broadband_households / total_households)
}

cat("Step 2: Fetching Census Broadband Penetration Data...\n")
census_enhanced_df <- bind_rows(lapply(years, get_enhanced_census))

# --- 4. Merge ---
# Research Question: Did state-level broadband infrastructure mitigate the 2022 score drop?
analysis_df <- census_enhanced_df %>%
  inner_join(naep_df, by = c("NAME", "year")) %>%
  mutate(period = ifelse(year >= 2022, "Post-Pandemic", "Pre-Pandemic"),
         avg_naep_score = as.numeric(avg_naep_score)) %>%
  drop_na()

# --- 5. Visualization: The Resilience Gap ---
# We use 'geom_smooth' to compare the slopes of achievement relative to connectivity.
ggplot(analysis_df, aes(x = broadband_rate, y = avg_naep_score, color = period)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  theme_minimal() +
  scale_color_manual(values = c("Pre-Pandemic" = "#3498db", "Post-Pandemic" = "#e74c3c")) +
  labs(title = "Digital Divide & Learning Loss Resilience",
       subtitle = "Interaction of Broadband Access and Pandemic-Era Achievement",
       x = "Broadband Penetration Rate (State Level)",
       y = "Avg NAEP Reading Score (Grade 4)",
       color = "Time Period",
       caption = "Analysis by M. Al Hasib | Data: NAEP & US Census ACS") +
  theme(plot.title = element_text(face = "bold", size = 16))
