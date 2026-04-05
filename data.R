install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "tidycensus"))

# Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tidycensus)

# SET API KEY
# -----------------------------
census_api_key("046fb55b77651b85fa665c605e7c35b7d89905ce", install = TRUE)

years <- c(2013, 2015, 2017, 2019, 2022, 2024)

# 1. NAEP API FUNCTION
# -----------------------------
get_naep <- function(year) {
  
  url <- "https://www.nationsreportcard.gov/Dataservice/GetAdhocData.aspx"
  
  res <- GET(url, query = list(
    Type = "Data",
    subject = "reading",
    grade = "4",
    year = year,
    jurisdiction = "states",
    variable = "avgscore"
  ))
  
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  df <- as.data.frame(data)
  
  df <- df %>%
    rename(
      NAME = Jurisdiction,
      avg_naep_score = Value
    ) %>%
    mutate(year = year) %>%
    select(NAME, year, avg_naep_score)
  
  return(df)
}

naep_df <- bind_rows(lapply(years, get_naep))

# 2. CENSUS (ACS) DATA
# -----------------------------
get_census_data <- function(year) {
  
  get_acs(
    geography = "state",
    variables = c(
      median_income = "B19013_001",
      poverty = "B17001_002"
    ),
    year = year,
    survey = "acs5"
  ) %>%
    select(NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(year = year)
}

census_df <- bind_rows(lapply(years, get_census_data))

# 3. CLEAN + MERGE
# -----------------------------
# Standardize names
naep_df$NAME <- toupper(naep_df$NAME)
census_df$NAME <- toupper(census_df$NAME)

merged_df <- naep_df %>%
  inner_join(census_df, by = c("NAME", "year"))

# 4. FEATURE ENGINEERING
# -----------------------------
merged_df <- merged_df %>%
  mutate(
    #avg_naep_score = as.numeric(avg_naep_score),
    post2020 = ifelse(year >= 2022, 1, 0),
    poverty_rate = poverty / 1000000,
    broadband_rate = broadband / 1000000
  ) %>%
  drop_na()

# 5. SAVE DATASET
# -----------------------------
write.csv(merged_df, "final_dataset.csv", row.names = FALSE)

# Preview
head(merged_df)