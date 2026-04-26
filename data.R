install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "tidycensus"))

# Libraries
# -----------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tidycensus)

# SET API KEY
# -----------------------------
census_api_key("046fb55b77651b85fa665c605e7c35b7d89905ce", install = TRUE)

# Parameters
# -----------------------------
years <- c(2013, 2015, 2017, 2019, 2022, 2024)
grades <- c(4, 8)
states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA",
            "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
            "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
            "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
            "UT","VT","VA","WA","WV","WI","WY")


# Mapping abbreviations → full names (DC not in base state.abb/state.name)
state_map <- bind_rows(
  data.frame(
    jurisdiction = state.abb,
    state = toupper(state.name)
  ),
  data.frame(jurisdiction = "DC", state = "DISTRICT OF COLUMBIA")
)

# 1. NAEP API FUNCTION
# -----------------------------
get_naep <- function(year, grade) {
  url <- "https://www.nationsreportcard.gov/Dataservice/GetAdhocData.aspx"
  
  res <- GET(url, query = list(
    type = "data",
    subject = "reading",
    grade = grade,
    subscale = "RRPCM",
    variable = "TOTAL",
    jurisdiction = paste(states, collapse = ","),
    stattype = "MN:MN",  
    year = year
  ))
  
  raw <- content(res, "text", encoding = "UTF-8")
  data <- fromJSON(raw)
  
  df <- data$result %>%
    select(jurisdiction, year, value) %>%   # value = score
    mutate(
      year = as.numeric(year),
      grade = as.numeric(grade)
    )
  
  return(df)
}

cat("Step 1: Fetching NAEP Scores via API...\n")
naep_df <- bind_rows(lapply(grades, function(g) {
  bind_rows(lapply(years, function(y) get_naep(y, g)))
}))

# Clean NAEP
naep_df <- naep_df %>%
  rename(score = value) %>%
  left_join(state_map, by = "jurisdiction") %>%
  select(-jurisdiction)

# 2. CENSUS (ACS) DATA
# -----------------------------
get_census_data <- function(year) {
  
  # Base variables (always exist)
  vars <- c(
    median_income = "B19013_001",
    poverty = "B17001_002",
    total_pop = "B17001_001"
  )
  
  # Add internet only for newer years
  if (year >= 2017) {
    vars <- c(vars,
              total_households = "B28002_001",
              internet_households = "B28002_002")
  }
  
  df <- get_acs(
    geography = "state",
    variables = vars,
    year = year,
    survey = "acs5"
  ) %>%
    select(NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    rename(state = NAME) %>%
    mutate(
      year = year,
      state = toupper(state),
      poverty_rate = poverty / total_pop
    )
  
  # Add internet rate only if available
  if (year >= 2017) {
    df <- df %>%
      mutate(internet_rate = internet_households / total_households) %>%
      select(-internet_households, -total_households)
  }
  
  return(df)
}

cat("Step 2: Fetching Census data...\n")
census_df <- bind_rows(lapply(years, get_census_data))

# 3. MERGE
# -----------------------------
merged_df <- naep_df %>%
inner_join(census_df, by = c("state", "year"))

# 4. FEATURE ENGINEERING
# -----------------------------
merged_df <- merged_df %>%
  mutate(
    post2020 = ifelse(year >= 2022, 1, 0)
  ) %>%
  filter(!is.na(score), !is.na(median_income), !is.na(poverty_rate)) %>%
  select(state, year, grade, score, median_income, total_pop, poverty_rate, internet_rate, post2020)

# 5. SAVE DATASET
# -----------------------------
write.csv(merged_df, "dataset.csv", row.names = FALSE)

# Preview
cat("Final dataset preview:\n")
head(merged_df)

