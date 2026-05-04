install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "tidycensus"))
install.packages("ggcorrplot")

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
get_naep <- function(year) {
  url <- "https://www.nationsreportcard.gov/Dataservice/GetAdhocData.aspx"
  
  res <- GET(url, query = list(
    type = "data",
    subject = "reading",
    grade = 4,
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
    mutate(year = as.numeric(year))
  
  return(df)
}

cat("Step 1: Fetching NAEP Scores via API...\n")
naep_df <- bind_rows(lapply(years, get_naep))

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
  select(state, year, score, median_income, total_pop, poverty_rate, internet_rate, post2020)

# 5. SAVE DATASET
# -----------------------------
write.csv(merged_df, "dataset.csv", row.names = FALSE)

# Preview
cat("Final dataset preview:\n")
head(merged_df)

# View the first few rows of your newly merged data
head(merged_df)

# Or open it in a spreadsheet view
View(merged_df)

# Check how many rows you have (should be ~250-300 based on your years and states)
nrow(merged_df)


# The "Learning Loss" Trend (Pre vs. Post 2020)
library(ggplot2)

ggplot(merged_df, aes(x = year, y = score, group = state)) +
  geom_line(color = "lightgrey", alpha = 0.5) +
  geom_point(aes(color = as.factor(post2020)), size = 2) +
  geom_smooth(aes(group = 1), method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "NAEP 4th Grade Reading Score Trends",
       subtitle = "Red line shows the national average trend; points colored by Pre/Post 2020",
       x = "Year", y = "Average Score", color = "Post-2020")


ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_trend_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)


# 1. Redefine the function for Grade 8
get_naep_8th <- function(year) {
  url <- "https://www.nationsreportcard.gov/Dataservice/GetAdhocData.aspx"
  
  res <- GET(url, query = list(
    type = "data",
    subject = "reading",
    grade = 8,           # Changed from 4 to 8
    subscale = "RRPCM",
    variable = "TOTAL",
    jurisdiction = paste(states, collapse = ","),
    stattype = "MN:MN",  
    year = year
  ))
  
  raw <- content(res, "text", encoding = "UTF-8")
  data <- fromJSON(raw)
  
  df <- data$result %>%
    select(jurisdiction, year, value) %>% 
    mutate(year = as.numeric(year))
  
  return(df)
}

# Execute the fetch for all years
naep_df_8th <- bind_rows(lapply(years, get_naep_8th))

# Clean and Merge (This creates the missing 'merged_df_8th' object)
merged_df_8th <- naep_df_8th %>%
  rename(score = value) %>%
  left_join(state_map, by = "jurisdiction") %>%
  select(-jurisdiction) %>%
  inner_join(census_df, by = c("state", "year")) %>%
  mutate(post2020 = ifelse(year >= 2022, 1, 0))

# Now this will work:
merged_df$grade <- "4th Grade"
merged_df_8th$grade <- "8th Grade"

combined_naep <- bind_rows(merged_df, merged_df_8th)

ggplot(combined_naep, aes(x = year, y = score, color = grade)) +
  geom_smooth(method = "loess", se = FALSE, size = 2) +
  geom_point(alpha = 0.8) +
  facet_wrap(~ grade, scales = "free_y") + 
  theme_minimal() +
  scale_color_manual(values = c("4th Grade" = "#E46726", "8th Grade" = "#6D9EC1")) +
  labs(title = "Reading Score Trends: 4th vs. 8th Grade",
       subtitle = "Tracking learning loss across primary and middle levels",
       x = "Year", y = "Average Score", color = "Grade Level")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_comparison_web.png", 
       width = 12, height = 7, dpi = 150) # Lower DPI is actually better for fast web loading

# 1. Calculate the average scores for the two periods
summary_8th <- combined_naep %>%
  filter(grade == "8th Grade") %>%
  group_by(post2020) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  mutate(period = ifelse(post2020 == 1, "Post-Pandemic", "Pre-Pandemic"))

# 2. Create the Bar Chart
ggplot(summary_8th, aes(x = period, y = mean_score, fill = period)) +
  geom_col(width = 0.5, color = "black", size = 0.8) +
  geom_text(aes(label = round(mean_score, 1)), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Pre-Pandemic" = "#6D9EC1", "Post-Pandemic" = "#E46726")) +
  coord_cartesian(ylim = c(250, 275)) + # Zooming in to highlight the meaningful 3-5 point drop
  theme_minimal() +
  labs(title = "8th Grade Reading Scores: Pre vs. Post 2020",
       subtitle = "Aggregated average across all 50 states and DC",
       x = "Period", y = "Average NAEP Score") +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14, face = "bold"))

# 3. Save the visualization (using a safe filename)
ggsave("D:/PhD Study/Spring 2026/EPPS 6323/8th_grade_pre_post_bar_chart.jpg", 
       width = 8, height = 6, dpi = 300)


# The "Digital Divide" Impact

ggplot(merged_df %>% filter(!is.na(internet_rate)), 
       aes(x = internet_rate, y = score)) +
  geom_point(aes(size = total_pop, color = poverty_rate), alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Reading Scores vs. Internet Access",
       x = "Internet Access Rate", y = "Reading Score",
       size = "Population", color = "Poverty Rate")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_digital_divide_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)

# The "Poverty-Achievement Gap" (Scatter with Facets)

ggplot(merged_df, aes(x = poverty_rate, y = score)) +
  geom_point(aes(color = as.factor(post2020)), alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ post2020, labeller = as_labeller(c("0" = "Pre-2020", "1" = "Post-2020"))) +
  theme_minimal() +
  labs(title = "Impact of Poverty on Reading Scores",
       subtitle = "Comparing relationship slope before and after 2020",
       x = "Poverty Rate", y = "NAEP Score")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_poverty_achievement_gap_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)


# Variation in State Reading Scores

ggplot(merged_df, aes(x = as.factor(post2020), y = score, fill = as.factor(post2020))) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) + # This shows the individual states as dots
  theme_minimal() +
  scale_x_discrete(labels = c("Pre-2020", "Post-2020")) +
  labs(title = "Variation in State Reading Scores",
       subtitle = "Did the gap between states widen after the pandemic?",
       x = "Period", y = "Score", fill = "Period")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/variation_state_reading_scores_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)


# The Economic Barrier: Income vs. Internet Access

ggplot(merged_df %>% filter(!is.na(internet_rate)), 
       aes(x = median_income, y = internet_rate)) +
  geom_point(aes(size = total_pop, color = poverty_rate), alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed") +
  scale_color_gradient(low = "green", high = "red") +
  theme_minimal() +
  labs(title = "The Digital Divide: Income vs. Internet",
       subtitle = "Size:Population; Color:Poverty Rate",
       x = "Median Household Income", 
       y = "Internet Access Rate (Broadband)",
       size = "State Population",
       color = "Poverty Rate")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/digital_divide_income_vs_internet.jpg", 
       width = 15, 
       height = 6, 
       dpi = 300)

# The "Poverty Trap" Visualization (Bivariate Analysis)

ggplot(merged_df %>% filter(year == 2022), 
       aes(x = poverty_rate, y = internet_rate)) +
  geom_point(color = "darkred", size = 3) +
  geom_text(aes(label = state), vjust = -1, size = 3, check_overlap = TRUE) +
  geom_smooth(method = "loess", color = "black", alpha = 0.1) +
  theme_light() +
  labs(title = "Poverty Trap:State-Level Digital Exclusion (2022)",
       subtitle = "High poverty correlates with lower digital infrastructure",
       x = "Poverty Rate", 
       y = "Internet Access Rate")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/poverty_trap_state-level_igital_exclusion_(2022).jpg", 
       width = 18, 
       height = 8, 
       dpi = 300)

# High-Res Correlation Heatmap

library(ggcorrplot)

# Calculate correlations for numeric variables
corr_matrix <- cor(merged_df %>% select(score, median_income, poverty_rate, internet_rate, post2020), 
                   use = "complete.obs")

ggcorrplot(corr_matrix, lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix of Educational & Socioeconomic Factors")



ggplot(merged_df, aes(x = score, fill = as.factor(post2020))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Reading Scores",
       subtitle = "Pre-2020 vs. Post-2020",
       x = "NAEP Score", fill = "Period")

ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_distribution_reading_scores_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)



# State-Level Variance "Slopegraph"

# Filter for just the most recent pre-pandemic year and the most recent post-pandemic year
slope_data <- merged_df %>% 
  filter(year %in% c(2019, 2022))

ggplot(slope_data, aes(x = as.factor(year), y = score, group = state)) +
  geom_line(aes(color = score), size = 1) +
  geom_text(data = slope_data %>% filter(year == 2022), 
            aes(label = state), hjust = -0.1, size = 3) +
  theme_classic() +
  labs(title = "State-by-State Score Change (2019 vs 2022)",
       x = "Year", y = "Reading Score") +
  guides(color = "none")


ggsave("D:/PhD Study/Spring 2026/EPPS 6323/naep_poverty_achievement_gap_plot.jpg", 
       width = 10, 
       height = 6, 
       dpi = 300)
