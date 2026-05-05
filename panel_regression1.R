# ----------------------------
# Full Panel Regression Model
# ----------------------------

required_pkgs <- c(
  "tidyverse", "plm", "lmtest", "sandwich", "broom", "margins"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

# Project-relative paths
project_root <- getwd()
input_path <- file.path(project_root, "dataset.csv")
output_dir <- file.path(project_root, "output", "panel_regression")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load data
df <- read.csv(input_path)
# ----------------------------
# SPLIT BY GRADE
# ----------------------------
df4 <- df %>% filter(grade == 4)
df8 <- df %>% filter(grade == 8)

# ----------------------------
# SET UP PANEL STRUCTURE
# ----------------------------
panel4 <- pdata.frame(df4, index = c("state", "year"))
panel8 <- pdata.frame(df8, index = c("state", "year"))

# ----------------------------
# MODEL FORMULA DEFINITIONS
# ----------------------------
f_ols <- score ~ post2020 + child_poverty_rate + median_income + 
  internet_rate + unemployment_rate + 
  bach_or_higher_rate + pct_black + pct_hispanic

f_fe  <- score ~ post2020 + child_poverty_rate + median_income + 
  internet_rate + unemployment_rate + 
  bach_or_higher_rate + pct_black + pct_hispanic

f_int <- score ~ post2020 * child_poverty_rate + 
  post2020 * median_income +
  post2020 * internet_rate +
  unemployment_rate + bach_or_higher_rate + 
  pct_black + pct_hispanic

# ----------------------------
# GRADE 4 MODELS
# ----------------------------
m1_g4 <- lm(f_ols, data = df4)

m2_g4 <- plm(f_fe, data = panel4, model = "within", effect = "twoways")

m3_g4 <- plm(f_int, data = panel4, model = "within", effect = "twoways")

# Cluster-robust SEs at state level
se_m2_g4 <- coeftest(m2_g4, vcov = vcovHC(m2_g4, type = "HC1", cluster = "group"))
se_m3_g4 <- coeftest(m3_g4, vcov = vcovHC(m3_g4, type = "HC1", cluster = "group"))

# ----------------------------
# GRADE 8 MODELS
# ----------------------------
m1_g8 <- lm(f_ols, data = df8)

m2_g8 <- plm(f_fe, data = panel8, model = "within", effect = "twoways")

m3_g8 <- plm(f_int, data = panel8, model = "within", effect = "twoways")

se_m2_g8 <- coeftest(m2_g8, vcov = vcovHC(m2_g8, type = "HC1", cluster = "group"))
se_m3_g8 <- coeftest(m3_g8, vcov = vcovHC(m3_g8, type = "HC1", cluster = "group"))

# ----------------------------
# Fixed Effects
# ----------------------------
re_g4 <- plm(f_fe, data = panel4, model = "random", 
             effect = "twoways", random.method = "walhus")

re_g8 <- plm(f_fe, data = panel8, model = "random", 
             effect = "twoways", random.method = "walhus")

phtest(m2_g4, re_g4)
phtest(m2_g8, re_g8)

# Extract robust SEs for Grade 4
se_m2_g4_vec <- se_m2_g4[, 2]
se_m3_g4_vec <- se_m3_g4[, 2]

# Extract robust SEs for Grade 8
se_m2_g8_vec <- se_m2_g8[, 2]
se_m3_g8_vec <- se_m3_g8[, 2]

# Keep script output visual-only (PNG files).
library(broom)

# ─────────────────────────────────────────
# PLOT 1: REGRESSION COEFFICIENT PLOTS
# ─────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(broom)
library(margins)

# ─────────────────────────────────────────
# PLOT 1: COEFFICIENT PLOT
# ─────────────────────────────────────────

# OLS helper
tidy_ols <- function(model, model_name, grade) {
  t1 <- broom::tidy(model)
  data.frame(
    term      = t1$term,
    estimate  = t1$estimate,
    std.error = t1$std.error,
    conf.low  = t1$estimate - 1.96 * t1$std.error,
    conf.high = t1$estimate + 1.96 * t1$std.error,
    model     = model_name,
    grade     = grade,
    stringsAsFactors = FALSE
  )
}

# FE helper with robust SEs
tidy_model <- function(model, se_robust, model_name, grade) {
  data.frame(
    term      = rownames(se_robust),
    estimate  = se_robust[, 1],
    std.error = se_robust[, 2],
    conf.low  = se_robust[, 1] - 1.96 * se_robust[, 2],
    conf.high = se_robust[, 1] + 1.96 * se_robust[, 2],
    model     = model_name,
    grade     = grade,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

coef_g4 <- bind_rows(
  tidy_ols(m1_g4, "Model 1 (OLS)", "Grade 4"),
  tidy_model(m2_g4, se_m2_g4, "Model 2 (FE)", "Grade 4"),
  tidy_model(m3_g4, se_m3_g4, "Model 3 (FE + Interactions)", "Grade 4")
)

coef_g8 <- bind_rows(
  tidy_ols(m1_g8, "Model 1 (OLS)", "Grade 8"),
  tidy_model(m2_g8, se_m2_g8, "Model 2 (FE)", "Grade 8"),
  tidy_model(m3_g8, se_m3_g8, "Model 3 (FE + Interactions)", "Grade 8")
)

coef_all <- bind_rows(coef_g4, coef_g8) %>%
  filter(!term %in% c("(Intercept)")) %>%
  mutate(term = recode(term,
                       "post2020"                    = "Post-2020",
                       "child_poverty_rate"          = "Child Poverty Rate",
                       "median_income"               = "Median Income",
                       "internet_rate"               = "Internet Access Rate",
                       "unemployment_rate"           = "Unemployment Rate",
                       "bach_or_higher_rate"         = "Bachelor's or Higher Rate",
                       "pct_black"                   = "% Black",
                       "pct_hispanic"                = "% Hispanic",
                       "post2020:child_poverty_rate" = "Post-2020 × Child Poverty",
                       "post2020:median_income"      = "Post-2020 × Median Income",
                       "post2020:internet_rate"      = "Post-2020 × Internet Rate"
  ))

plot1 <- ggplot(coef_all, aes(x = estimate, y = term, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6), size = 0.5) +
  facet_wrap(~ grade, scales = "free_x") +
  scale_color_manual(values = c("#2C3E50", "#E74C3C", "#27AE60")) +
  labs(
    title    = "Regression Coefficient Plot",
    subtitle = "Point estimates with 95% confidence intervals",
    x        = "Coefficient Estimate",
    y        = NULL,
    color    = "Model", shape = "Model",
    caption  = "Note: Robust standard errors clustered at state level."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#2C3E50"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(face = "bold")
  )

ggsave(file.path(output_dir, "plot1_coefplot.png"),
       plot1, width = 12, height = 8, dpi = 300)

# ─────────────────────────────────────────
# PLOT 2: PREDICTED VS ACTUAL SCORES
# ─────────────────────────────────────────

# Add fitted values from FE models
# Add fitted values from FE models
df4_model <- df4 %>%
  filter(!is.na(score), !is.na(median_income), !is.na(poverty_rate),
         !is.na(internet_rate), !is.na(unemployment_rate),
         !is.na(bach_or_higher_rate), !is.na(pct_black), !is.na(pct_hispanic),
         !is.na(child_poverty_rate)) %>%
  mutate(
    fitted_m2 = as.numeric(fitted(m2_g4)),
    period    = ifelse(post2020 == 1, "Post-2020", "Pre-2020")
  )

df8_model <- df8 %>%
  filter(!is.na(score), !is.na(median_income), !is.na(poverty_rate),
         !is.na(internet_rate), !is.na(unemployment_rate),
         !is.na(bach_or_higher_rate), !is.na(pct_black), !is.na(pct_hispanic),
         !is.na(child_poverty_rate)) %>%
  mutate(
    fitted_m2 = as.numeric(fitted(m2_g8)),
    period    = ifelse(post2020 == 1, "Post-2020", "Pre-2020")
  )

# Plot 2
pred_df <- bind_rows(
  df4_model %>% select(state, year, score, fitted_m2, period) %>% mutate(grade = "Grade 4"),
  df8_model %>% select(state, year, score, fitted_m2, period) %>% mutate(grade = "Grade 8")
)

plot2 <- ggplot(pred_df, aes(x = fitted_m2, y = score, color = period)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~ grade, scales = "free") +
  scale_color_manual(values = c("Post-2020" = "#E74C3C", "Pre-2020" = "#2980B9")) +
  labs(
    title    = "Figure 2. Predicted vs. Actual NAEP Reading Scores",
    subtitle = "Two-way fixed effects model (Model 2); dashed line = perfect fit",
    x        = "Fitted Values",
    y        = "Actual Score",
    color    = "Period",
    caption  = "Note: Each point represents a state-year observation."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#2C3E50"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(face = "bold")
  )

ggsave(file.path(output_dir, "plot2_predicted_actual.png"),
       plot2, width = 10, height = 6, dpi = 300)

# ─────────────────────────────────────────
# PLOT 3: INTERACTION EFFECT PLOT (GRADE 8)
# ─────────────────────────────────────────

# Marginal effect of Post-2020 across median income values
income_range <- seq(min(df8$median_income, na.rm = TRUE),
                    max(df8$median_income, na.rm = TRUE),
                    length.out = 100)

# Manual marginal effect for Grade 8 interaction
b_int    <- coef(m3_g8)["post2020:median_income"]
se_int   <- se_m3_g8["post2020:median_income", 2]

# Since post2020 main effect is absorbed by year FE,
# we plot just the interaction component
marginal_df <- data.frame(
  median_income   = income_range,
  marginal_effect = b_int * income_range
) %>%
  mutate(
    se_marginal = abs(income_range) * se_int,
    conf_low    = marginal_effect - 1.96 * se_marginal,
    conf_high   = marginal_effect + 1.96 * se_marginal,
    income_usd  = median_income * 1000
  )

plot3 <- ggplot(marginal_df, aes(x = income_usd / 1000, y = marginal_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), fill = "#AED6F1", alpha = 0.5) +
  geom_line(color = "#2C3E50", linewidth = 1) +
  labs(
    title    = "Figure 3. Marginal Effect of Post-2020 on Grade 8 Reading Scores",
    subtitle = "Across the distribution of state median household income",
    x        = "Median Household Income ($ thousands)",
    y        = "Marginal Effect of Post-2020",
    caption  = "Note: Shaded band = 95% confidence interval (delta method). Model 3, Grade 8."
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "plot3_interaction.png"),
       plot3, width = 9, height = 6, dpi = 300)

# ─────────────────────────────────────────
# PLOT 4: WITHIN-STATE SCORE TRENDS
# ─────────────────────────────────────────

# Select a representative set of states across poverty levels
selected_states <- c("TEXAS", "CALIFORNIA", "MISSISSIPPI",
                     "MASSACHUSETTS", "LOUISIANA", "COLORADO",
                     "NEW YORK", "ALABAMA", "MINNESOTA", "VIRGINIA")

# Plot 4
trend_df <- bind_rows(
  df4_model %>% mutate(grade = "Grade 4"),
  df8_model %>% mutate(grade = "Grade 8")
) %>%
  filter(state %in% selected_states)

plot4 <- ggplot(trend_df, aes(x = year, y = score, color = state, group = state)) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  geom_point(size = 2) +
  annotate("text", x = 2020.3, y = Inf, label = "COVID-19",
           hjust = 0, vjust = 1.5, size = 3, color = "gray40") +
  facet_wrap(~ grade, scales = "free_y") +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2022, 2024)) +
  labs(
    title    = "Figure 4. Within-State NAEP Reading Score Trends (Selected States)",
    subtitle = "Dashed vertical line marks the COVID-19 disruption period",
    x        = "Year",
    y        = "NAEP Reading Score",
    color    = "State",
    caption  = "Note: States selected to represent diversity across poverty and income levels."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#2C3E50"),
    strip.text       = element_text(color = "white", face = "bold"),
    plot.title       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "plot4_trends.png"),
       plot4, width = 12, height = 7, dpi = 300)

cat("All 4 plots saved.\n")
