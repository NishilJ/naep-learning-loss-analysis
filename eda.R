# -----------------------------
# EDA — feature correlation matrix
# Run data.R first to build final_dataset.csv
# Run data.R first to build dataset.csv
# -----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

if (!file.exists("dataset.csv")) {
  stop("dataset.csv not found. Run data.R first.", call. = FALSE)
}

df <- read.csv("dataset.csv", stringsAsFactors = FALSE, check.names = FALSE)

feat <- df %>%
  select(where(is.numeric))

if (ncol(feat) < 2) {
  stop("Need at least 2 numeric columns for a correlation matrix.", call. = FALSE)
}

corr <- cor(feat, use = "pairwise.complete.obs")

cat("Feature correlation matrix (Pearson):\n\n")
print(round(corr, 3))

corr_long <- as.data.frame(corr) %>%
  mutate(var1 = rownames(corr)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  mutate(label = sprintf("%.2f", r))

p <- ggplot(corr_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 2.8, color = "gray10") +
  scale_fill_gradient2(
    limits = c(-1, 1),
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "r"
  ) +
  labs(
    title = "Feature correlation matrix",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("eda_corr_heatmap.png", p, width = 7, height = 6, dpi = 150)
cat("\nSaved eda_corr_heatmap.png\n")
