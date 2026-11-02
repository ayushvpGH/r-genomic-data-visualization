# =====================================================================
# Project: Genomic Expression Analysis & Advanced R Visualization
# Course: BIO 579: Data Analysis and Visualization in R
# Author: Ayush Patel
# =====================================================================

# 1. Load Required Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(viridis)
})

# 2. Simulate / Load Gene Expression & Transcriptomic Dataset
set.seed(42)
n_genes <- 1500

gene_data <- tibble(
  Gene_ID = paste0("GENE_", 1:n_genes),
  Condition = rep(c("Control", "Treatment_A", "Treatment_B"), each = n_genes / 3),
  Base_Mean = rnorm(n_genes, mean = 50, sd = 15),
  Log2_Fold_Change = rnorm(n_genes, mean = 0, sd = 2.5),
  p_value = runif(n_genes, min = 0, max = 1)
) %>%
  mutate(
    Expression_Level = Base_Mean * 2^Log2_Fold_Change,
    Significant = ifelse(p_value < 0.05 & abs(Log2_Fold_Change) > 1.5, "Significant", "Not Significant")
  )

# 3. Data Wrangling with Tidyverse
summary_stats <- gene_data %>%
  group_by(Condition) %>%
  summarize(
    Mean_Expression = mean(Expression_Level, na.rm = TRUE),
    Median_Expression = median(Expression_Level, na.rm = TRUE),
    Sig_Count = sum(Significant == "Significant")
  )

print("--- Transcriptomic Summary Statistics ---")
print(summary_stats)

# 4. Publication-Grade Visualization (ggplot2 Volcano / Expression Plot)
ggplot(gene_data, aes(x = Log2_Fold_Change, y = -log10(p_value), color = Significant)) +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c("Not Significant" = "grey70", "Significant" = "#2b5c8f")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Differential Gene Expression Analysis",
    subtitle = "BIO 579 Transcriptomic Visualization Pipeline",
    x = "Log2 Fold Change",
    y = "-Log10 p-value",
    color = "Expression Status"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
