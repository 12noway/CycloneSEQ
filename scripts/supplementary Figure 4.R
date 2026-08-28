# 1. Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(grDevices)

# 2. Read and preprocess data
data <- read.csv("../表格/supplementary Figure 4.csv")

data_long <- data %>%
  mutate(sample = as.character(sample)) %>%
  pivot_longer(
    cols = c("without_pretreatment_shannon", "with_pretreatment_shannon"),
    names_to = "Group",
    values_to = "Shannon"
  ) %>%
  mutate(
    Group = factor(
      Group,
      levels = c("without_pretreatment_shannon", "with_pretreatment_shannon"),
      labels = c("Without Pretreatment", "Pretreatment")
    )
  )

# 3. Define colors
colors_border <- c("Without Pretreatment" = "#2166AC", "Pretreatment" = "#B2182B")
colors_fill   <- c("Without Pretreatment" = "#4393C3", "Pretreatment" = "#D6604D")

# 4. Plot
# Key fix: use geom_point() instead of geom_jitter() so that points stay aligned with paired lines
p <- ggplot(data_long, aes(x = Group, y = Shannon, fill = Group, color = Group)) +
  geom_line(aes(group = sample), color = "grey88", linewidth = 0.5) +
  geom_boxplot(
    width = 0.4,
    linewidth = 1,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 3,
    stroke = 0.8,
    alpha = 0.85,
    color = "black"
  ) +
  stat_compare_means(
    comparisons = list(c("Without Pretreatment", "Pretreatment")),
    method = "wilcox.test",
    paired = TRUE,
    exact = FALSE,
    label = "p.signif",
    label.y = max(data_long$Shannon, na.rm = TRUE) * 1.1,
    bracket.size = 0.8,
    tip.length = 0.02,
    color = "black"
  ) +
  scale_color_manual(values = colors_border) +
  scale_fill_manual(values = colors_fill) +
  labs(x = NULL, y = "sourmash Shannon Index") +
  theme_classic() +
  theme(
    text = element_text(family = "Arial"),
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.ticks = element_line(linewidth = 0.8, color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 10)),
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(p)

ggsave(
  "Shannon_Index_Comparison_fixed.pdf",
  plot = p,
  device = cairo_pdf,
  width = 4.5,
  height = 5.5,
  units = "in"
)

