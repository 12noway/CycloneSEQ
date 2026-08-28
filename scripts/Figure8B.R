library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

input_file <- "../表格/Figure 8B.csv"
total_species <- 433

df <- read_csv(input_file, show_col_types = FALSE)
colnames(df) <- c("Workflow", "Recovered")

workflow_labels <- c(
  "flye2_8_3" = "Flye 2.8.3",
  "flye2_8_3_sr" = "Flye 2.8.3 + SR",
  "flye2_9_6" = "Flye 2.9.6",
  "flye2_9_6_sr" = "Flye 2.9.6 + SR",
  "hybridspades" = "hybridSPAdes",
  "metamdbg" = "metaMDBG",
  "metamdbg_sr" = "metaMDBG + SR",
  "myloasm" = "myloasm",
  "myloasm_sr" = "myloasm + SR",
  "opera_ms" = "OPERA-MS",
  "raven1_8_3" = "Raven 1.8.3",
  "raven1_8_3_sr" = "Raven 1.8.3 + SR"
)

df <- df %>%
  mutate(
    Workflow = factor(Workflow, levels = Workflow),
    Not_recovered = total_species - Recovered,
    Label = as.character(Recovered)
  )

df_long <- df %>%
  select(Workflow, Recovered, Not_recovered) %>%
  pivot_longer(
    cols = c(Recovered, Not_recovered),
    names_to = "Category",
    values_to = "Cluster_number"
  ) %>%
  mutate(
    Category = factor(
      Category,
      levels = c("Recovered", "Not_recovered"),
      labels = c("Recovered species-level MAG clusters", "Not recovered")
    )
  )

journal_theme <- theme_classic(base_size = 9.5, base_family = "sans") +
  theme(
    axis.line = element_line(linewidth = 0.35, color = "black"),
    axis.ticks = element_line(linewidth = 0.35, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0, size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, color = "black"),
    legend.key.size = unit(0.35, "cm"),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

p <- ggplot(df_long, aes(x = Workflow, y = Cluster_number, fill = Category)) +
  geom_col(
    width = 0.72,
    color = "black",
    linewidth = 0.22
  ) +
  geom_text(
    data = df,
    aes(x = Workflow, y = Recovered, label = Label),
    inherit.aes = FALSE,
    vjust = -0.35,
    size = 2.6,
    color = "black"
  ) +
  scale_x_discrete(labels = workflow_labels) +
  scale_y_continuous(
    limits = c(0, total_species * 1.08),
    breaks = seq(0, 400, by = 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = c(
      "Recovered species-level MAG clusters" = "#7FB9D6",
      "Not recovered" = "#E6E6E6"
    )
  ) +
  journal_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.2, color = "black"),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9, color = "black"),
    legend.position = "top"
  ) +
  labs(
    y = "Number of species-level MAG clusters",
    title = "A"
  )

print(p)

ggsave(
  filename = "Figure_6B_stacked_species_msystems.tiff",
  plot = p,
  width = 180,
  height = 95,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figure_6B_stacked_species_msystems.pdf",
  plot = p,
  width = 180,
  height = 95,
  units = "mm"
)