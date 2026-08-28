library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

file_path <- "../表格/Figure 9A.xlsx"

df <- read_excel(file_path, skip = 1)

df$Species <- gsub("\n", " ", df$Species)
df$Species <- gsub("\\s+", " ", df$Species)
df$Species <- trimws(df$Species)

assemblers <- c(
  "flye2.8.3",
  "flye2.8.3_sr",
  "flye2.9.6",
  "flye2.9.6_sr",
  "hybridspades",
  "metamdbg",
  "MetaMDBG_sr",
  "myloasm",
  "myloasm_sr",
  "opera_ms",
  "raven",
  "raven_sr"
)

assembler_labels <- c(
  "flye2.8.3" = "Flye 2.8.3",
  "flye2.8.3_sr" = "Flye 2.8.3 + SR",
  "flye2.9.6" = "Flye 2.9.6",
  "flye2.9.6_sr" = "Flye 2.9.6 + SR",
  "hybridspades" = "hybridSPAdes",
  "metamdbg" = "metaMDBG",
  "MetaMDBG_sr" = "metaMDBG + SR",
  "myloasm" = "myloasm",
  "myloasm_sr" = "myloasm + SR",
  "opera_ms" = "OPERA-MS",
  "raven" = "Raven",
  "raven_sr" = "Raven + SR"
)

df <- df %>%
  arrange(desc(`Genomic DNA(%)`)) %>%
  mutate(
    Species = factor(Species, levels = rev(Species)),
    Abundance_Label = as.character(`Genomic DNA(%)`)
  )

df_long <- df %>%
  select(Species, all_of(assemblers)) %>%
  pivot_longer(
    cols = all_of(assemblers),
    names_to = "Assembler",
    values_to = "Genome_Fraction"
  ) %>%
  mutate(
    Assembler = factor(Assembler, levels = assemblers),
    Label = as.character(Genome_Fraction)
  )

max_abundance <- max(df$`Genomic DNA(%)`, na.rm = TRUE)

journal_theme <- theme_classic(base_size = 9.5, base_family = "sans") +
  theme(
    axis.line = element_line(linewidth = 0.35, color = "black"),
    axis.ticks = element_line(linewidth = 0.35, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0, size = 12, color = "black"),
    plot.margin = margin(3, 3, 3, 3)
  )

p_bar <- ggplot(df, aes(x = `Genomic DNA(%)`, y = Species)) +
  geom_col(
    fill = "#D0D0D0",
    color = "black",
    linewidth = 0.22,
    width = 0.72
  ) +
  geom_text(
    aes(label = Abundance_Label),
    hjust = 1.12,
    size = 2.55,
    color = "black"
  ) +
  scale_x_reverse(
    limits = c(max_abundance * 1.18, 0),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  journal_theme +
  theme(
    axis.text.y = element_text(face = "italic", size = 8.2, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 8.8, color = "black"),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  ) +
  labs(
    x = "Genomic DNA (%)",
    title = "A"
  )

p_heat <- ggplot(df_long, aes(x = Assembler, y = Species, fill = Genome_Fraction)) +
  geom_tile(
    color = "white",
    linewidth = 0.30
  ) +
  geom_text(
    aes(label = Label),
    size = 1.9,
    color = "black"
  ) +
  scale_x_discrete(labels = assembler_labels) +
  scale_fill_gradientn(
    colours = c("#FFFFFF", "#F0F7FB", "#D8EBF3", "#B8D8E8", "#7FB9D6"),
    values = scales::rescale(c(0, 25, 50, 75, 100)),
    name = "Genome\nfraction (%)",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  journal_theme +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5, color = "black"),
    axis.title.x = element_text(size = 8.8, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.35),
    legend.position = "right",
    legend.title = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 7.5, color = "black"),
    legend.key.width = unit(0.35, "cm"),
    legend.key.height = unit(0.40, "cm"),
    plot.margin = margin(19, 5.5, 5.5, 1)
  ) +
  labs(
    x = "Assembly and polishing workflow",
    title = NULL
  )

combined_plot <- p_bar + p_heat + plot_layout(widths = c(1.25, 4.9))

print(combined_plot)

ggsave(
  filename = "mock_validation_abundance_heatmap_msystems.tiff",
  plot = combined_plot,
  width = 180,
  height = 105,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "mock_validation_abundance_heatmap_msystems.pdf",
  plot = combined_plot,
  width = 180,
  height = 105,
  units = "mm"
)
