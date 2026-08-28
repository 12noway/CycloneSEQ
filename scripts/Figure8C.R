#!/usr/bin/env Rscript

# mSystems-style grouped heatmap for circular contig counts
# Input:
#   /work/zhangzhe4/circular_summary_strict_v3.tsv
# Output:
#   PDF / PNG / SVG heatmap under /work/zhangzhe4/assembly_circular_heatmap_msystems

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

input_file <- "../表格/Figure 8C.tsv"
outdir <- "./"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

df <- read_tsv(input_file, show_col_types = FALSE)

tool_levels <- c(
  "flye2.8.3", "flye2.9.6", "metamdbg", "myloasm", "raven1.8.3",
  "hybridspades", "opera_ms",
  "flye2.8.3_sr", "flye2.9.6_sr", "metamdbg_sr", "myloasm_sr", "raven_sr"
)

sample_levels <- c("2118", "3068", "4143", "5178", "8366", "8373", "8915", "0934", "0942")

group_levels <- c(
  "Long-read assemblers",
  "Hybrid assemblers",
  "Polished long-read assemblies"
)

plot_df <- df %>%
  mutate(
    Sample = gsub("_$", "", Sample),
    Sample = ifelse(Sample == "934", "0934", Sample),
    Sample = ifelse(Sample == "942", "0942", Sample),
    Sample = ifelse(Sample == "0942", "0942", Sample),
    Tool = factor(Tool, levels = tool_levels),
    Sample = factor(Sample, levels = sample_levels),
    Assembler_group = case_when(
      Tool %in% c("flye2.8.3", "flye2.9.6", "metamdbg", "myloasm", "raven1.8.3") ~ "Long-read assemblers",
      Tool %in% c("hybridspades", "opera_ms") ~ "Hybrid assemblers",
      Tool %in% c("flye2.8.3_sr", "flye2.9.6_sr", "metamdbg_sr", "myloasm_sr", "raven_sr") ~ "Polished long-read assemblies",
      TRUE ~ "Other"
    ),
    Assembler_group = factor(Assembler_group, levels = group_levels)
  ) %>%
  filter(!is.na(Tool), !is.na(Sample), !is.na(Assembler_group)) %>%
  mutate(
    Circular_for_plot = Circular_Contigs + 1,
    Label = ifelse(Circular_Contigs == 0, "", as.character(Circular_Contigs))
  )

annot_df <- plot_df %>%
  group_by(Tool, Assembler_group) %>%
  summarise(
    has_header = any(Header_Marked > 0),
    has_overlap = any(Overlap_Inferred > 0),
    .groups = "drop"
  ) %>%
  mutate(
    Evidence_type = case_when(
      has_header & !has_overlap ~ "Header-supported",
      !has_header & has_overlap ~ "Overlap-inferred",
      has_header & has_overlap ~ "Mixed",
      TRUE ~ "None"
    ),
    Tool = factor(Tool, levels = tool_levels),
    Assembler_group = factor(Assembler_group, levels = group_levels)
  )

write_csv(plot_df, file.path(outdir, "circular_summary_strict_v3_plot_table_grouped_v3.csv"))
write_csv(annot_df, file.path(outdir, "circular_summary_strict_v3_tool_annotation_v3.csv"))

base_theme <- theme_bw(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", size = 8, angle = 0, hjust = 0),
    strip.placement = "outside",
    panel.spacing.y = unit(1.5, "mm"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

p_heat <- ggplot(plot_df, aes(x = Sample, y = Tool, fill = Circular_for_plot)) +
  geom_tile(color = "grey95", linewidth = 0.35) +
  geom_text(aes(label = Label), size = 2.5, color = "grey15") +
  facet_grid(Assembler_group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6"),
    trans = "log10",
    breaks = c(1, 2, 5, 10, 20, 50, 100),
    labels = c("0", "1", "4", "9", "19", "49", "99"),
    name = "Circular contigs"
  ) +
  labs(x = "Sample", y = NULL) +
  base_theme

p_annot <- ggplot(annot_df, aes(x = "Evidence", y = Tool, fill = Evidence_type)) +
  geom_tile(color = "white", linewidth = 0.35) +
  facet_grid(Assembler_group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(
    values = c(
      "Header-supported" = "#C73E1D",
      "Overlap-inferred" = "#2C7FB8",
      "Mixed" = "#7B3294",
      "None" = "#BDBDBD"
    ),
    name = "Evidence type"
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black", angle = 90, vjust = 0.5, hjust = 0.5),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing.y = unit(1.5, "mm"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.margin = margin(5.5, 0, 5.5, 0)
  )

p_final <- p_heat + p_annot +
  plot_layout(widths = c(9.5, 1.3), guides = "collect") &
  theme(legend.position = "right")

ggsave(
  file.path(outdir, "circular_contigs_heatmap_msystems_grouped_v3.png"),
  p_final,
  width = 8.6,
  height = 5.2,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(outdir, "circular_contigs_heatmap_msystems_grouped_v3.pdf"),
  p_final,
  width = 8.6,
  height = 5.2,
  device = cairo_pdf,
  bg = "white"
)

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(
    file.path(outdir, "circular_contigs_heatmap_msystems_grouped_v3.svg"),
    p_final,
    width = 8.6,
    height = 5.2,
    device = svglite::svglite,
    bg = "white"
  )
}

message("Done. Outputs saved to: ", outdir)