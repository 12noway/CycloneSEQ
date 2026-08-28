#!/usr/bin/env Rscript

# ============================================================
# Figure 9B-E refined merged heatmap version
# Ultra-large number version
# 1. Larger tile numbers
# 2. Larger axis labels and titles
# 3. Larger export size
# 4. Slightly simplified numeric display for readability
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

base_dir <- "../表格/"
outdir <- "./"

bc_file <- file.path(base_dir, "Figure 9B-C.csv")
d_file  <- file.path(base_dir, "Figure 9D.tsv")
e_file  <- file.path(base_dir, "Figure 9E.tsv")

workflow_order <- c(
  "flye2_8_3", "flye2_8_3_sr",
  "flye2_9_6", "flye2_9_6_sr",
  "hybridspades",
  "metamdbg", "MetaMDBG_sr",
  "myloasm", "myloasm_sr",
  "opera_ms",
  "raven", "raven_sr"
)

workflow_labels <- c(
  flye2_8_3 = "F2.8.3",
  flye2_8_3_sr = "F2.8.3+SR",
  flye2_9_6 = "F2.9.6",
  flye2_9_6_sr = "F2.9.6+SR",
  hybridspades = "hybridSPAdes",
  metamdbg = "metaMDBG",
  MetaMDBG_sr = "mMDBG+SR",
  myloasm = "myloasm",
  myloasm_sr = "myloasm+SR",
  opera_ms = "OPERA-MS",
  raven = "Raven",
  raven_sr = "Raven+SR"
)

normalize_workflow <- function(x) {
  recode(
    x,
    "flye2.8.3" = "flye2_8_3",
    "flye2.8.3_sr" = "flye2_8_3_sr",
    "flye2.9.6" = "flye2_9_6",
    "flye2.9.6_sr" = "flye2_9_6_sr",
    .default = x
  )
}

pretty_species <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("\\n", " ", x)
  trimws(x)
}

# 为提高可读性，进一步简化数值显示
format_tile_number <- function(x, panel_name) {
  if (is.na(x)) return(NA_character_)
  if (panel_name %in% c("B. Copy-number accuracy", "C. Sequence-length accuracy")) {
    if (x >= 100) return(sprintf("%.0f", x))
    if (x >= 10)  return(sprintf("%.0f", x))
    return(sprintf("%.1f", x))
  }
  if (x >= 1000) return(sprintf("%.0f", x))
  if (x >= 100)  return(sprintf("%.0f", x))
  if (x >= 10)   return(sprintf("%.1f", x))
  return(sprintf("%.2f", x))
}

# -----------------------------
# Load B/C
# -----------------------------
bc <- read_csv(bc_file, show_col_types = FALSE) %>%
  mutate(
    species = pretty_species(Species),
    workflow = normalize_workflow(Tool),
    copy_dev_abs = abs(Copy_number_recovery_percent - 100),
    len_dev_abs  = abs(Length_recovery_percent - 100)
  )

bc_species <- bc %>%
  distinct(species) %>%
  pull(species)

panel_B <- bc %>% transmute(panel = "B. Copy-number accuracy", species, workflow, value = copy_dev_abs)
panel_C <- bc %>% transmute(panel = "C. Sequence-length accuracy", species, workflow, value = len_dev_abs)

# -----------------------------
# Load D/E, keep only B/C species
# -----------------------------
load_wide_panel <- function(path, panel_name) {
  read_tsv(path, show_col_types = FALSE) %>%
    mutate(Assemblies = pretty_species(Assemblies)) %>%
    filter(Assemblies %in% bc_species) %>%
    pivot_longer(-Assemblies, names_to = "workflow", values_to = "value") %>%
    mutate(workflow = normalize_workflow(workflow)) %>%
    transmute(panel = panel_name, species = Assemblies, workflow, value)
}

panel_D <- load_wide_panel(d_file, "D. Indel errors")
panel_E <- load_wide_panel(e_file, "E. Mismatch errors")

plot_df <- bind_rows(panel_B, panel_C, panel_D, panel_E) %>%
  mutate(workflow = factor(workflow, levels = workflow_order)) %>%
  filter(!is.na(workflow), !is.na(value)) %>%
  rowwise() %>%
  mutate(tile_label = format_tile_number(value, panel)) %>%
  ungroup()

species_order <- panel_B %>%
  group_by(species) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_value) %>%
  pull(species)

plot_df$species <- factor(plot_df$species, levels = rev(species_order))

best_df <- plot_df %>%
  group_by(panel, species) %>%
  slice_min(order_by = value, n = 1, with_ties = TRUE) %>%
  ungroup()

common_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = "black", size = 11.5, face = "bold"),
    axis.text.y = element_text(color = "black", size = 10.5, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(4, 4, 4, 4, "mm")
  )

plot_heatmap_panel <- function(df, best_df, panel_name, legend_title,
                               log_scale = FALSE,
                               palette_type = c("blue", "orange"),
                               show_species_labels = TRUE,
                               show_workflow_labels = TRUE) {
  palette_type <- match.arg(palette_type)
  sub_df <- df %>% filter(panel == panel_name)
  sub_best <- best_df %>% filter(panel == panel_name)
  
  p <- ggplot(sub_df, aes(x = workflow, y = species, fill = value)) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(aes(label = tile_label), size = 3.4, color = "black", fontface = "bold") +
    geom_point(
      data = sub_best,
      aes(x = workflow, y = species),
      inherit.aes = FALSE,
      shape = 8,
      size = 2.1,
      stroke = 0.8,
      color = "#D73027",
      position = position_nudge(x = 0.33, y = 0.28)
    ) +
    scale_x_discrete(labels = workflow_labels, drop = FALSE) +
    scale_y_discrete(drop = TRUE) +
    labs(title = panel_name, fill = legend_title) +
    coord_cartesian(clip = "off") +
    common_theme
  
  if (!show_species_labels) {
    p <- p + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  if (!show_workflow_labels) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  
  if (palette_type == "blue") {
    cols <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")
  } else {
    cols <- c("#FFF5EB", "#FDBE85", "#FD8D3C", "#D94701", "#7F2704")
  }
  
  if (log_scale) {
    p <- p + scale_fill_gradientn(colours = cols, trans = "log10", name = legend_title)
  } else {
    p <- p + scale_fill_gradientn(colours = cols, name = legend_title)
  }
  
  p
}

pB <- plot_heatmap_panel(
  plot_df, best_df,
  panel_name = "B. Copy-number accuracy",
  legend_title = "Deviation from 100%\ncopy-number recovery (%)",
  log_scale = FALSE,
  palette_type = "blue",
  show_species_labels = TRUE,
  show_workflow_labels = FALSE
)

pC <- plot_heatmap_panel(
  plot_df, best_df,
  panel_name = "C. Sequence-length accuracy",
  legend_title = "Deviation from 100%\nlength recovery (%)",
  log_scale = FALSE,
  palette_type = "blue",
  show_species_labels = FALSE,
  show_workflow_labels = FALSE
)

pD <- plot_heatmap_panel(
  plot_df, best_df,
  panel_name = "D. Indel errors",
  legend_title = "Indel errors\nper 100 kbp",
  log_scale = TRUE,
  palette_type = "orange",
  show_species_labels = TRUE,
  show_workflow_labels = TRUE
)

pE <- plot_heatmap_panel(
  plot_df, best_df,
  panel_name = "E. Mismatch errors",
  legend_title = "Mismatch errors\nper 100 kbp",
  log_scale = TRUE,
  palette_type = "orange",
  show_species_labels = FALSE,
  show_workflow_labels = TRUE
)

p_all <- (pB | pC) / (pD | pE) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

pdf(file.path(outdir, "Figure7_B_to_E_species_workflow_heatmaps_axisAdjusted_ultraBigNumbers.pdf"), width = 20, height = 12)
print(p_all)
dev.off()

png(file.path(outdir, "Figure7_B_to_E_species_workflow_heatmaps_axisAdjusted_ultraBigNumbers.png"), width = 20, height = 12, units = "in", res = 300)
print(p_all)
dev.off()

message("Done. Ultra-large-number heatmaps written to: ", outdir)