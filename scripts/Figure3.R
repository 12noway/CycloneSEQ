# ==========================================
# Figure 1: DNA quality metrics of short-read and long-read samples
# Style follows 提取主图.R
# X/Y labels are in English
# Special note: 0934-both is a single sample representing the combined long-read and
# short-read metrics, and is highlighted as a third category: Both using color only
#
# Statistical test used:
# Paired Wilcoxon signed-rank test
# For each metric, CP and CZ are compared as paired observations within each sample
# ==========================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, readxl, cowplot, grDevices)

dir.create("output", showWarnings = FALSE)

get_sig <- function(p) {
  if (is.na(p)) return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
}

save_fig <- function(plot_obj, filename_base, width, height, dpi = 300) {
  ggsave(
    paste0(filename_base, ".pdf"),
    plot_obj,
    width = width,
    height = height,
    device = cairo_pdf
  )
  
  ggsave(
    paste0(filename_base, ".tiff"),
    plot_obj,
    width = width,
    height = height,
    device = "tiff",
    dpi = dpi,
    compression = "lzw",
    type = "cairo",
    bg = "white"
  )
  
  ggsave(
    paste0(filename_base, ".png"),
    plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

# ==========================================
# 1. Read data
# ==========================================

xlsx_path <- if (file.exists("Figure3-5.xlsx")) {
  "Figure3-5.xlsx"
} else if (file.exists("../表格/Figure3-5.xlsx")) {
  "../表格/Figure3-5.xlsx"
} else {
  stop("Cannot find Figure3-5.xlsx in current directory or ../表格/")
}

raw <- read_excel(xlsx_path, sheet = "figure3")

# Explicit sample groups based on sample suffix
# 0934-both is kept as a single row and labeled as Both
raw_dna <- raw %>%
  mutate(
    TypeGroup = case_when(
      str_detect(sample, "-both$") ~ "Both",
      str_detect(sample, "long-read") ~ "Long-read",
      TRUE ~ "Short-read"
    ),
    ReadType = TypeGroup,
    SampleID = str_remove(sample, "-short-read|-long-read|-both"),
    PairID = sample
  ) %>%
  select(
    PairID, SampleID, ReadType, TypeGroup,
    CP_Conc = `cp_concentration(ng/uL)`,
    CZ_Conc = `cz_concentration(ng/uL)`,
    CP_Yield = `cp_unit_yield(ug/100ul)`,
    CZ_Yield = `cz_unit_yield(ug/100ul)`,
    CP_A260_280 = `cp_A260/A280`,
    CZ_A260_280 = `cz_A260/A280`,
    CP_A260_230 = `cp_A260/A230`,
    CZ_A260_230 = `cz_A260/A230`,
    CP_Frag = `cp_Fragment Length(kb)`,
    CZ_Frag = `cz_Fragment Length(kb)`
  ) %>%
  mutate(
    across(
      -c(PairID, SampleID, ReadType, TypeGroup),
      ~ suppressWarnings(as.numeric(.))
    )
  ) %>%
  pivot_longer(
    cols = -c(PairID, SampleID, ReadType, TypeGroup),
    names_to = "Key",
    values_to = "Value"
  ) %>%
  separate(
    Key,
    into = c("Method", "Param"),
    sep = "_",
    extra = "merge"
  ) %>%
  mutate(
    Method = factor(Method, levels = c("CP", "CZ")),
    ReadType = factor(ReadType, levels = c("Short-read", "Long-read", "Both")),
    TypeGroup = factor(TypeGroup, levels = c("Short-read", "Long-read", "Both"))
  ) %>%
  drop_na(Value)

# ==========================================
# 2. Paired Wilcoxon test
# ==========================================

paired_wilcox <- function(df) {
  wide <- df %>%
    select(PairID, Method, Value) %>%
    pivot_wider(names_from = Method, values_from = Value) %>%
    drop_na(CP, CZ)
  
  if (nrow(wide) < 2) return(NA)
  
  tryCatch(
    wilcox.test(wide$CP, wide$CZ, paired = TRUE)$p.value,
    error = function(e) NA
  )
}

# ==========================================
# 3. Common theme
# ==========================================

base_theme <- theme_pubr() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    axis.title = element_text(face = "bold", size = 12, family = "sans"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
    axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
    plot.margin = margin(8, 8, 8, 8)
  )

# ==========================================
# 4. Plot function
# ==========================================

plot_metric <- function(p_name, y_lab, tag_lab, ref_lines = NULL) {
  df <- raw_dna %>% filter(Param == p_name)
  
  p_value <- paired_wilcox(df)
  sig_label <- get_sig(p_value)
  
  y_max <- max(df$Value, na.rm = TRUE)
  y_min <- min(df$Value, na.rm = TRUE)
  y_rng <- ifelse(y_max == y_min, y_max, y_max - y_min)
  y_sig <- y_max + 0.12 * y_rng
  
  p <- ggplot(df, aes(x = Method, y = Value)) +
    geom_boxplot(
      aes(fill = Method),
      width = 0.45,
      color = "black",
      alpha = 0.55,
      outlier.shape = NA,
      linewidth = 0.8
    ) +
    geom_point(
      aes(color = TypeGroup),
      position = position_jitter(width = 0.12, height = 0),
      size = 2.8,
      stroke = 0.9,
      alpha = 0.95
    ) +
    scale_fill_manual(
      values = c("CP" = "#08519C", "CZ" = "#EF6548")
    ) +
    scale_color_manual(
      values = c(
        "Short-read" = "#08519C",
        "Long-read" = "#D73027",
        "Both" = "#7A3DB8"
      )
    ) +
    labs(
      x = NULL,
      y = y_lab,
      tag = tag_lab,
      color = "Sample group"
    ) +
    base_theme +
    annotate(
      "segment",
      x = 1,
      xend = 2,
      y = y_sig,
      yend = y_sig,
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = 1.5,
      y = y_sig + 0.05 * y_rng,
      label = sig_label,
      size = 5
    )
  
  if (!is.null(ref_lines)) {
    for (line in ref_lines) {
      p <- p +
        annotate(
          "segment",
          x = 0.5,
          xend = 2.5,
          y = line,
          yend = line,
          linetype = "dotted",
          color = "grey40"
        )
    }
  }
  
  return(p)
}

# ==========================================
# 5. Generate subplots
# ==========================================

p_a <- plot_metric(
  p_name = "Conc",
  y_lab = "Concentration (ng/µL)",
  tag_lab = "A"
)

p_b <- plot_metric(
  p_name = "Yield",
  y_lab = "Yield (µg)",
  tag_lab = "B"
)

p_c <- plot_metric(
  p_name = "A260_280",
  y_lab = "A260/A280",
  tag_lab = "C",
  ref_lines = c(1.7, 2.0)
)

p_d <- plot_metric(
  p_name = "A260_230",
  y_lab = "A260/A230",
  tag_lab = "D",
  ref_lines = c(2.0)
)

p_e <- plot_metric(
  p_name = "Frag",
  y_lab = "Fragment Length (kb)",
  tag_lab = "E"
)

# ==========================================
# 6. Extract legend and remove it from subplots
# ==========================================

legend_plot <- cowplot::get_legend(
  p_a + theme(legend.position = "right")
)

p_a <- p_a + theme(legend.position = "none")
p_b <- p_b + theme(legend.position = "none")
p_c <- p_c + theme(legend.position = "none")
p_d <- p_d + theme(legend.position = "none")
p_e <- p_e + theme(legend.position = "none")

p_blank <- ggplot() + theme_void()

# ==========================================
# 7. Build a strict 3 x 2 grid first
#    A | B | C
#    D | E | blank
# ==========================================

grid_panels <- cowplot::plot_grid(
  p_a, p_b, p_c,
  p_d, p_e, p_blank,
  ncol = 3,
  nrow = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1, 1),
  rel_heights = c(1, 1)
)

# ==========================================
# 8. Put legend outside the whole grid
# ==========================================

fig1 <- cowplot::plot_grid(
  grid_panels, legend_plot,
  nrow = 1,
  rel_widths = c(1, 0.18),
  align = "h",
  axis = "tb"
)

# ==========================================
# 9. Save figure
# ==========================================

save_fig(
  fig1,
  "output/Figure3_DNA_Quality_Long_Short",
  width = 15,
  height = 8
)

cat("Done: output/Figure3_DNA_Quality_Long_Short.pdf/.tiff/.png\n")