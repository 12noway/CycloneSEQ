# ============================================================================
# Contamination Distribution Analysis
# GigaScience / Nature-Cell Publication-ready Density Plot
#
# Optimized for:
# 1. Large and sharp publication-grade typography
# 2. Enhanced readability of numbers/symbols/labels
# 3. Vector PDF export
# 4. Colorblind-friendly scientific palette
# 5. High-resolution TIFF export
# 6. Clear density visualization and median comparison
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,
  tidyr,
  dplyr,
  scales,
  grid,
  cairoDevice
)

# ============================================================================
# 2. Read and preprocess data
# ============================================================================

data <- read.csv("../表格/组装主图_组装污染度密度.csv")

colnames(data) <- c(
  "Flye",
  "MetamDBG",
  "Myloasm",
  "Flye_NextPolish",
  "MetamDBG_NextPolish",
  "Myloasm_NextPolish",
  "HybridSPAdes",
  "OPERA-MS"
)

# Convert to long format
data_long <- data %>%
  
  pivot_longer(
    cols = everything(),
    
    names_to = "Method",
    
    values_to = "Contamination"
  ) %>%
  
  filter(!is.na(Contamination)) %>%
  
  mutate(
    
    Group = factor(
      
      ifelse(
        grepl("NextPolish", Method),
        "After Polish",
        "Original"
      ),
      
      levels = c(
        "Original",
        "After Polish"
      )
    ),
    
    Method_Simple = factor(
      
      gsub("_NextPolish", "", Method),
      
      levels = c(
        "Flye",
        "MetamDBG",
        "Myloasm",
        "HybridSPAdes",
        "OPERA-MS"
      )
    )
  )

# ============================================================================
# 3. Calculate median values
# ============================================================================

median_data <- data_long %>%
  
  group_by(
    Method_Simple,
    Group
  ) %>%
  
  summarise(
    Median = median(
      Contamination,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# ============================================================================
# 4. Scientific publication palette
# Colorblind-friendly + high contrast
# ============================================================================

sci_palette_fill <- c(
  "Original" = "#56B4E9",       # Light blue
  "After Polish" = "#E69F00"   # Orange
)

sci_palette_line <- c(
  "Original" = "#0072B2",       # Deep blue
  "After Polish" = "#D55E00"   # Vermillion
)

# ============================================================================
# 5. Publication-ready theme
# ============================================================================

theme_gigascience <- function() {
  
  theme_classic(base_size = 18) +
    
    theme(
      
      # Text
      text = element_text(
        family = "sans",
        colour = "black"
      ),
      
      # Axis titles
      axis.title = element_text(
        size = 22,
        face = "bold",
        colour = "black"
      ),
      
      # Axis text
      axis.text = element_text(
        size = 17,
        face = "bold",
        colour = "black"
      ),
      
      # Axis lines
      axis.line = element_line(
        colour = "black",
        linewidth = 1.2
      ),
      
      # Axis ticks
      axis.ticks = element_line(
        colour = "black",
        linewidth = 1.1
      ),
      
      axis.ticks.length = unit(
        0.22,
        "cm"
      ),
      
      # Facet labels
      strip.background = element_blank(),
      
      strip.text = element_text(
        size = 18,
        face = "bold",
        colour = "black",
        margin = margin(
          t = 6,
          b = 6
        )
      ),
      
      # Legend
      legend.position = "right",
      
      legend.title = element_text(
        size = 18,
        face = "bold",
        colour = "black"
      ),
      
      legend.text = element_text(
        size = 16,
        face = "bold",
        colour = "black"
      ),
      
      legend.key.size = unit(
        0.8,
        "cm"
      ),
      
      legend.background = element_blank(),
      
      # Panel spacing
      panel.spacing = unit(
        1.4,
        "lines"
      ),
      
      # Margins
      plot.margin = margin(
        t = 18,
        r = 18,
        b = 18,
        l = 18
      ),
      
      # Background
      plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

# ============================================================================
# 6. Main density plot
# ============================================================================

density_plot <- ggplot(
  data_long,
  
  aes(
    x = Contamination,
    fill = Group,
    colour = Group
  )
) +
  
  # --------------------------------------------------------------------------
# Density curves
# --------------------------------------------------------------------------

geom_density(
  
  alpha = 0.50,
  
  linewidth = 1.5,
  
  adjust = 1.15,
  
  trim = TRUE
) +
  
  # --------------------------------------------------------------------------
# Median dashed lines
# --------------------------------------------------------------------------

geom_vline(
  
  data = median_data,
  
  aes(
    xintercept = Median,
    colour = Group
  ),
  
  linetype = "dashed",
  
  linewidth = 1.3,
  
  alpha = 0.95
) +
  
  # --------------------------------------------------------------------------
# Facet layout
# --------------------------------------------------------------------------

facet_wrap(
  ~ Method_Simple,
  
  ncol = 3,
  
  scales = "fixed"
) +
  
  # --------------------------------------------------------------------------
# Labels
# --------------------------------------------------------------------------

labs(
  
  x = "Contamination Level (%)",
  
  y = "Density",
  
  fill = "Stage",
  
  colour = "Stage"
) +
  
  # --------------------------------------------------------------------------
# Color scales
# --------------------------------------------------------------------------

scale_fill_manual(
  values = sci_palette_fill
) +
  
  scale_color_manual(
    values = sci_palette_line
  ) +
  
  # --------------------------------------------------------------------------
# X-axis
# --------------------------------------------------------------------------

scale_x_continuous(
  
  limits = c(0, 5),
  
  breaks = seq(0, 5, 1),
  
  expand = expansion(mult = c(0.02, 0.02))
) +
  
  # --------------------------------------------------------------------------
# Y-axis
# --------------------------------------------------------------------------

scale_y_continuous(
  
  expand = expansion(mult = c(0, 0.08))
) +
  
  # --------------------------------------------------------------------------
# Theme
# --------------------------------------------------------------------------

theme_gigascience()

# ============================================================================
# 7. Export publication-ready PDF
# Vector graphics for journal submission
# ============================================================================

ggsave(
  
  filename = "Contamination_Density_GigaScience.pdf",
  
  plot = density_plot,
  
  device = cairo_pdf,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# ============================================================================
# 8. Export TIFF version
# ============================================================================

ggsave(
  
  filename = "Contamination_Density_GigaScience.tiff",
  
  plot = density_plot,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
)

# ============================================================================
# 9. Export PNG preview
# ============================================================================

ggsave(
  
  filename = "Contamination_Density_GigaScience.png",
  
  plot = density_plot,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 10. Grayscale version
# ============================================================================

gray_fill <- c(
  "Original" = "#B3B3B3",
  "After Polish" = "#666666"
)

gray_line <- c(
  "Original" = "#4D4D4D",
  "After Polish" = "#1A1A1A"
)

density_plot_gray <- ggplot(
  data_long,
  
  aes(
    x = Contamination,
    fill = Group,
    colour = Group
  )
) +
  
  geom_density(
    alpha = 0.50,
    linewidth = 1.5,
    adjust = 1.15,
    trim = TRUE
  ) +
  
  geom_vline(
    data = median_data,
    
    aes(
      xintercept = Median,
      colour = Group
    ),
    
    linetype = "dashed",
    
    linewidth = 1.3
  ) +
  
  facet_wrap(
    ~ Method_Simple,
    
    ncol = 3,
    
    scales = "fixed"
  ) +
  
  labs(
    x = "Contamination Level (%)",
    y = "Density",
    fill = "Stage",
    colour = "Stage"
  ) +
  
  scale_fill_manual(
    values = gray_fill
  ) +
  
  scale_color_manual(
    values = gray_line
  ) +
  
  scale_x_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  theme_gigascience()

ggsave(
  
  filename = "Contamination_Density_GigaScience_Gray.pdf",
  
  plot = density_plot_gray,
  
  device = cairo_pdf,
  
  width = 12,
  
  height = 8.5,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# ============================================================================
# 11. Terminal output
# ============================================================================

cat("\n")
cat(paste(rep("=", 82), collapse = ""))
cat("\n")

cat("Publication-ready contamination density plot generated successfully!\n\n")

cat("Generated files:\n")
cat("1. Contamination_Density_GigaScience.pdf\n")
cat("2. Contamination_Density_GigaScience.tiff\n")
cat("3. Contamination_Density_GigaScience.png\n")
cat("4. Contamination_Density_GigaScience_Gray.pdf\n\n")

cat("GigaScience optimizations:\n")
cat("- Vector PDF export for journal submission\n")
cat("- Enlarged publication-grade typography\n")
cat("- Thickened density curves and median lines\n")
cat("- Improved readability of labels and symbols\n")
cat("- Colorblind-friendly scientific palette\n")
cat("- High-resolution TIFF included\n")
cat("- Grayscale version included\n")
cat("- Enhanced panel spacing and contrast\n")
cat("- Nature/Cell-style clean visualization\n\n")

cat("Recommended submission file:\n")
cat("-> Contamination_Density_GigaScience.pdf\n\n")

cat(paste(rep("=", 82), collapse = ""))
cat("\n")

# ============================================================================
# 12. Display plot
# ============================================================================

print(density_plot)