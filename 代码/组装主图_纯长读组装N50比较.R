# ============================================================================
# N50 Comparison Plot for GigaScience
# Publication-ready PDF version (Vector graphics)
# Optimized for:
# 1. Large and sharp text/numbers/symbols
# 2. Thick lines and borders
# 3. Clear legend and axis labels
# 4. High-resolution vector PDF export
# 5. Colorblind-friendly palette
# 6. Consistent publication typography
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,
  tidyr,
  dplyr,
  ggrepel,
  scales,
  grid,
  cairoDevice
)

# ============================================================================
# 2. Data preparation
# ============================================================================

data <- data.frame(
  Sample = c("8373", "4143", "0942", "8366", "8915"),
  Flye = c(167.3, 128.75, 221.16, 273.98, 173.31),
  MetaMDBG = c(156.3, 120.17, 320.32, 382.67, 207.53),
  MyLoAsm = c(215.01, 151.84, 291.65, 792.39, 328.32)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(
    cols = c(Flye, MetaMDBG, MyLoAsm),
    names_to = "Method",
    values_to = "N50"
  )

# Factor ordering
data_long$Sample <- factor(
  data_long$Sample,
  levels = data$Sample
)

data_long$Method <- factor(
  data_long$Method,
  levels = c("Flye", "MetaMDBG", "MyLoAsm")
)

# ============================================================================
# 3. GigaScience-style palette
# Colorblind-friendly + high contrast
# ============================================================================

gigascience_cols <- c(
  "Flye" = "#0072B2",       # Blue
  "MetaMDBG" = "#D55E00",  # Vermillion
  "MyLoAsm" = "#009E73"    # Bluish green
)

# ============================================================================
# 4. Global publication theme
# Enlarged typography for journal submission
# ============================================================================

theme_gigascience <- function() {
  
  theme_minimal(base_size = 18) +
    
    theme(
      
      # Panel
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1.3
      ),
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.major.y = element_line(
        colour = "#D9D9D9",
        linewidth = 0.5
      ),
      
      panel.grid.minor = element_blank(),
      
      # Axis lines
      axis.line = element_line(
        colour = "black",
        linewidth = 1.1
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 1.1
      ),
      
      axis.ticks.length = unit(0.25, "cm"),
      
      # Axis titles
      axis.title.x = element_text(
        size = 22,
        face = "bold",
        colour = "black",
        margin = margin(t = 14)
      ),
      
      axis.title.y = element_text(
        size = 22,
        face = "bold",
        colour = "black",
        margin = margin(r = 14)
      ),
      
      # Axis text
      axis.text.x = element_text(
        size = 18,
        face = "bold",
        colour = "black"
      ),
      
      axis.text.y = element_text(
        size = 18,
        face = "bold",
        colour = "black"
      ),
      
      # Legend
      legend.position = "top",
      
      legend.direction = "horizontal",
      
      legend.title = element_text(
        size = 19,
        face = "bold",
        colour = "black"
      ),
      
      legend.text = element_text(
        size = 17,
        face = "bold",
        colour = "black"
      ),
      
      legend.key.size = unit(0.9, "cm"),
      
      legend.spacing.x = unit(0.5, "cm"),
      
      legend.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Plot background
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Margins
      plot.margin = margin(
        t = 18,
        r = 18,
        b = 18,
        l = 18
      )
    )
}

# ============================================================================
# 5. Main plot
# ============================================================================

p_n50 <- ggplot(
  data_long,
  aes(
    x = Sample,
    y = N50,
    group = Method,
    colour = Method
  )
) +
  
  # Lines
  geom_line(
    aes(linetype = Method),
    linewidth = 2.0,
    alpha = 0.95
  ) +
  
  # Points
  geom_point(
    aes(fill = Method),
    shape = 21,
    size = 6,
    stroke = 1.6,
    colour = "white"
  ) +
  
  # Labels
  geom_text_repel(
    aes(label = round(N50, 0)),
    
    size = 6,
    
    fontface = "bold",
    
    colour = "black",
    
    box.padding = 0.7,
    
    point.padding = 0.6,
    
    segment.color = "grey40",
    
    segment.size = 0.6,
    
    min.segment.length = 0,
    
    max.overlaps = Inf,
    
    show.legend = FALSE
  ) +
  
  # Color scales
  scale_colour_manual(
    values = gigascience_cols,
    name = "Assembly Method"
  ) +
  
  scale_fill_manual(
    values = gigascience_cols,
    name = "Assembly Method"
  ) +
  
  scale_linetype_manual(
    values = c(
      "Flye" = "solid",
      "MetaMDBG" = "dashed",
      "MyLoAsm" = "dotdash"
    ),
    name = "Assembly Method"
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 850),
    
    breaks = seq(0, 800, 200),
    
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  # Labels
  labs(
    x = "Metagenome Samples",
    y = "N50 (kb)"
  ) +
  
  # Theme
  theme_gigascience()

# ============================================================================
# 6. PDF export (recommended for GigaScience)
# Vector graphics — infinitely scalable
# ============================================================================

# Main publication PDF
ggsave(
  filename = "N50_Comparison_GigaScience.pdf",
  
  plot = p_n50,
  
  device = cairo_pdf,
  
  width = 9.5,
  height = 7,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# ============================================================================
# 7. TIFF export (many journals also require TIFF)
# ============================================================================

ggsave(
  filename = "N50_Comparison_GigaScience.tiff",
  
  plot = p_n50,
  
  width = 9.5,
  height = 7,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
)

# ============================================================================
# 8. Grayscale version
# Useful for print compatibility
# ============================================================================

gray_cols <- c(
  "Flye" = "#333333",
  "MetaMDBG" = "#777777",
  "MyLoAsm" = "#B3B3B3"
)

p_gray <- ggplot(
  data_long,
  aes(
    x = Sample,
    y = N50,
    group = Method,
    colour = Method
  )
) +
  
  geom_line(
    aes(linetype = Method),
    linewidth = 2.0
  ) +
  
  geom_point(
    aes(fill = Method),
    shape = 21,
    size = 6,
    stroke = 1.6,
    colour = "white"
  ) +
  
  geom_text_repel(
    aes(label = round(N50, 0)),
    
    size = 6,
    
    fontface = "bold",
    
    colour = "black",
    
    box.padding = 0.7,
    
    point.padding = 0.6,
    
    segment.color = "grey40",
    
    segment.size = 0.6,
    
    show.legend = FALSE
  ) +
  
  scale_colour_manual(
    values = gray_cols,
    name = "Assembly Method"
  ) +
  
  scale_fill_manual(
    values = gray_cols,
    name = "Assembly Method"
  ) +
  
  scale_linetype_manual(
    values = c(
      "Flye" = "solid",
      "MetaMDBG" = "dashed",
      "MyLoAsm" = "dotdash"
    ),
    name = "Assembly Method"
  ) +
  
  scale_y_continuous(
    limits = c(0, 850),
    
    breaks = seq(0, 800, 200),
    
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    x = "Metagenome Samples",
    y = "N50 (kb)"
  ) +
  
  theme_gigascience()

# Save grayscale PDF
ggsave(
  filename = "N50_Comparison_GigaScience_Gray.pdf",
  
  plot = p_gray,
  
  device = cairo_pdf,
  
  width = 9.5,
  height = 7,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# ============================================================================
# 9. Print validation
# ============================================================================

cat("\n")
cat(paste(rep("=", 75), collapse = ""))
cat("\n")

cat("Publication-ready N50 figures generated successfully!\n\n")

cat("Generated files:\n")
cat("1. N50_Comparison_GigaScience.pdf\n")
cat("2. N50_Comparison_GigaScience.tiff\n")
cat("3. N50_Comparison_GigaScience_Gray.pdf\n\n")

cat("Optimization summary:\n")
cat("- Vector PDF export for journal submission\n")
cat("- Enlarged publication-grade typography\n")
cat("- Thickened lines, borders, and symbols\n")
cat("- High-contrast colorblind-friendly palette\n")
cat("- Enhanced readability of labels and legends\n")
cat("- TIFF version included for submission systems\n")
cat("- Grayscale version included for print compatibility\n\n")

cat("Recommended submission file:\n")
cat("-> N50_Comparison_GigaScience.pdf\n\n")

cat(paste(rep("=", 75), collapse = ""))
cat("\n")