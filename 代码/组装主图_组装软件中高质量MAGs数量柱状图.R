# ============================================================================
# Figure Title:
# Impact of Assembly Methods and Polishing Strategies on Recovery of
# Medium-quality and Near-complete MAGs
#
# Journal Style:
# GigaScience Publication-ready Version
#
# Optimizations:
# 1. Large and sharp publication-grade typography
# 2. Enhanced readability for labels/numbers/symbols
# 3. Vector PDF export
# 4. Colorblind-friendly scientific palette
# 5. Thick borders and clear group separation
# 6. TIFF export for journal submission
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,
  dplyr,
  tidyr,
  scales,
  ggpubr,
  forcats,
  cowplot,
  grid,
  cairoDevice
)

# ============================================================================
# 2. Data preparation
# ============================================================================

plot_data <- data.frame(
  Software = factor(
    c(
      "Flye",
      "MetaMDBG",
      "MyLoAsm",
      "Flye + SR",
      "MetaMDBG + SR",
      "MyLoAsm + SR",
      "HybridSPAdes",
      "OPERA-MS"
    ),
    
    levels = c(
      "Flye",
      "MetaMDBG",
      "MyLoAsm",
      "Flye + SR",
      "MetaMDBG + SR",
      "MyLoAsm + SR",
      "HybridSPAdes",
      "OPERA-MS"
    )
  ),
  
  Method = factor(
    c(
      rep("Long-read", 3),
      rep("Long-read + Short-read", 3),
      rep("HYB", 2)
    ),
    
    levels = c(
      "Long-read",
      "Long-read + Short-read",
      "HYB"
    )
  ),
  
  NC_MAGs = c(
    120, 156, 101,
    173, 207, 139,
    190, 194
  ),
  
  MQ_MAGs = c(
    226, 201, 179,
    223, 204, 232,
    258, 183
  )
)

# ============================================================================
# 3. Convert to long format
# ============================================================================

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(NC_MAGs, MQ_MAGs),
    
    names_to = "Quality",
    
    values_to = "Count",
    
    names_pattern = "(.*)_MAGs"
  ) %>%
  
  mutate(
    Quality = factor(
      Quality,
      
      levels = c("MQ", "NC"),
      
      labels = c(
        "Medium-quality",
        "Near-complete"
      )
    )
  )

# ============================================================================
# 4. Publication-quality palette
# Colorblind-friendly + high contrast
# ============================================================================

quality_colors <- c(
  "Near-complete" = "#0072B2",    # Deep blue
  "Medium-quality" = "#56B4E9"   # Light blue
)

# ============================================================================
# 5. Group positions
# ============================================================================

group_positions <- data.frame(
  Method = c(
    "Long-read",
    "Long-read + Short-read",
    "HYB"
  ),
  
  x_start = c(1, 4, 7),
  
  x_end = c(3, 6, 8)
  
) %>%
  
  mutate(
    x_center = (x_start + x_end) / 2
  )

# ============================================================================
# 6. Global GigaScience theme
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
      
      # Axis
      axis.line = element_line(
        colour = "black",
        linewidth = 1.1
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 1.1
      ),
      
      axis.ticks.length = unit(0.22, "cm"),
      
      # Axis titles
      axis.title.y = element_text(
        size = 24,
        face = "bold",
        colour = "black",
        margin = margin(r = 14)
      ),
      
      axis.title.x = element_blank(),
      
      # Axis texts
      axis.text.x = element_text(
        size = 17,
        face = "bold",
        colour = "black",
        angle = 45,
        hjust = 1,
        vjust = 1
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
      
      legend.key.size = unit(0.8, "cm"),
      
      legend.spacing.x = unit(0.4, "cm"),
      
      legend.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Plot margins
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
# 7. Main stacked bar plot
# ============================================================================

p_main <- ggplot(
  plot_data_long,
  aes(
    x = Software,
    y = Count,
    fill = Quality
  )
) +
  
  # Stacked bars
  geom_bar(
    stat = "identity",
    
    position = "stack",
    
    width = 0.72,
    
    colour = "white",
    
    linewidth = 0.7,
    
    alpha = 0.95
  ) +
  
  # Value labels
  geom_text(
    aes(
      label = Count,
      
      fontface = ifelse(
        Quality == "Near-complete",
        "bold",
        "plain"
      )
    ),
    
    position = position_stack(vjust = 0.5),
    
    size = 6,
    
    colour = "white"
  ) +
  
  # Group separator lines
  geom_vline(
    xintercept = c(3.5, 6.5),
    
    linewidth = 1.5,
    
    colour = "black",
    
    linetype = "solid"
  ) +
  
  # Color mapping
  scale_fill_manual(
    values = quality_colors,
    
    name = "MAG Quality",
    
    guide = guide_legend(reverse = TRUE)
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 550),
    
    breaks = seq(0, 500, 100),
    
    expand = expansion(mult = c(0, 0.03))
  ) +
  
  # Labels
  labs(
    y = "Number of Recovered MAGs"
  ) +
  
  # Theme
  theme_gigascience()

# ============================================================================
# 8. Group labels panel
# ============================================================================

group_labels <- ggplot() +
  
  # Group titles
  geom_text(
    data = group_positions,
    
    aes(
      x = x_center,
      y = 0.55,
      label = Method,
      colour = Method
    ),
    
    size = 7,
    
    fontface = "bold"
  ) +
  
  # Underline segments
  geom_segment(
    data = group_positions,
    
    aes(
      x = x_start - 0.4,
      xend = x_end + 0.4,
      y = 0.82,
      yend = 0.82,
      colour = Method
    ),
    
    linewidth = 2.5
  ) +
  
  scale_colour_manual(
    values = c(
      "Long-read" = "#0072B2",
      "Long-read + Short-read" = "#D55E00",
      "HYB" = "#009E73"
    )
  ) +
  
  xlim(0.5, 8.5) +
  
  ylim(0, 1) +
  
  theme_void() +
  
  theme(
    legend.position = "none",
    
    plot.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0
    )
  )

# ============================================================================
# 9. Combine plots
# ============================================================================

final_plot <- plot_grid(
  group_labels,
  p_main,
  
  ncol = 1,
  
  rel_heights = c(0.10, 0.90),
  
  align = "v"
)

# ============================================================================
# 10. White background
# ============================================================================

final_plot <- ggdraw(final_plot) +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# ============================================================================
# 11. Export publication files
# ============================================================================

# -------------------------
# Main PDF (recommended)
# -------------------------

ggsave(
  filename = "MAGs_Yield_GigaScience.pdf",
  
  plot = final_plot,
  
  device = cairo_pdf,
  
  width = 11,
  height = 8,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# -------------------------
# TIFF version
# -------------------------

ggsave(
  filename = "MAGs_Yield_GigaScience.tiff",
  
  plot = final_plot,
  
  width = 11,
  height = 8,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
)

# -------------------------
# PNG preview version
# -------------------------

ggsave(
  filename = "MAGs_Yield_GigaScience.png",
  
  plot = final_plot,
  
  width = 11,
  height = 8,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 12. Grayscale version
# ============================================================================

gray_colors <- c(
  "Near-complete" = "#4D4D4D",
  "Medium-quality" = "#A6A6A6"
)

p_gray <- p_main +
  
  scale_fill_manual(
    values = gray_colors,
    
    name = "MAG Quality",
    
    guide = guide_legend(reverse = TRUE)
  )

gray_final <- plot_grid(
  group_labels,
  p_gray,
  
  ncol = 1,
  
  rel_heights = c(0.10, 0.90),
  
  align = "v"
)

ggsave(
  filename = "MAGs_Yield_GigaScience_Gray.pdf",
  
  plot = gray_final,
  
  device = cairo_pdf,
  
  width = 11,
  height = 8,
  
  units = "in",
  
  dpi = 1200,
  
  bg = "white"
)

# ============================================================================
# 13. Terminal output
# ============================================================================

cat("\n")
cat(paste(rep("=", 78), collapse = ""))
cat("\n")

cat("Publication-ready MAG recovery figure generated successfully!\n\n")

cat("Generated files:\n")
cat("1. MAGs_Yield_GigaScience.pdf\n")
cat("2. MAGs_Yield_GigaScience.tiff\n")
cat("3. MAGs_Yield_GigaScience.png\n")
cat("4. MAGs_Yield_GigaScience_Gray.pdf\n\n")

cat("GigaScience optimizations:\n")
cat("- Vector PDF export for journal submission\n")
cat("- Enlarged publication-grade typography\n")
cat("- Thickened borders and separator lines\n")
cat("- Improved readability of numbers and labels\n")
cat("- Colorblind-friendly scientific palette\n")
cat("- High-resolution TIFF included\n")
cat("- Grayscale version included\n")
cat("- Clear group separation and hierarchy\n\n")

cat("Recommended submission file:\n")
cat("-> MAGs_Yield_GigaScience.pdf\n\n")

cat(paste(rep("=", 78), collapse = ""))
cat("\n")

# ============================================================================
# 14. Display plot
# ============================================================================

print(final_plot)