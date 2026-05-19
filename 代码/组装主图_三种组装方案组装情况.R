# ============================================================================
# MAGs Proportion Analysis
# GigaScience Publication-ready Stacked Barplot
# FINAL OPTIMIZED VERSION
#
# Optimizations:
# 1. Publication-grade typography
# 2. Larger labels and numbers
# 3. Vector PDF export
# 4. Colorblind-friendly palette
# 5. High-resolution TIFF export
# 6. Improved spacing and readability
# 7. Stable ggplot2 compatibility
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,
  dplyr,
  tidyr,
  cowplot,
  grid,
  grDevices,
  scales
)

# ============================================================================
# 2. Data preparation
# ============================================================================

total_vOTUs <- 314

assembler_data <- data.frame(
  
  Assembler = c(
    "flye",
    "metamdbg",
    "myloasm",
    "flye_short_polish",
    "metamdbg_short_polish",
    "myloasm_short_polish",
    "hybridspades",
    "opera_ms"
  ),
  
  Single_tool = c(
    235,
    247,
    229,
    252,
    262,
    259,
    275,
    252
  ),
  
  Group = c(
    rep("Long-read", 3),
    rep("Long-read + Short-read", 3),
    rep("HYB", 2)
  )
  
)

# ============================================================================
# 3. Calculate expansion
# ============================================================================

assembler_data$Expansion <-
  total_vOTUs -
  assembler_data$Single_tool

# ============================================================================
# 4. Convert to long format
# ============================================================================

plot_data <- assembler_data %>%
  
  pivot_longer(
    
    cols = c("Single_tool", "Expansion"),
    
    names_to = "Type",
    
    values_to = "Value"
    
  ) %>%
  
  group_by(Assembler) %>%
  
  mutate(
    
    Proportion = Value / sum(Value)
    
  )

# ============================================================================
# 5. Factor ordering
# ============================================================================

plot_data$Type <- factor(
  
  plot_data$Type,
  
  levels = c("Expansion", "Single_tool")
  
)

plot_data$Assembler <- factor(
  
  plot_data$Assembler,
  
  levels = assembler_data$Assembler
  
)

plot_data$Group <- factor(
  
  plot_data$Group,
  
  levels = c(
    "Long-read",
    "Long-read + Short-read",
    "HYB"
  )
  
)

# ============================================================================
# 6. Publication-ready labels
# ============================================================================

assembler_labels <- c(
  
  "flye" = "Flye",
  
  "metamdbg" = "MetaMDBG",
  
  "myloasm" = "MyLoAsm",
  
  "flye_short_polish" = "Flye\n+ Polish",
  
  "metamdbg_short_polish" = "MetaMDBG\n+ Polish",
  
  "myloasm_short_polish" = "MyLoAsm\n+ Polish",
  
  "hybridspades" = "Hybrid\nSPAdes",
  
  "opera_ms" = "OPERA-MS"
  
)

# ============================================================================
# 7. GigaScience scientific palette
# ============================================================================

gigascience_colors <- c(
  
  "Single_tool" = "#0072B2",  # Deep blue
  
  "Expansion" = "#D9D9D9"     # Light gray
  
)

text_colors <- c(
  
  "Single_tool" = "white",
  
  "Expansion" = "#1F1F1F"
  
)

# ============================================================================
# 8. Create publication plotting function
# ============================================================================

create_mag_plot <- function(
    
  base_size = 16,
  
  label_size = 5.2,
  
  line_width = 0.45
  
) {
  
  p <- ggplot(
    
    plot_data,
    
    aes(
      
      x = Assembler,
      
      y = Proportion,
      
      fill = Type
      
    )
    
  ) +
    
    # ------------------------------------------------------------------------
  # Stacked bars
  # ------------------------------------------------------------------------
  
  geom_bar(
    
    stat = "identity",
    
    width = 0.72,
    
    color = "white",
    
    linewidth = line_width
    
  ) +
    
    # ------------------------------------------------------------------------
  # Value labels
  # ------------------------------------------------------------------------
  
  geom_text(
    
    aes(
      
      label = Value,
      
      color = Type,
      
      fontface = ifelse(
        Type == "Single_tool",
        "bold",
        "plain"
      )
      
    ),
    
    position = position_stack(vjust = 0.5),
    
    size = label_size
    
  ) +
    
    # ------------------------------------------------------------------------
  # Facet
  # ------------------------------------------------------------------------
  
  facet_grid(
    
    ~ Group,
    
    scales = "free_x",
    
    space = "free_x"
    
  ) +
    
    # ------------------------------------------------------------------------
  # Colors
  # ------------------------------------------------------------------------
  
  scale_fill_manual(
    
    values = gigascience_colors
    
  ) +
    
    scale_color_manual(
      
      values = text_colors
      
    ) +
    
    # ------------------------------------------------------------------------
  # X labels
  # ------------------------------------------------------------------------
  
  scale_x_discrete(
    
    labels = assembler_labels
    
  ) +
    
    # ------------------------------------------------------------------------
  # Y axis
  # ------------------------------------------------------------------------
  
  scale_y_continuous(
    
    limits = c(0, 1),
    
    breaks = seq(0, 1, 0.25),
    
    labels = percent_format(accuracy = 1),
    
    expand = c(0, 0)
    
  ) +
    
    # ------------------------------------------------------------------------
  # Labels
  # ------------------------------------------------------------------------
  
  labs(
    
    y = "Proportion of Total MAGs",
    
    x = NULL
    
  ) +
    
    # ------------------------------------------------------------------------
  # Theme
  # ------------------------------------------------------------------------
  
  theme_classic(base_size = base_size) +
    
    theme(
      
      # ----------------------------------------------------------------------
      # Text
      # ----------------------------------------------------------------------
      
      text = element_text(
        
        family = "sans",
        
        color = "#1F1F1F"
        
      ),
      
      # ----------------------------------------------------------------------
      # Facet strips
      # ----------------------------------------------------------------------
      
      strip.background = element_blank(),
      
      strip.text = element_text(
        
        size = base_size + 1,
        
        face = "bold",
        
        margin = margin(b = 10)
        
      ),
      
      # ----------------------------------------------------------------------
      # Axis titles
      # ----------------------------------------------------------------------
      
      axis.title.y = element_text(
        
        size = base_size + 1,
        
        face = "bold",
        
        margin = margin(r = 12)
        
      ),
      
      # ----------------------------------------------------------------------
      # Axis text
      # ----------------------------------------------------------------------
      
      axis.text.x = element_text(
        
        angle = 45,
        
        hjust = 1,
        
        vjust = 1,
        
        face = "bold",
        
        size = base_size - 2
        
      ),
      
      axis.text.y = element_text(
        
        face = "bold",
        
        size = base_size - 1
        
      ),
      
      # ----------------------------------------------------------------------
      # Axis lines
      # ----------------------------------------------------------------------
      
      axis.line = element_line(
        
        linewidth = 0.7,
        
        color = "black"
        
      ),
      
      axis.ticks = element_line(
        
        linewidth = 0.7,
        
        color = "black"
        
      ),
      
      axis.ticks.length = unit(0.12, "cm"),
      
      # ----------------------------------------------------------------------
      # Grid
      # ----------------------------------------------------------------------
      
      panel.grid.major.y = element_line(
        
        color = "#EFEFEF",
        
        linewidth = 0.35
        
      ),
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      # ----------------------------------------------------------------------
      # Spacing
      # ----------------------------------------------------------------------
      
      panel.spacing.x = unit(1.4, "lines"),
      
      # ----------------------------------------------------------------------
      # Legend
      # ----------------------------------------------------------------------
      
      legend.position = "none",
      
      # ----------------------------------------------------------------------
      # Margins
      # ----------------------------------------------------------------------
      
      plot.margin = margin(
        
        20,
        20,
        20,
        20
        
      )
      
    )
  
  return(p)
  
}

# ============================================================================
# 9. Main plot
# ============================================================================

p_main <- create_mag_plot()

# ============================================================================
# 10. Add panel label
# ============================================================================

p_final <- ggdraw(p_main) +
  
  draw_label(
    
    "",
    
    x = 0.02,
    
    y = 0.98,
    
    fontface = "bold",
    
    size = 22
    
  )

# ============================================================================
# 11. Export PDF (recommended)
# ============================================================================

ggsave(
  
  filename = "MAGs_Proportion_GigaScience.pdf",
  
  plot = p_final,
  
  device = cairo_pdf,
  
  width = 11,
  
  height = 6.8,
  
  dpi = 600
  
)

# ============================================================================
# 12. Export TIFF
# ============================================================================

ggsave(
  
  filename = "MAGs_Proportion_GigaScience.tiff",
  
  plot = p_final,
  
  width = 11,
  
  height = 6.8,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
  
)

# ============================================================================
# 13. Export PNG
# ============================================================================

ggsave(
  
  filename = "MAGs_Proportion_GigaScience.png",
  
  plot = p_final,
  
  width = 11,
  
  height = 6.8,
  
  dpi = 600,
  
  bg = "white"
  
)

# ============================================================================
# 14. Compact version
# ============================================================================

p_compact <- create_mag_plot(
  
  base_size = 13,
  
  label_size = 4.5,
  
  line_width = 0.35
  
)

ggsave(
  
  filename = "MAGs_Proportion_GigaScience_Compact.png",
  
  plot = p_compact,
  
  width = 8.5,
  
  height = 5.5,
  
  dpi = 600,
  
  bg = "white"
  
)

# ============================================================================
# 15. No-number version
# ============================================================================

p_no_numbers <- ggplot(
  
  plot_data,
  
  aes(
    
    x = Assembler,
    
    y = Proportion,
    
    fill = Type
    
  )
  
) +
  
  geom_bar(
    
    stat = "identity",
    
    width = 0.72,
    
    color = "white",
    
    linewidth = 0.4
    
  ) +
  
  facet_grid(
    
    ~ Group,
    
    scales = "free_x",
    
    space = "free_x"
    
  ) +
  
  scale_fill_manual(
    
    values = gigascience_colors
    
  ) +
  
  scale_x_discrete(
    
    labels = assembler_labels
    
  ) +
  
  scale_y_continuous(
    
    limits = c(0, 1),
    
    breaks = seq(0, 1, 0.25),
    
    labels = percent_format(accuracy = 1),
    
    expand = c(0, 0)
    
  ) +
  
  labs(
    
    y = "Proportion of Total MAGs",
    
    x = NULL
    
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      
      face = "bold",
      
      size = 17
      
    ),
    
    axis.text.x = element_text(
      
      angle = 45,
      
      hjust = 1,
      
      face = "bold",
      
      size = 13
      
    ),
    
    axis.text.y = element_text(
      
      face = "bold",
      
      size = 14
      
    ),
    
    axis.title.y = element_text(
      
      face = "bold",
      
      size = 17
      
    ),
    
    axis.line = element_line(
      
      linewidth = 0.7
      
    ),
    
    axis.ticks = element_line(
      
      linewidth = 0.7
      
    ),
    
    panel.grid.major.y = element_line(
      
      color = "#EFEFEF",
      
      linewidth = 0.35
      
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    legend.position = "none"
    
  )

ggsave(
  
  filename = "MAGs_Proportion_GigaScience_NoNumbers.png",
  
  plot = p_no_numbers,
  
  width = 11,
  
  height = 6.8,
  
  dpi = 600,
  
  bg = "white"
  
)

# ============================================================================
# 16. Grayscale version
# ============================================================================

gray_colors <- c(
  
  "Single_tool" = "#4D4D4D",
  
  "Expansion" = "#D9D9D9"
  
)

p_gray <- p_main +
  
  scale_fill_manual(values = gray_colors)

ggsave(
  
  filename = "MAGs_Proportion_GigaScience_Gray.pdf",
  
  plot = p_gray,
  
  device = cairo_pdf,
  
  width = 11,
  
  height = 6.8,
  
  dpi = 600
  
)

# ============================================================================
# 17. Terminal report
# ============================================================================

cat("\n")
cat(paste(rep("=", 85), collapse = ""))
cat("\n")

cat("✅ Publication-ready MAGs proportion plot generated successfully!\n\n")

cat("Generated files:\n")

cat("1. MAGs_Proportion_GigaScience.pdf\n")
cat("2. MAGs_Proportion_GigaScience.tiff\n")
cat("3. MAGs_Proportion_GigaScience.png\n")
cat("4. MAGs_Proportion_GigaScience_Compact.png\n")
cat("5. MAGs_Proportion_GigaScience_NoNumbers.png\n")
cat("6. MAGs_Proportion_GigaScience_Gray.pdf\n\n")

cat("GigaScience optimizations:\n")

cat("- Publication-grade typography\n")
cat("- Enlarged labels and axis text\n")
cat("- Colorblind-friendly scientific palette\n")
cat("- Vector PDF export\n")
cat("- High-resolution TIFF export\n")
cat("- Enhanced spacing and readability\n")
cat("- Improved facet layout\n")
cat("- Grayscale-compatible version\n\n")

cat("Recommended submission file:\n")

cat("-> MAGs_Proportion_GigaScience.pdf\n\n")

cat(paste(rep("=", 85), collapse = ""))
cat("\n")