# ============================================================================
# Assembly Cycle Heatmap
# GigaScience Publication-ready Heatmap (Blue Theme)
#
# Optimized for:
# 1. Publication-grade typography
# 2. High-resolution vector PDF export
# 3. Nature/GigaScience blue scientific palette
# 4. Enhanced readability and spacing
# 5. TIFF + PNG export for submission
# 6. Clean panel organization
# 7. Colorblind-friendly contrast
# ============================================================================

# ============================================================================
# 1. Clear environment
# ============================================================================

rm(list = ls())

# ============================================================================
# 2. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,
  dplyr,
  tidyr,
  grid,
  gridExtra,
  cowplot,
  scales,
  grDevices,
  pdftools
)

# ============================================================================
# 3. Data preparation
# ============================================================================

cycle_data <- data.frame(
  
  sample = paste0("Sample_", 1:5),
  
  flye_cycle = c(2, 2, 7, 5, 6),
  
  flye_short_polish_cycle = c(3, 2, 7, 5, 6),
  
  hybridspades_cycle = c(2, 0, 1, 3, 0),
  
  metamdbg_cycle = c(4, 6, 8, 11, 7),
  
  metamdbg_short_polish_cycle = c(4, 5, 8, 10, 7),
  
  myloasm_cycle = c(8, 9, 18, 13, 16),
  
  myloasm_short_polish_cycle = c(9, 8, 18, 6, 16),
  
  opera_ms_cycle = c(0, 1, 1, 0, 0)
)

# ============================================================================
# 4. Convert to long format
# ============================================================================

cycle_long <- cycle_data %>%
  
  pivot_longer(
    
    cols = -sample,
    
    names_to = "tool",
    
    values_to = "cycle"
  ) %>%
  
  mutate(
    
    tool = gsub("_cycle", "", tool),
    
    tool_display = case_when(
      
      tool == "flye" ~ "Flye",
      
      tool == "metamdbg" ~ "MetaMDBG",
      
      tool == "myloasm" ~ "MyLoAsm",
      
      tool == "flye_short_polish" ~ "Flye\n+ Polish",
      
      tool == "metamdbg_short_polish" ~ "MetaMDBG\n+ Polish",
      
      tool == "myloasm_short_polish" ~ "MyLoAsm\n+ Polish",
      
      tool == "hybridspades" ~ "Hybrid\nSPAdes",
      
      tool == "opera_ms" ~ "OPERA-MS"
    )
  )

# ============================================================================
# 5. Define plotting order
# ============================================================================

tool_order <- c(
  
  "Flye",
  
  "MetaMDBG",
  
  "MyLoAsm",
  
  "Flye\n+ Polish",
  
  "MetaMDBG\n+ Polish",
  
  "MyLoAsm\n+ Polish",
  
  "Hybrid\nSPAdes",
  
  "OPERA-MS"
)

cycle_long$tool_display <- factor(
  
  cycle_long$tool_display,
  
  levels = tool_order
)

cycle_long$sample <- factor(
  
  cycle_long$sample,
  
  levels = rev(paste0("Sample_", 1:5))
)

# ============================================================================
# 6. Scientific blue palette
# ============================================================================

gigascience_blue <- c(
  
  "#F7FBFF",
  "#DEEBF7",
  "#C6DBEF",
  "#9ECAE1",
  "#6BAED6",
  "#4292C6",
  "#2171B5",
  "#08519C",
  "#08306B"
)

# ============================================================================
# 7. Main heatmap function
# ============================================================================

create_heatmap <- function() {
  
  p <- ggplot(
    
    cycle_long,
    
    aes(
      x = tool_display,
      y = sample,
      fill = cycle
    )
  ) +
    
    # ----------------------------------------------------------------------
  # Heatmap tiles
  # ----------------------------------------------------------------------
  
  geom_tile(
    
    color = "white",
    
    linewidth = 1.0
  ) +
    
    # ----------------------------------------------------------------------
  # Value labels
  # ----------------------------------------------------------------------
  
  geom_text(
    
    aes(
      label = cycle,
      color = cycle >= 10
    ),
    
    fontface = "bold",
    
    size = 5.2,
    
    show.legend = FALSE
  ) +
    
    scale_color_manual(
      
      values = c(
        "TRUE" = "white",
        "FALSE" = "#1F1F1F"
      )
    ) +
    
    # ----------------------------------------------------------------------
  # Blue gradient palette
  # ----------------------------------------------------------------------
  
  scale_fill_gradientn(
    
    colors = gigascience_blue,
    
    limits = c(0, 18),
    
    breaks = seq(0, 18, 3),
    
    oob = squish,
    
    name = "Cycle Count",
    
    guide = guide_colorbar(
      
      title.position = "top",
      
      title.hjust = 0.5,
      
      frame.colour = "#2C3E50",
      
      ticks.colour = "#2C3E50",
      
      barwidth = unit(0.8, "cm"),
      
      barheight = unit(6, "cm")
    )
  ) +
    
    # ----------------------------------------------------------------------
  # Group separators
  # ----------------------------------------------------------------------
  
  geom_vline(
    
    xintercept = c(3.5, 6.5),
    
    color = "#2C3E50",
    
    linewidth = 1.2
  ) +
    
    # ----------------------------------------------------------------------
  # Labels
  # ----------------------------------------------------------------------
  
  labs(
    
    x = "Assembly Tools",
    
    y = NULL
  ) +
    
    # ----------------------------------------------------------------------
  # GigaScience theme
  # ----------------------------------------------------------------------
  
  theme_minimal(
    
    base_family = "Arial",
    
    base_size = 14
  ) +
    
    theme(
      
      text = element_text(
        color = "#2C3E50"
      ),
      
      # Axis text
      axis.text.x = element_text(
        
        angle = 45,
        
        hjust = 1,
        
        vjust = 1,
        
        face = "bold",
        
        size = 12
      ),
      
      axis.text.y = element_blank(),
      
      axis.ticks.y = element_blank(),
      
      # Axis title
      axis.title.x = element_text(
        
        face = "bold",
        
        size = 14,
        
        margin = margin(t = 15)
      ),
      
      # Legend
      legend.position = "right",
      
      legend.title = element_text(
        
        face = "bold",
        
        size = 12
      ),
      
      legend.text = element_text(
        
        size = 11
      ),
      
      # Remove grids
      panel.grid = element_blank(),
      
      # Background
      panel.background = element_rect(
        
        fill = "white",
        
        color = NA
      ),
      
      plot.background = element_rect(
        
        fill = "white",
        
        color = NA
      ),
      
      # Margins
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
# 8. Group annotation
# ============================================================================

create_annotation <- function() {
  
  ggplot() +
    
    annotate(
      
      "segment",
      
      x = c(0.6, 3.6, 6.6),
      
      xend = c(3.4, 6.4, 8.4),
      
      y = 0,
      
      yend = 0,
      
      linewidth = 1.3,
      
      color = "#2C3E50"
    ) +
    
    annotate(
      
      "text",
      
      x = c(2, 5, 7.5),
      
      y = 0.45,
      
      label = c(
        "Long-read",
        "Long-read+SR Polish",
        "Hybrid (HYB)"
      ),
      
      fontface = "bold",
      
      size = 5.2,
      
      color = "#2C3E50"
    ) +
    
    xlim(0.5, 8.5) +
    
    ylim(-0.2, 1) +
    
    theme_void() +
    
    theme(
      
      plot.background = element_rect(
        
        fill = "white",
        
        color = NA
      )
    )
}

# ============================================================================
# 9. Combine plot
# ============================================================================

heatmap_main <- create_heatmap()

annotation_plot <- create_annotation()

title_grob <- textGrob(
  
  "Comparative Analysis of Assembly Cycle Counts",
  
  gp = gpar(
    
    fontsize = 16,
    
    fontface = "bold",
    
    col = "#2C3E50"
  )
)

combined_plot <- grid.arrange(
  
  title_grob,
  
  annotation_plot,
  
  heatmap_main,
  
  nrow = 3,
  
  heights = c(0.08, 0.08, 0.84)
)

# ============================================================================
# 10. Export PDF (Recommended)
# ============================================================================

ggsave(
  
  filename = "Assembly_Cycle_Heatmap_GigaScience.pdf",
  
  plot = combined_plot,
  
  width = 9,
  
  height = 6.8,
  
  device = cairo_pdf,
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 11. Export TIFF
# ============================================================================

tiff(
  
  filename = "Assembly_Cycle_Heatmap_GigaScience.tiff",
  
  width = 9,
  
  height = 6.8,
  
  units = "in",
  
  res = 600,
  
  compression = "lzw",
  
  bg = "white"
)

grid.arrange(
  
  title_grob,
  
  annotation_plot,
  
  heatmap_main,
  
  nrow = 3,
  
  heights = c(0.08, 0.08, 0.84)
)

dev.off()

# ============================================================================
# 12. Export PNG
# ============================================================================

ggsave(
  
  filename = "Assembly_Cycle_Heatmap_GigaScience.png",
  
  plot = combined_plot,
  
  width = 9,
  
  height = 6.8,
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 13. Compact version
# ============================================================================

ggsave(
  
  filename = "Assembly_Cycle_Heatmap_GigaScience_Compact.png",
  
  plot = combined_plot,
  
  width = 7,
  
  height = 5.5,
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 14. Grayscale version
# ============================================================================

gray_palette <- c(
  
  "#F5F5F5",
  "#E0E0E0",
  "#CCCCCC",
  "#B3B3B3",
  "#999999",
  "#808080",
  "#666666",
  "#4D4D4D",
  "#262626"
)

gray_heatmap <- ggplot(
  
  cycle_long,
  
  aes(
    x = tool_display,
    y = sample,
    fill = cycle
  )
) +
  
  geom_tile(
    
    color = "white",
    
    linewidth = 1
  ) +
  
  geom_text(
    
    aes(
      label = cycle,
      color = cycle >= 10
    ),
    
    fontface = "bold",
    
    size = 5,
    
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    
    values = c(
      "TRUE" = "white",
      "FALSE" = "black"
    )
  ) +
  
  scale_fill_gradientn(
    
    colors = gray_palette,
    
    limits = c(0, 18),
    
    breaks = seq(0, 18, 3),
    
    name = "Cycle Count"
  ) +
  
  geom_vline(
    
    xintercept = c(3.5, 6.5),
    
    color = "black",
    
    linewidth = 1.2
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    
    axis.text.x = element_text(
      
      angle = 45,
      
      hjust = 1,
      
      face = "bold"
    ),
    
    axis.text.y = element_text(
      
      face = "bold"
    ),
    
    panel.grid = element_blank()
  )

ggsave(
  
  filename = "Assembly_Cycle_Heatmap_GigaScience_Gray.pdf",
  
  plot = gray_heatmap,
  
  width = 9,
  
  height = 6.8,
  
  device = cairo_pdf,
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 15. Convert PDF to PNG
# ============================================================================

pdf_convert(
  
  pdf = "Assembly_Cycle_Heatmap_GigaScience.pdf",
  
  format = "png",
  
  dpi = 300,
  
  filenames = "Assembly_Cycle_Heatmap_FromPDF.png"
)

# ============================================================================
# 16. Terminal report
# ============================================================================

cat("\n")
cat(paste(rep("=", 85), collapse = ""))
cat("\n")

cat("Publication-ready assembly cycle heatmap generated successfully!\n\n")

cat("Generated files:\n")
cat("1. Assembly_Cycle_Heatmap_GigaScience.pdf\n")
cat("2. Assembly_Cycle_Heatmap_GigaScience.tiff\n")
cat("3. Assembly_Cycle_Heatmap_GigaScience.png\n")
cat("4. Assembly_Cycle_Heatmap_GigaScience_Compact.png\n")
cat("5. Assembly_Cycle_Heatmap_GigaScience_Gray.pdf\n")
cat("6. Assembly_Cycle_Heatmap_FromPDF.png\n\n")

cat("GigaScience optimizations:\n")
cat("- Publication-grade typography\n")
cat("- Vector PDF export\n")
cat("- High-resolution TIFF export\n")
cat("- Enhanced tile readability\n")
cat("- Thickened separator lines\n")
cat("- Scientific blue gradient palette\n")
cat("- Improved spacing and margins\n")
cat("- Colorblind-friendly contrast\n")
cat("- Compact version included\n")
cat("- Grayscale version included\n\n")

cat("Heatmap summary:\n")
cat("- Samples: 5\n")
cat("- Assembly tools: 8\n")
cat("- Cycle range: 0-18\n")
cat("- Three assembly strategy groups included\n\n")

cat("Recommended submission file:\n")
cat("-> Assembly_Cycle_Heatmap_GigaScience.pdf\n\n")

cat(paste(rep("=", 85), collapse = ""))
cat("\n")

# ============================================================================
# 17. Summary statistics
# ============================================================================

summary_stats <- cycle_long %>%
  
  group_by(tool_display) %>%
  
  summarise(
    
    Min = min(cycle),
    
    Mean = round(mean(cycle), 1),
    
    Max = max(cycle),
    
    SD = round(sd(cycle), 1),
    
    .groups = "drop"
  )

print(summary_stats)

cat("\n")