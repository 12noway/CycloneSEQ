# ============================================================================
# MAGs Assembly Intersection Analysis
# GigaScience Publication-ready UpSet Plot
#
# Optimized for:
# 1. Publication-grade typography
# 2. Clear symbols and labels
# 3. Vector PDF export
# 4. Colorblind-friendly scientific palette
# 5. High-resolution TIFF export
# 6. Enhanced readability for journal submission
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  UpSetR,
  grid,
  grDevices
)

# ============================================================================
# 2. Define intersection data
# ============================================================================

expressions <- c(
  
  "Long-read" = 0,
  
  "Long-read (Short-read polished)" = 17,
  
  "HYB" = 2,
  
  "Long-read&Long-read (Short-read polished)" = 5,
  
  "Long-read&HYB" = 11,
  
  "Long-read (Short-read polished)&HYB" = 18,
  
  "Long-read&Long-read (Short-read polished)&HYB" = 261
)

# ============================================================================
# 3. Publication-quality scientific palette
# Colorblind-friendly + high contrast
# ============================================================================

gigascience_colors <- c(
  
  "#0072B2",   # Deep blue
  
  "#D55E00",   # Vermillion
  
  "#009E73"    # Bluish green
)

# ============================================================================
# 4. Common plotting function
# ============================================================================

create_upset_plot <- function() {
  
  upset(
    
    fromExpression(expressions),
    
    nsets = 3,
    
    # ------------------------------------------------------------------------
    # Sorting and layout
    # ------------------------------------------------------------------------
    
    order.by = "freq",
    
    decreasing = TRUE,
    
    mb.ratio = c(0.62, 0.38),
    
    keep.order = TRUE,
    
    # ------------------------------------------------------------------------
    # Geometry
    # ------------------------------------------------------------------------
    
    point.size = 6.5,
    
    line.size = 2.0,
    
    # ------------------------------------------------------------------------
    # Labels
    # ------------------------------------------------------------------------
    
    mainbar.y.label = "Number of MAGs",
    
    sets.x.label = "Total MAGs Per Method",
    
    # ------------------------------------------------------------------------
    # Publication typography
    # text.scale order:
    # c(
    #   main bar title,
    #   main bar ticks,
    #   set size title,
    #   set size ticks,
    #   set names,
    #   intersection numbers
    # )
    # ------------------------------------------------------------------------
    
    text.scale = c(
      2.3,  # Main bar title
      1.9,  # Main bar ticks
      2.1,  # Set size title
      1.8,  # Set size ticks
      2.0,  # Set names
      2.0   # Intersection numbers
    ),
    
    # ------------------------------------------------------------------------
    # Colors
    # ------------------------------------------------------------------------
    
    sets.bar.color = gigascience_colors,
    
    main.bar.color = "#1F1F1F",
    
    matrix.color = "#1F1F1F",
    
    shade.color = "#F2F2F2",
    
    shade.alpha = 0.7,
    
    # ------------------------------------------------------------------------
    # Additional formatting
    # ------------------------------------------------------------------------
    
    scale.intersections = "identity",
    
    number.angles = 0,
    
    matrix.dot.alpha = 1,
    
    mainbar.y.max = 300
  )
}

# ============================================================================
# 5. Export publication-quality PDF (recommended)
# Vector graphics for journal submission
# ============================================================================

cairo_pdf(
  filename = "MAGs_Intersection_GigaScience.pdf",
  
  width = 11,
  
  height = 8
)

create_upset_plot()

dev.off()

# ============================================================================
# 6. Export TIFF version
# Many journals require TIFF
# ============================================================================

tiff(
  filename = "MAGs_Intersection_GigaScience.tiff",
  
  width = 11,
  
  height = 8,
  
  units = "in",
  
  res = 600,
  
  compression = "lzw"
)

create_upset_plot()

dev.off()

# ============================================================================
# 7. Export high-resolution PNG preview
# ============================================================================

png(
  filename = "MAGs_Intersection_GigaScience.png",
  
  width = 11,
  
  height = 8,
  
  units = "in",
  
  res = 600,
  
  bg = "white"
)

create_upset_plot()

dev.off()

# ============================================================================
# 8. Compact version
# ============================================================================

png(
  filename = "MAGs_Intersection_GigaScience_Compact.png",
  
  width = 8.5,
  
  height = 6.5,
  
  units = "in",
  
  res = 600,
  
  bg = "white"
)

upset(
  
  fromExpression(expressions),
  
  nsets = 3,
  
  order.by = "freq",
  
  decreasing = TRUE,
  
  mb.ratio = c(0.62, 0.38),
  
  keep.order = TRUE,
  
  point.size = 5.2,
  
  line.size = 1.6,
  
  mainbar.y.label = "Number of MAGs",
  
  sets.x.label = "Total MAGs Per Method",
  
  text.scale = c(
    1.9,
    1.5,
    1.8,
    1.5,
    1.7,
    1.7
  ),
  
  sets.bar.color = gigascience_colors,
  
  main.bar.color = "#1F1F1F",
  
  matrix.color = "#1F1F1F",
  
  shade.color = "#F2F2F2",
  
  shade.alpha = 0.7,
  
  scale.intersections = "identity",
  
  number.angles = 0,
  
  matrix.dot.alpha = 1,
  
  mainbar.y.max = 300
)

dev.off()

# ============================================================================
# 9. Grayscale version
# For print compatibility
# ============================================================================

gray_colors <- c(
  "#4D4D4D",
  "#808080",
  "#B3B3B3"
)

cairo_pdf(
  filename = "MAGs_Intersection_GigaScience_Gray.pdf",
  
  width = 11,
  
  height = 8
)

upset(
  
  fromExpression(expressions),
  
  nsets = 3,
  
  order.by = "freq",
  
  decreasing = TRUE,
  
  mb.ratio = c(0.62, 0.38),
  
  keep.order = TRUE,
  
  point.size = 6.5,
  
  line.size = 2.0,
  
  mainbar.y.label = "Number of MAGs",
  
  sets.x.label = "Total MAGs Per Method",
  
  text.scale = c(
    2.3,
    1.9,
    2.1,
    1.8,
    2.0,
    2.0
  ),
  
  sets.bar.color = gray_colors,
  
  main.bar.color = "#1F1F1F",
  
  matrix.color = "#1F1F1F",
  
  shade.color = "#E6E6E6",
  
  shade.alpha = 0.7,
  
  scale.intersections = "identity",
  
  number.angles = 0,
  
  matrix.dot.alpha = 1,
  
  mainbar.y.max = 300
)

dev.off()

# ============================================================================
# 10. Terminal output
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")

cat("Publication-ready UpSet plot generated successfully!\n\n")

cat("Generated files:\n")
cat("1. MAGs_Intersection_GigaScience.pdf\n")
cat("2. MAGs_Intersection_GigaScience.tiff\n")
cat("3. MAGs_Intersection_GigaScience.png\n")
cat("4. MAGs_Intersection_GigaScience_Compact.png\n")
cat("5. MAGs_Intersection_GigaScience_Gray.pdf\n\n")

cat("GigaScience optimizations:\n")
cat("- Vector PDF export for journal submission\n")
cat("- Enlarged publication-grade typography\n")
cat("- Thickened matrix points and connection lines\n")
cat("- Improved readability of numbers and labels\n")
cat("- Colorblind-friendly scientific palette\n")
cat("- High-resolution TIFF included\n")
cat("- Grayscale version included\n")
cat("- Enhanced contrast and spacing\n\n")

cat("Intersection summary:\n")
cat("- Long-read unique MAGs: 0\n")
cat("- Long-read polished unique MAGs: 17\n")
cat("- HYB unique MAGs: 2\n")
cat("- Long-read + polished shared MAGs: 5\n")
cat("- Long-read + HYB shared MAGs: 11\n")
cat("- Polished + HYB shared MAGs: 18\n")
cat("- Shared across all methods: 261\n\n")

cat("Recommended submission file:\n")
cat("-> MAGs_Intersection_GigaScience.pdf\n\n")

cat(paste(rep("=", 80), collapse = ""))
cat("\n")