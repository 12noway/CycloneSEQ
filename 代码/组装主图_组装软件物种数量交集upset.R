# ============================================================================
# Bacterial Species Identification Analysis
# GigaScience Publication-ready UpSet Plot
# FINAL FIXED VERSION
# ============================================================================

# ============================================================================
# 1. Load packages
# ============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  UpSetR,
  dplyr,
  grDevices,
  grid
)

# ============================================================================
# 2. Read data
# ============================================================================

raw_data <- read.csv(
  "../表格/组装主图_组装物种交集upset.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# ============================================================================
# 3. Clean species lists
# ============================================================================

clean_list <- lapply(raw_data, function(x) {
  
  x_clean <- x[
    !is.na(x) &
      x != ""
  ]
  
  unique(x_clean)
})

# ============================================================================
# 4. Convert to binary matrix
# ============================================================================

all_species <- unique(unlist(clean_list))

upset_data_raw <- as.data.frame(
  
  sapply(clean_list, function(x) {
    
    as.integer(all_species %in% x)
    
  })
  
)

rownames(upset_data_raw) <- all_species

# ============================================================================
# 5. Generate combination labels
# ============================================================================

upset_data_raw$combination_str <- apply(
  
  upset_data_raw,
  
  1,
  
  function(row) {
    
    paste(
      colnames(upset_data_raw)[as.logical(row)],
      collapse = "&"
    )
    
  }
  
)

# ============================================================================
# 6. Calculate combination sizes
# ============================================================================

combination_size <- upset_data_raw %>%
  
  group_by(combination_str) %>%
  
  summarise(
    combo_size = n(),
    .groups = "drop"
  )

# ============================================================================
# 7. Merge and filter
# ============================================================================

upset_data_filtered <- upset_data_raw %>%
  
  left_join(
    combination_size,
    by = "combination_str"
  )

upset_data_final <- upset_data_filtered %>%
  
  filter(combo_size >= 5) %>%
  
  select(
    -combination_str,
    -combo_size
  )

# ============================================================================
# 8. Statistics
# ============================================================================

removed_species_count <-
  nrow(upset_data_raw) -
  nrow(upset_data_final)

remaining_intersections <-
  length(
    unique(
      upset_data_filtered$combination_str[
        upset_data_filtered$combo_size >= 5
      ]
    )
  )

# ============================================================================
# 9. Define safe colors
# IMPORTANT:
# UpSetR is unstable with vector colors in some versions
# Use SINGLE color only for stability
# ============================================================================

main_blue <- "#0072B2"
main_black <- "#1F1F1F"
shade_gray <- "#F2F2F2"

# ============================================================================
# 10. Publication plotting function
# ============================================================================

create_upset_plot <- function(
    
  text_scale = c(2.4, 1.9, 2.2, 1.8, 2.0, 1.9),
  
  point_size = 6,
  
  line_size = 2
  
) {
  
  upset(
    
    upset_data_final,
    
    nsets = ncol(upset_data_final),
    
    # ------------------------------------------------------------------------
    # Layout
    # ------------------------------------------------------------------------
    
    order.by = "freq",
    
    decreasing = TRUE,
    
    keep.order = TRUE,
    
    mb.ratio = c(0.58, 0.42),
    
    # ------------------------------------------------------------------------
    # Typography
    # ------------------------------------------------------------------------
    
    text.scale = text_scale,
    
    # ------------------------------------------------------------------------
    # Geometry
    # ------------------------------------------------------------------------
    
    point.size = point_size,
    
    line.size = line_size,
    
    number.angles = 0,
    
    matrix.dot.alpha = 1,
    
    # ------------------------------------------------------------------------
    # Colors
    # ------------------------------------------------------------------------
    
    main.bar.color = main_black,
    
    sets.bar.color = main_blue,
    
    matrix.color = main_black,
    
    shade.color = shade_gray,
    
    shade.alpha = 0.7,
    
    # ------------------------------------------------------------------------
    # Labels
    # ------------------------------------------------------------------------
    
    mainbar.y.label = "Intersection Size (Species)",
    
    sets.x.label = "Total Species Per Tool"
    
  )
  
}

# ============================================================================
# 11. Export PDF (recommended)
# ============================================================================

cairo_pdf(
  
  filename = "Bacterial_Species_UpSet_GigaScience.pdf",
  
  width = 13,
  
  height = 9.5
  
)

create_upset_plot()

dev.off()

# ============================================================================
# 12. Export TIFF
# ============================================================================

tiff(
  
  filename = "Bacterial_Species_UpSet_GigaScience.tiff",
  
  width = 13,
  
  height = 9.5,
  
  units = "in",
  
  res = 600,
  
  compression = "lzw"
  
)

create_upset_plot()

dev.off()

# ============================================================================
# 13. Export PNG
# ============================================================================

png(
  
  filename = "Bacterial_Species_UpSet_GigaScience.png",
  
  width = 13,
  
  height = 9.5,
  
  units = "in",
  
  res = 600,
  
  bg = "white"
  
)

create_upset_plot()

dev.off()

# ============================================================================
# 14. Compact version
# ============================================================================

png(
  
  filename = "Bacterial_Species_UpSet_GigaScience_Compact.png",
  
  width = 9,
  
  height = 7,
  
  units = "in",
  
  res = 600,
  
  bg = "white"
  
)

create_upset_plot(
  
  text_scale = c(1.9, 1.5, 1.8, 1.5, 1.7, 1.6),
  
  point_size = 5,
  
  line_size = 1.6
  
)

dev.off()

# ============================================================================
# 15. Grayscale version
# ============================================================================

cairo_pdf(
  
  filename = "Bacterial_Species_UpSet_GigaScience_Gray.pdf",
  
  width = 13,
  
  height = 9.5
  
)

upset(
  
  upset_data_final,
  
  nsets = ncol(upset_data_final),
  
  order.by = "freq",
  
  decreasing = TRUE,
  
  keep.order = TRUE,
  
  mb.ratio = c(0.58, 0.42),
  
  text.scale = c(2.4, 1.9, 2.2, 1.8, 2.0, 1.9),
  
  point.size = 6,
  
  line.size = 2,
  
  number.angles = 0,
  
  matrix.dot.alpha = 1,
  
  main.bar.color = "#1F1F1F",
  
  sets.bar.color = "#666666",
  
  matrix.color = "#1F1F1F",
  
  shade.color = "#E6E6E6",
  
  shade.alpha = 0.7,
  
  mainbar.y.label = "Intersection Size (Species)",
  
  sets.x.label = "Total Species Per Tool"
  
)

dev.off()

# ============================================================================
# 16. Terminal report
# ============================================================================

cat("\n")
cat(paste(rep("=", 85), collapse = ""))
cat("\n")

cat("✅ Publication-ready UpSet plot generated successfully!\n\n")

cat("Filtering report:\n")

cat("- Original species count: ",
    nrow(upset_data_raw), "\n")

cat("- Remaining species count: ",
    nrow(upset_data_final), "\n")

cat("- Removed species count: ",
    removed_species_count, "\n")

cat("- Remaining intersections (>=5 species): ",
    remaining_intersections, "\n\n")

cat("Generated files:\n")

cat("1. Bacterial_Species_UpSet_GigaScience.pdf\n")
cat("2. Bacterial_Species_UpSet_GigaScience.tiff\n")
cat("3. Bacterial_Species_UpSet_GigaScience.png\n")
cat("4. Bacterial_Species_UpSet_GigaScience_Compact.png\n")
cat("5. Bacterial_Species_UpSet_GigaScience_Gray.pdf\n\n")

cat("Recommended submission file:\n")

cat("-> Bacterial_Species_UpSet_GigaScience.pdf\n\n")

cat(paste(rep("=", 85), collapse = ""))
cat("\n")