# ==========================================
# Figure 3: Long-read microbiome summary
# 2 x 2 layout
# Panels: Gram staining, species number, Shannon index, Bray-Curtis distance
# Style follows 提取主图.R; all labels are in English
# ==========================================
# Statistical tests used:
# Panel A (Gram-positive / Gram-negative): paired Wilcoxon signed-rank test (CP vs CZ)
# Panel B (Species number): paired Wilcoxon signed-rank test (CP vs CZ)
# Panel C (Shannon index): paired Wilcoxon signed-rank test (CP vs CZ)
# Panel D (Bray-Curtis dissimilarity): Wilcoxon rank-sum tests for
#   1) Within vs Between_CP
#   2) Between_CP vs Between_CZ
#   3) Within vs Between_CZ
# Note: Panel D follows the style of the original figure code. If needed, PERMANOVA can be added separately.
# ==========================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, readxl, tibble, grDevices, vegan, permute)

dir.create("output", showWarnings = FALSE)

find_first_existing <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) {
    stop(paste0("None of the candidate files exists: ", paste(paths, collapse = ", ")))
  }
  hits[1]
}

excel_file <- find_first_existing(c(
  "Figure3-5.xlsx",
  "./Figure3-5.xlsx",
  "../表格/Figure3-5.xlsx"
))

get_sig <- function(p) {
  if (is.na(p)) return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
}

save_fig <- function(plot_obj, filename_base, width, height, dpi = 300) {
  ggsave(paste0(filename_base, ".pdf"), plot_obj, width = width, height = height, device = cairo_pdf)
  ggsave(paste0(filename_base, ".tiff"), plot_obj, width = width, height = height,
         device = "tiff", dpi = dpi, compression = "lzw", type = "cairo", bg = "white")
  ggsave(paste0(filename_base, ".png"), plot_obj, width = width, height = height,
         dpi = dpi, bg = "white")
}

paired_sig <- function(df, value_col = "Value", method_col = "Method", sample_col = "SampleID") {
  wide <- df %>%
    select(all_of(c(sample_col, method_col, value_col))) %>%
    pivot_wider(names_from = all_of(method_col), values_from = all_of(value_col))
  tryCatch(
    get_sig(wilcox.test(wide$CP, wide$CZ, paired = TRUE)$p.value),
    error = function(e) "ns"
  )
}

base_paired_theme <- theme_pubr() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 12, family = "sans"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
    axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
    plot.margin = margin(5, 5, 5, 5)
  )

raw <- read_excel(excel_file, sheet = "figure4-5A-C")

long_raw <- raw %>%
  filter(str_detect(sample, "long-read")) %>%
  mutate(SampleID = str_remove(sample, "-long-read"))

# ------------------------------------------
# Panel A: Gram-positive and Gram-negative species
# Test: paired Wilcoxon signed-rank test
# ------------------------------------------
gram_df <- long_raw %>%
  select(
    SampleID,
    cp_pos = cp_gram_positive,
    cp_neg = cp_gram_negative,
    cz_pos = cz_gram_positive,
    cz_neg = cz_gram_negative
  ) %>%
  pivot_longer(-SampleID, names_to = "Group", values_to = "Value") %>%
  separate(Group, into = c("Method", "Stain"), sep = "_") %>%
  mutate(
    Method = toupper(Method),
    Stain = recode(Stain, pos = "Gram-positive", neg = "Gram-negative"),
    Stain = factor(Stain, levels = c("Gram-positive", "Gram-negative")),
    X = interaction(Stain, Method, sep = "_"),
    X = factor(
      X,
      levels = c(
        "Gram-positive_CP",
        "Gram-positive_CZ",
        "Gram-negative_CP",
        "Gram-negative_CZ"
      )
    )
  )

gp_label <- get_sig(wilcox.test(long_raw$cp_gram_positive, long_raw$cz_gram_positive, paired = TRUE)$p.value)
gn_label <- get_sig(wilcox.test(long_raw$cp_gram_negative, long_raw$cz_gram_negative, paired = TRUE)$p.value)

y_max <- max(gram_df$Value, na.rm = TRUE)
sig_height <- y_max + 10

p_a <- ggplot(gram_df, aes(x = X, y = Value)) +
  geom_line(aes(group = interaction(SampleID, Stain)), color = "grey90", linewidth = 0.5) +
  geom_boxplot(aes(fill = Method), width = 0.5, color = "black", alpha = 0.7, outlier.shape = NA) +
  geom_point(aes(color = Method), size = 2.5) +
  scale_x_discrete(labels = c("CP", "CZ", "CP", "CZ")) +
  scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.5, y = y_max + 25, label = "Gram-positive", size = 5, fontface = "bold") +
  annotate("text", x = 3.5, y = y_max + 25, label = "Gram-negative", size = 5, fontface = "bold") +
  annotate("segment", x = 1, xend = 2, y = sig_height, yend = sig_height, linewidth = 0.8) +
  annotate("text", x = 1.5, y = sig_height + 3, label = gp_label, size = 5) +
  annotate("segment", x = 3, xend = 4, y = sig_height, yend = sig_height, linewidth = 0.8) +
  annotate("text", x = 3.5, y = sig_height + 3, label = gn_label, size = 5) +
  labs(x = NULL, y = "Number of Species", tag = "A") +
  base_paired_theme +
  ylim(0, y_max + 30)

# ------------------------------------------
# Common paired plot function for species number and Shannon index
# Test: paired Wilcoxon signed-rank test
# ------------------------------------------
paired_box_plot <- function(df, y_lab, tag_lab) {
  sig <- paired_sig(df)
  y_max <- max(df$Value, na.rm = TRUE)
  y_min <- min(df$Value, na.rm = TRUE)
  y_rng <- ifelse(y_max == y_min, y_max, y_max - y_min)
  y_sig <- y_max + 0.1 * y_rng
  
  ggplot(df, aes(x = Method, y = Value, fill = Method)) +
    geom_line(aes(group = SampleID), color = "grey85", linewidth = 0.5) +
    geom_boxplot(width = 0.4, color = "black", alpha = 0.7, outlier.shape = NA, linewidth = 0.8) +
    geom_point(shape = 21, size = 3, color = "black", stroke = 0.5, alpha = 0.75) +
    scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    annotate("segment", x = 1, xend = 2, y = y_sig, yend = y_sig, linewidth = 0.8) +
    annotate("text", x = 1.5, y = y_sig + 0.05 * y_rng, label = sig, size = 5) +
    labs(y = y_lab, x = NULL, tag = tag_lab) +
    base_paired_theme
}

species_df <- long_raw %>%
  select(SampleID, CP = cp_species_number, CZ = cz_species_number) %>%
  pivot_longer(-SampleID, names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("CP", "CZ")))

shannon_df <- long_raw %>%
  select(SampleID, CP = cp_shannon, CZ = cz_shannon) %>%
  pivot_longer(-SampleID, names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("CP", "CZ")))

p_b <- paired_box_plot(species_df, "Number of Species", "B")
p_c <- paired_box_plot(shannon_df, "Shannon Index", "C")

# ------------------------------------------
# Panel D: Bray-Curtis dissimilarity from long-read-bray-distance
# Tests: Wilcoxon rank-sum tests
# This sheet is stored as text-like blocks:
# rows 2-5: within (CP vs CZ, same sample)
# rows 7-12: between CP
# rows 14-19: between CZ
# ------------------------------------------
bray_raw <- read_excel(excel_file, sheet = "long-read-bray-distance", col_names = FALSE)

parse_last_number <- function(x) {
  as.numeric(str_extract(as.character(x), "[0-9]+\\.[0-9]+$"))
}

within_vals <- parse_last_number(bray_raw[[1]][2:5])
cp_vals <- parse_last_number(bray_raw[[1]][7:12])
cz_vals <- parse_last_number(bray_raw[[1]][14:19])

within_vals <- within_vals[!is.na(within_vals)]
cp_vals <- cp_vals[!is.na(cp_vals)]
cz_vals <- cz_vals[!is.na(cz_vals)]


# ------------------------------------------
# Optional PERMANOVA on long-read extraction-comparison profiles
# Current Excel sheet stores only pairwise distances, so adonis2 cannot be
# computed unless an auxiliary raw abundance table is supplied.
# ------------------------------------------
permanova_status <- paste(
  "PERMANOVA not run:",
  "the current long-read-bray-distance sheet contains only precomputed pairwise distances,",
  "which are insufficient for adonis2."
)
permanova_method <- NULL
permanova_both <- NULL

profile_candidates <- c(
  "Figure5_long_read_bray_profiles.csv",
  "./Figure5_long_read_bray_profiles.csv",
  "long-read-bray-abundance.csv",
  "./long-read-bray-abundance.csv",
  "long_read_extraction_bray_abundance.csv",
  "./long_read_extraction_bray_abundance.csv",
  "../表格/Figure5_long_read_bray_profiles.csv",
  "../表格/long-read-bray-abundance.csv",
  "../表格/long_read_extraction_bray_abundance.csv"
)
profile_hits <- profile_candidates[file.exists(profile_candidates)]

if (length(profile_hits) > 0) {
  profile_file <- profile_hits[1]
  raw_prof <- read.csv(profile_file, check.names = FALSE, stringsAsFactors = FALSE)
  names(raw_prof) <- tolower(gsub("[^a-zA-Z0-9]+", "_", names(raw_prof)))
  
  sample_col <- intersect(c("sample", "donor", "sample_id"), names(raw_prof))[1]
  specie_col <- intersect(c("specie", "species", "bacteria", "taxon"), names(raw_prof))[1]
  cp_col <- intersect(c("cp_abundance", "cp", "cp_relative_abundance", "cp_rel_abundance"), names(raw_prof))[1]
  cz_col <- intersect(c("cz_abundance", "cz", "cz_relative_abundance", "cz_rel_abundance"), names(raw_prof))[1]
  
  if (all(!is.na(c(sample_col, specie_col, cp_col, cz_col)))) {
    prof_data <- raw_prof %>%
      transmute(
        sample = as.character(.data[[sample_col]]),
        specie = as.character(.data[[specie_col]]),
        cp_abundance = suppressWarnings(as.numeric(.data[[cp_col]])),
        cz_abundance = suppressWarnings(as.numeric(.data[[cz_col]]))
      ) %>%
      filter(!is.na(sample), !is.na(specie), !is.na(cp_abundance), !is.na(cz_abundance))
    
    if (nrow(prof_data) > 0) {
      cp_profiles <- prof_data %>%
        group_by(sample, specie) %>%
        summarise(val = sum(cp_abundance, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
        mutate(sample_id = paste0(sample, "_CP"), donor = sample, method = "CP") %>%
        select(sample_id, donor, method, everything(), -sample)
      
      cz_profiles <- prof_data %>%
        group_by(sample, specie) %>%
        summarise(val = sum(cz_abundance, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
        mutate(sample_id = paste0(sample, "_CZ"), donor = sample, method = "CZ") %>%
        select(sample_id, donor, method, everything(), -sample)
      
      all_profiles <- bind_rows(cp_profiles, cz_profiles)
      meta <- all_profiles %>%
        select(sample_id, donor, method) %>%
        mutate(donor = factor(donor), method = factor(method, levels = c("CP", "CZ")))
      
      profile_mat <- all_profiles %>%
        select(-sample_id, -donor, -method) %>%
        as.matrix()
      
      row_sums <- rowSums(profile_mat)
      row_sums[row_sums == 0] <- 1
      profile_mat <- profile_mat / row_sums
      profile_mat[is.nan(profile_mat)] <- 0
      
      set.seed(42)
      perm_ctrl <- permute::how(nperm = 999)
      permute::setBlocks(perm_ctrl) <- with(meta, donor)
      permanova_method <- adonis2(
        profile_mat ~ method,
        data = meta,
        method = "bray",
        permutations = perm_ctrl
      )
      
      set.seed(42)
      permanova_both <- adonis2(
        profile_mat ~ donor + method,
        data = meta,
        method = "bray",
        permutations = 999,
        by = "margin"
      )
      
      permanova_status <- paste0("PERMANOVA run successfully from auxiliary profile table: ", profile_file)
    } else {
      permanova_status <- paste0("PERMANOVA not run: auxiliary file was found but no usable rows remained after filtering: ", profile_file)
    }
  } else {
    permanova_status <- paste0("PERMANOVA not run: could not map required columns in auxiliary file: ", profile_file)
  }
}

sink("output/Figure3_Long_Read_PERMANOVA.txt")
cat("PERMANOVA analysis of long-read community profiles (Bray-Curtis)
")
cat("==========================================================

")
cat(permanova_status, "

")
if (!is.null(permanova_method)) {
  cat("Test 1: method effect, permutations blocked within donor (999 perms)
")
  cat("--------------------------------------------------------------
")
  print(permanova_method)
  cat("

Test 2: donor + method, marginal R^2 (999 unrestricted perms)
")
  cat("--------------------------------------------------------------
")
  print(permanova_both)
} else {
  cat("No PERMANOVA table was produced.
")
}
sink()

cat("
--- Long-read PERMANOVA status ---
")
cat(permanova_status, "
")
if (!is.null(permanova_method)) {
  print(permanova_method)
  print(permanova_both)
}

bray_df <- tibble(
  Distance = c(within_vals, cp_vals, cz_vals),
  Group = factor(
    c(
      rep("Within", length(within_vals)),
      rep("Between_CP", length(cp_vals)),
      rep("Between_CZ", length(cz_vals))
    ),
    levels = c("Within", "Between_CP", "Between_CZ")
  )
)

lab1 <- get_sig(tryCatch(wilcox.test(within_vals, cp_vals)$p.value, error = function(e) NA))
lab2 <- get_sig(tryCatch(wilcox.test(cp_vals, cz_vals)$p.value, error = function(e) NA))
lab3 <- get_sig(tryCatch(wilcox.test(within_vals, cz_vals)$p.value, error = function(e) NA))

y_max <- max(bray_df$Distance, na.rm = TRUE)

p_d <- ggplot(bray_df, aes(x = Group, y = Distance)) +
  geom_boxplot(aes(color = Group), fill = "white", width = 0.5, linewidth = 1, outlier.shape = NA) +
  geom_jitter(aes(fill = Group, color = Group), width = 0.15, size = 3, shape = 21, stroke = 0.6, alpha = 0.7) +
  scale_color_manual(values = c("Within" = "#636363", "Between_CP" = "#2166AC", "Between_CZ" = "#B2182B")) +
  scale_fill_manual(values = c("Within" = "#D1D1D1", "Between_CP" = "#4393C3", "Between_CZ" = "#D6604D")) +
  scale_x_discrete(labels = c("Within\n(CP vs CZ)", "Between\n(CP)", "Between\n(CZ)")) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.04, yend = y_max + 0.04, linewidth = 0.8) +
  annotate("text", x = 1.5, y = y_max + 0.05, label = lab1, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.08, yend = y_max + 0.08, linewidth = 0.8) +
  annotate("text", x = 2.5, y = y_max + 0.09, label = lab2, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.12, yend = y_max + 0.12, linewidth = 0.8) +
  annotate("text", x = 2, y = y_max + 0.13, label = lab3, size = 5) +
  labs(y = "Bray-Curtis Dissimilarity", x = NULL, tag = "D") +
  theme_classic() +
  theme(
    plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
    axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
    axis.title.y = element_text(size = 12, face = "bold", family = "sans"),
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ylim(0, y_max + 0.16)

# ------------------------------------------
# 2 x 2 layout
# A | B
# C | D
# ------------------------------------------
fig3 <- ggarrange(
  p_a, p_b,
  p_c, p_d,
  ncol = 2,
  nrow = 2,
  widths = c(1, 1),
  heights = c(1, 1),
  align = "hv"
)

save_fig(fig3, "output/Figure5_Long_Read", width = 14, height = 10)
cat("Done: output/Figure5_Long_Read.pdf/.tiff/.png\n")

