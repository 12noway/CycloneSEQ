# ==========================================
# Figure 2: Short-read microbiome summary
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
# 修正版：解决 short-read-bray-distance 中 cp_abundance/cz_abundance 被读取为 character 导致 sum() 报错的问题
# ==========================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, readxl, vegan, tibble, grDevices, permute)

dir.create("output", showWarnings = FALSE)

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

find_first_existing <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) stop(paste0("None of the candidate files exists: ", paste(paths, collapse = ", ")))
  hits[1]
}

excel_file <- find_first_existing(c(
  "Figure3-5.xlsx",
  "./Figure3-5.xlsx",
  "../表格/Figure3-5.xlsx"
))

raw <- read_excel(excel_file, sheet = "figure4-5A-C")

short_raw <- raw %>%
  filter(str_detect(sample, "short-read")) %>%
  mutate(SampleID = str_remove(sample, "-short-read"))

# ------------------------------------------
# Panel A: Gram-positive and Gram-negative species
# Test: paired Wilcoxon signed-rank test
# ------------------------------------------
gram_df <- short_raw %>%
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

gp_label <- get_sig(wilcox.test(short_raw$cp_gram_positive, short_raw$cz_gram_positive, paired = TRUE)$p.value)
gn_label <- get_sig(wilcox.test(short_raw$cp_gram_negative, short_raw$cz_gram_negative, paired = TRUE)$p.value)

y_max <- max(gram_df$Value, na.rm = TRUE)
sig_height <- y_max + 5

p_a <- ggplot(gram_df, aes(x = X, y = Value)) +
  geom_line(aes(group = interaction(SampleID, Stain)), color = "grey90", linewidth = 0.5) +
  geom_boxplot(aes(fill = Method), width = 0.5, color = "black", alpha = 0.7, outlier.shape = NA) +
  geom_point(aes(color = Method), size = 2) +
  scale_x_discrete(labels = c("CP", "CZ", "CP", "CZ")) +
  scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.5, y = y_max + 12, label = "Gram-positive", size = 5, fontface = "bold") +
  annotate("text", x = 3.5, y = y_max + 12, label = "Gram-negative", size = 5, fontface = "bold") +
  annotate("segment", x = 1, xend = 2, y = sig_height, yend = sig_height, linewidth = 0.8) +
  annotate("text", x = 1.5, y = sig_height + 2, label = gp_label, size = 5) +
  annotate("segment", x = 3, xend = 4, y = sig_height, yend = sig_height, linewidth = 0.8) +
  annotate("text", x = 3.5, y = sig_height + 2, label = gn_label, size = 5) +
  labs(x = NULL, y = "Number of Species", tag = "A") +
  base_paired_theme +
  ylim(0, y_max + 15)

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
    geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.5, alpha = 0.75) +
    scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    annotate("segment", x = 1, xend = 2, y = y_sig, yend = y_sig, linewidth = 0.8) +
    annotate("text", x = 1.5, y = y_sig + 0.05 * y_rng, label = sig, size = 5) +
    labs(y = y_lab, x = NULL, tag = tag_lab) +
    base_paired_theme
}

species_df <- short_raw %>%
  select(SampleID, CP = cp_species_number, CZ = cz_species_number) %>%
  pivot_longer(-SampleID, names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("CP", "CZ")))

shannon_df <- short_raw %>%
  select(SampleID, CP = cp_shannon, CZ = cz_shannon) %>%
  pivot_longer(-SampleID, names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("CP", "CZ")))

p_b <- paired_box_plot(species_df, "Number of Species", "B")
p_c <- paired_box_plot(shannon_df, "Shannon Index", "C")

# ------------------------------------------
# Panel D: Bray-Curtis dissimilarity from short-read-bray-distance
# Tests: Wilcoxon rank-sum tests
# 修正重点：cp_abundance / cz_abundance 转为 numeric
# ------------------------------------------
bray_data <- read_excel(excel_file, sheet = "short-read-bray-distance") %>%
  mutate(
    cp_abundance = suppressWarnings(as.numeric(cp_abundance)),
    cz_abundance = suppressWarnings(as.numeric(cz_abundance))
  ) %>%
  filter(
    !is.na(sample),
    !is.na(specie),
    !is.na(cp_abundance),
    !is.na(cz_abundance)
  )

samples <- unique(bray_data$sample)

paired_dist <- map_dbl(samples, function(s) {
  df_s <- bray_data %>%
    filter(sample == s) %>%
    group_by(specie) %>%
    summarise(
      cp = sum(cp_abundance, na.rm = TRUE),
      cz = sum(cz_abundance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cp = cp / sum(cp, na.rm = TRUE),
      cz = cz / sum(cz, na.rm = TRUE)
    )
  
  mat <- t(as.matrix(df_s[, c("cp", "cz")]))
  as.numeric(vegdist(mat, method = "bray"))
})

cp_mat <- bray_data %>%
  group_by(sample, specie) %>%
  summarise(
    val = sum(cp_abundance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = specie,
    values_from = val,
    values_fill = 0
  ) %>%
  column_to_rownames("sample")

cp_mat <- as.matrix(cp_mat)
cp_mat <- cp_mat / rowSums(cp_mat)
cp_dist <- as.vector(vegdist(cp_mat, method = "bray"))

cz_mat <- bray_data %>%
  group_by(sample, specie) %>%
  summarise(
    val = sum(cz_abundance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = specie,
    values_from = val,
    values_fill = 0
  ) %>%
  column_to_rownames("sample")

cz_mat <- as.matrix(cz_mat)
cz_mat <- cz_mat / rowSums(cz_mat)
cz_dist <- as.vector(vegdist(cz_mat, method = "bray"))

# ------------------------------------------
# PERMANOVA on short-read full profile matrix
# 15 donors x 2 methods = 30 profiles
# ------------------------------------------
cp_profiles <- bray_data %>%
  group_by(sample, specie) %>%
  summarise(val = sum(cp_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
  mutate(sample_id = paste0(sample, "_CP"), donor = sample, method = "CP") %>%
  select(sample_id, donor, method, everything(), -sample)

cz_profiles <- bray_data %>%
  group_by(sample, specie) %>%
  summarise(val = sum(cz_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
  mutate(sample_id = paste0(sample, "_CZ"), donor = sample, method = "CZ") %>%
  select(sample_id, donor, method, everything(), -sample)

all_profiles <- bind_rows(cp_profiles, cz_profiles)
meta <- all_profiles %>%
  select(sample_id, donor, method) %>%
  mutate(
    donor = factor(donor),
    method = factor(method, levels = c("CP", "CZ"))
  )

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

sink("output/Figure4_short_read_PERMANOVA_from_figure4-5A-C.txt")
cat("PERMANOVA analysis of short-read community profiles (Bray-Curtis)
")
cat("===========================================================

")
cat("Data: 30 profiles (15 donors x 2 extraction methods: CP and CZ)

")
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
sink()

cat("
--- PERMANOVA on Figure 4 short-read Bray-Curtis profiles ---
")
print(permanova_method)
print(permanova_both)

bray_df <- tibble(
  Distance = c(paired_dist, cp_dist, cz_dist),
  Group = factor(
    c(
      rep("Within", length(paired_dist)),
      rep("Between_CP", length(cp_dist)),
      rep("Between_CZ", length(cz_dist))
    ),
    levels = c("Within", "Between_CP", "Between_CZ")
  )
)

lab1 <- get_sig(wilcox.test(paired_dist, cp_dist)$p.value)
lab2 <- get_sig(wilcox.test(cp_dist, cz_dist)$p.value)
lab3 <- get_sig(wilcox.test(paired_dist, cz_dist)$p.value)

y_max <- max(bray_df$Distance, na.rm = TRUE)

p_d <- ggplot(bray_df, aes(x = Group, y = Distance)) +
  geom_boxplot(aes(color = Group), fill = "white", width = 0.5, linewidth = 1, outlier.shape = NA) +
  geom_jitter(aes(fill = Group, color = Group), width = 0.15, size = 2.5, shape = 21, stroke = 0.6, alpha = 0.7) +
  scale_color_manual(values = c("Within" = "#636363", "Between_CP" = "#2166AC", "Between_CZ" = "#B2182B")) +
  scale_fill_manual(values = c("Within" = "#D1D1D1", "Between_CP" = "#4393C3", "Between_CZ" = "#D6604D")) +
  scale_x_discrete(labels = c("Within\n(CP vs CZ)", "Between\n(CP)", "Between\n(CZ)")) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.05, yend = y_max + 0.05, linewidth = 0.8) +
  annotate("text", x = 1.5, y = y_max + 0.06, label = lab1, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.10, yend = y_max + 0.10, linewidth = 0.8) +
  annotate("text", x = 2.5, y = y_max + 0.11, label = lab2, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.15, yend = y_max + 0.15, linewidth = 0.8) +
  annotate("text", x = 2, y = y_max + 0.16, label = lab3, size = 5) +
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
  ylim(0, y_max + 0.2)

# ------------------------------------------
# 2 x 2 layout
# A | B
# C | D
# ------------------------------------------
fig2 <- ggarrange(
  p_a, p_b,
  p_c, p_d,
  ncol = 2,
  nrow = 2,
  widths = c(1, 1),
  heights = c(1, 1),
  align = "hv"
)

save_fig(fig2, "output/Figure4_Short_Read_from_figure4-5A-C", width = 14, height = 10)
cat("Done: output/Figure4_Short_Read_from_figure4-5A-C.pdf/.tiff/.png\n")
