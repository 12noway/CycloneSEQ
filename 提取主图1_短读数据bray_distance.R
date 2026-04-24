pacman::p_load(tidyverse, vegan, ggpubr)

# 1. 距离计算
df_bray_raw <- read.csv("../表格/提取主图1_短读数据bray_distance.csv")
samples_bray <- unique(df_bray_raw$sample)

# 计算 Within (CP vs CZ)
paired_b <- map_dbl(samples_bray, function(s) {
  df_s <- df_bray_raw %>% filter(sample == s) %>% group_by(specie) %>% 
    summarise(cp = sum(cp_abundance), cz = sum(cz_abundance), .groups="drop") %>%
    mutate(cp = cp/sum(cp), cz = cz/sum(cz))
  as.numeric(vegdist(t(as.matrix(df_s[, c("cp", "cz")])), method = "bray"))
})

# 计算 Between (各组内部人与人的差异)
calc_bet <- function(prefix) {
  mat <- df_bray_raw %>% group_by(sample, specie) %>% 
    summarise(val = sum(get(paste0(tolower(prefix), "_abundance"))), .groups="drop") %>%
    pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>% column_to_rownames("sample")
  as.vector(vegdist(mat/rowSums(mat), method = "bray"))
}

# 2. 数据整合
df_bray_plot <- data.frame(
  Distance = c(paired_b, calc_bet("CP"), calc_bet("CZ")),
  Group = factor(c(rep("Within\n(CP vs CZ)", length(paired_b)), 
                   rep("Between\n(CP)", length(calc_bet("CP"))), 
                   rep("Between\n(CZ)", length(calc_bet("CZ")))), 
                 levels = c("Within\n(CP vs CZ)", "Between\n(CP)", "Between\n(CZ)"))
)

# 3. 绘图执行
p_i <- ggplot(df_bray_plot, aes(x = Group, y = Distance, fill = Group)) +
  geom_boxplot(width = 0.4, color = "black", alpha = 0.7, outlier.shape = NA, linewidth = 0.8) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.5, color = "black") +
  stat_compare_means(method = "wilcox.test", comparisons = list(c(1,2), c(2,3), c(1,3)), 
                     label = "p.signif", step_increase = 0.12) +
  scale_fill_manual(values = c("Within\n(CP vs CZ)" = "#969696", "Between\n(CP)" = "#08519C", "Between\n(CZ)" = "#EF6548")) +
  labs(y = "Bray-Curtis Dissimilarity", x = NULL, tag = "I") +
  theme_pubr() + 
  theme(legend.position = "none", axis.text.x = element_text(size = 10, face = "bold"))

ggsave("Panel_4_Bray_Modern.pdf", p_i, width = 6, height = 6, device = cairo_pdf)