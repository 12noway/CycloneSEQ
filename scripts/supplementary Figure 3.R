# ==========================================
# 1. 加载核心库
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vegan, ggpubr, grDevices)

# ==========================================
# 2. 数据准备与计算
# ==========================================
# 读取数据
data_raw <- read.csv("../表格/supplementary Figure 3.csv")

# 统一列名映射
data <- data_raw %>%
  rename(
    sample = sample,
    specie = bacteria, 
    cp_abundance = without_pretreatment_abundance, 
    cz_abundance = with_pretreatment_abundance
  )

samples <- unique(data$sample)

# 1️⃣ 计算 Within sample
paired_dist <- c()
for (s in samples) {
  df_sub <- data %>% 
    filter(sample == s) %>%
    group_by(specie) %>%
    summarise(cp_abundance = sum(cp_abundance, na.rm = TRUE),
              cz_abundance = sum(cz_abundance, na.rm = TRUE), 
              .groups = "drop")
  df_sub <- df_sub %>% mutate(
    cp_abundance = if(sum(cp_abundance) > 0) cp_abundance / sum(cp_abundance) else 0,
    cz_abundance = if(sum(cz_abundance) > 0) cz_abundance / sum(cz_abundance) else 0
  )
  mat <- df_sub[, c("cp_abundance", "cz_abundance")]
  dist_val <- as.numeric(vegdist(t(mat), method = "bray"))
  paired_dist <- c(paired_dist, dist_val)
}

# 2️⃣ 计算 Between samples (without)
cp_wide <- data %>%
  group_by(sample, specie) %>%
  summarise(cp_abundance = sum(cp_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = cp_abundance, values_fill = 0)
cp_mat <- as.matrix(cp_wide[, -1])
cp_mat <- cp_mat / rowSums(cp_mat)
cp_dist <- as.vector(vegdist(cp_mat, method = "bray"))

# 3️⃣ 计算 Between samples (with)
cz_wide <- data %>%
  group_by(sample, specie) %>%
  summarise(cz_abundance = sum(cz_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = cz_abundance, values_fill = 0)
cz_mat <- as.matrix(cz_wide[, -1])
cz_mat <- cz_mat / rowSums(cz_mat)
cz_dist <- as.vector(vegdist(cz_mat, method = "bray"))

# 4️⃣ 合并数据
df_plot <- data.frame(
  Distance = c(paired_dist, cp_dist, cz_dist),
  Group = c(
    rep("Within sample\n(Without vs With)", length(paired_dist)),
    rep("Between samples\n(Without)", length(cp_dist)),
    rep("Between samples\n(With)", length(cz_dist))
  )
)

df_plot$Group <- factor(df_plot$Group, levels = c(
  "Within sample\n(Without vs With)",
  "Between samples\n(Without)",
  "Between samples\n(With)"
))

# ==========================================
# 3. 绘图 (Nature/Cell SCI 风格 - 三色对比版)
# ==========================================

# 🔥 重新定义三组不同的颜色
# 顺序：Within (灰黑), Between-Without (红/暖), Between-With (蓝/冷)
color_fill   <- c("#7F7F7F", "#D6604D", "#4393C3") # 填充色
color_border <- c("#333333", "#B2182B", "#2166AC") # 边框色

my_comparisons <- list(
  c("Within sample\n(Without vs With)", "Between samples\n(Without)"),
  c("Between samples\n(Without)", "Between samples\n(With)"),
  c("Within sample\n(Without vs With)", "Between samples\n(With)")
)

p <- ggplot(df_plot, aes(x = Group, y = Distance, fill = Group, color = Group)) +
  # 1. 箱线图
  geom_boxplot(
    width = 0.5, 
    linewidth = 1.2, 
    outlier.shape = NA, 
    alpha = 0.6
  ) +
  
  # 2. 抖动散点：保持白心，边框颜色随组别变化
  geom_jitter(
    shape = 21, 
    width = 0.15, 
    size = 2.5, 
    stroke = 1.2, 
    fill = "white", 
    alpha = 0.8
  ) +
  
  # 3. 统计显著性 (Wilcoxon)
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test", 
    label = "p.signif",
    step.increase = 0.12,
    bracket.size = 0.8,
    tip.length = 0.02,
    size = 6,
    color = "black"
  ) +
  
  # 4. 色彩映射
  scale_fill_manual(values = color_fill) +
  scale_color_manual(values = color_border) +
  scale_y_continuous(
    limits = c(0, 1.5), # 增加上限，防止括号重叠
    expand = c(0, 0), 
    breaks = seq(0, 1, 0.2)
  ) +
  
  # 5. 主题精修
  labs(
    x = NULL, y = "Bray-Curtis Distance"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.tag = element_text(size = 24, face = "bold"),
    axis.line = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1.2, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "none",
    plot.margin = margin(25, 25, 25, 25)
  )

print(p)

# ==========================================
# 4. 导出高质量 PDF
# ==========================================
ggsave("Bray_Distance_ThreeColor_Style.pdf", 
       p, width = 7.5, height = 7, device = cairo_pdf)

message(">>> 绘图完成。三色对比样式已应用。")


# ==========================================
# 5. PERMANOVA 分析（Supp Fig 6 兜底，与 Figure 2I 呼应）
#    回应审稿人可能提出的"距离值不独立"问题；同时给出
#    donor / protocol 的 R^2 定量分解。
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(vegan, dplyr, tidyr)

message("\n--- PERMANOVA on Supp Fig 6 (Bray-Curtis, sourmash profiles) ---")

# 5.1 构造 10 行 profile 矩阵：5 donors × 2 protocols
# 注意：本脚本前面已经把列名映射为
#   cp_abundance = without_pretreatment_abundance
#   cz_abundance = with_pretreatment_abundance
# 这里保持一致

without_profiles <- data %>%
  group_by(sample, specie) %>%
  summarise(val = sum(cp_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
  mutate(sample_id = paste0(sample, "_Without"),
         donor = sample,
         protocol = "Without") %>%
  select(sample_id, donor, protocol, everything(), -sample)

with_profiles <- data %>%
  group_by(sample, specie) %>%
  summarise(val = sum(cz_abundance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
  mutate(sample_id = paste0(sample, "_With"),
         donor = sample,
         protocol = "With") %>%
  select(sample_id, donor, protocol, everything(), -sample)

all_profiles <- bind_rows(without_profiles, with_profiles)

meta <- all_profiles %>% select(sample_id, donor, protocol)

# 关键：donor 是分类变量（5 个不同个体），不是连续变量。
# 原始 CSV 里 sample 列是数字（942/4143/…），R 会误当成 continuous slope，
# 只算 1 df，严重低估 donor 的方差贡献。这里显式转成 factor。
meta$donor    <- factor(meta$donor)
meta$protocol <- factor(meta$protocol, levels = c("Without", "With"))

profile_mat <- all_profiles %>%
  select(-sample_id, -donor, -protocol) %>%
  as.matrix()

# 行内相对丰度归一化（防止总丰度不同影响距离）
row_sums <- rowSums(profile_mat)
row_sums[row_sums == 0] <- 1  # 防止除以 0
profile_mat <- profile_mat / row_sums
profile_mat[is.nan(profile_mat)] <- 0

# 5.2 Test 1: protocol 效应，donor 内置换（block permutation）
set.seed(42)
perm_ctrl <- how(nperm = 999)
setBlocks(perm_ctrl) <- with(meta, donor)  # 在同一 donor 内置换 protocol 标签

permanova_protocol <- adonis2(
  profile_mat ~ protocol,
  data = meta,
  method = "bray",
  permutations = perm_ctrl
)
message("\n[PERMANOVA] protocol effect (permutations blocked by donor):")
print(permanova_protocol)

# 5.3 Test 2: donor + protocol 各自贡献了多少方差（边际 R^2）
set.seed(42)
permanova_both <- adonis2(
  profile_mat ~ donor + protocol,
  data = meta,
  method = "bray",
  permutations = 999,
  by = "margin"
)
message("\n[PERMANOVA] donor + protocol (marginal R^2, 999 unrestricted permutations):")
print(permanova_both)

# 5.4 保存 PERMANOVA 结果到文本文件，方便复制到正文 / Response letter
sink("PERMANOVA_SuppFig6_results.txt")
cat("PERMANOVA analysis of sourmash community profiles (Bray-Curtis)\n")
cat("Supp Fig 6: library-preparation comparison (with vs. without pretreatment)\n")
cat("=================================================================\n\n")
cat("Data: 10 profiles (5 donors x 2 library-preparation protocols).\n\n")
cat("Test 1: protocol effect, permutations blocked within donor (999 perms).\n")
cat("--------\n")
print(permanova_protocol)
cat("\n\nTest 2: donor + protocol, marginal R^2 (999 unrestricted perms).\n")
cat("--------\n")
print(permanova_both)
sink()
message("\nPERMANOVA results saved to: PERMANOVA_SuppFig6_results.txt")