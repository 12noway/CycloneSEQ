# ==========================================
# 1. 加载核心库
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vegan, ggpubr, grDevices)

# ==========================================
# 2. 数据准备与计算
# ==========================================
# 读取数据
data_raw <- read.csv("../表格/建库附图_建库长读数据bray_distance.csv")

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