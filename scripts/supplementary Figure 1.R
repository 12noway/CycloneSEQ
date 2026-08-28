# ==========================================
# 1. 加载核心库
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, grDevices)

# ==========================================
# 2. 数据准备
# ==========================================
df_total <- data.frame(
  sample_id = factor(1:5),
  CZ = c(160, 61, 240, 242, 309), 
  CP = c(384, 418, 412, 512, 527)
)

df_plot_total <- df_total %>%
  pivot_longer(cols = c(CZ, CP), names_to = "Method", values_to = "Count") %>%
  mutate(
    Method = factor(Method, levels = c("CZ", "CP"), 
                    labels = c("Without Pretreatment", "With Pretreatment"))
  )

# ==========================================
# 3. 统计检验 (Wilcoxon Signed-Rank Test)
# ==========================================
# 使用 wilcox.test 并设置 paired = TRUE
stat_res <- wilcox.test(df_total$CZ, df_total$CP, paired = TRUE)
stat_p_val <- stat_res$p.value

# 根据 P 值自动生成显著性标签
signif_label <- case_when(
  stat_p_val < 0.001 ~ "***", 
  stat_p_val < 0.01  ~ "**", 
  stat_p_val < 0.05  ~ "*", 
  TRUE               ~ "ns"
)

# ==========================================
# 4. 绘图 (Nature/Cell 风格)
# ==========================================
color_border <- c("Without Pretreatment" = "#B2182B", "With Pretreatment" = "#2166AC")
color_fill   <- c("Without Pretreatment" = "#D6604D", "With Pretreatment" = "#4393C3")

p_total <- ggplot(df_plot_total, aes(x = Method, y = Count, fill = Method, color = Method)) +
  # 1. 配对连线
  geom_line(aes(group = sample_id), color = "grey88", linewidth = 0.6) +
  
  # 2. 箱线图
  geom_boxplot(width = 0.4, linewidth = 1, outlier.shape = NA, fill = "white", alpha = 0.7) +
  
  # 3. 原始散点
  geom_point(shape = 21, size = 4, stroke = 1, alpha = 0.9) +
  
  # 4. 显著性标注 (解决之前的报错)
  geom_bracket(
    xmin = "Without Pretreatment", 
    xmax = "With Pretreatment", 
    y.position = 580,
    label = signif_label, 
    tip.length = 0.03,
    color = "black", 
    label.size = 6, 
    fontface = "bold",
    inherit.aes = FALSE 
  ) +
  
  # 5. 样式定制
  scale_fill_manual(values = color_fill) +
  scale_color_manual(values = color_border) +
  scale_y_continuous(limits = c(0, 650), expand = c(0, 0)) +
  labs(x = NULL, y = "Total Bacteria Number") +
  theme_classic() +
  theme(
    text = element_text(family = "sans"),
    plot.tag = element_text(size = 22, face = "bold"),
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 13, face = "bold", color = "black"),
    legend.position = "none"
  )

# 显示图像
print(p_total)

# ==========================================
# 5. 导出 PDF
# ==========================================
ggsave("Total_Bacteria_Wilcoxon_Nature.pdf", 
       p_total, width = 4.5, height = 5.5, device = cairo_pdf)

message(">>> 绘图完成！Wilcoxon 检验 P 值：", format.pval(stat_p_val))