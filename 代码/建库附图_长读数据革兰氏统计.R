# ==========================================
# 1. 加载必要包
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, grDevices)

# ==========================================
# 2. 数据录入与清洗 (更新为 5 样本数据)
# ==========================================
df_raw <- data.frame(
  sample_id = c("942", "4143", "8366", "8373", "8915"),
  cz_pos = c(61, 35, 86, 86, 79),
  cz_neg = c(51, 15, 51, 51, 60),
  cp_pos = c(139, 128, 147, 148, 139),
  cp_neg = c(92, 92, 80, 96, 90)
)

df_plot <- df_raw %>%
  pivot_longer(cols = -sample_id, names_to = "Group", values_to = "Value") %>%
  separate(Group, into = c("Method", "Stain"), sep = "_") %>%
  mutate(
    # Without 在左，With 在右
    Method = factor(Method, levels = c("cz", "cp"), labels = c("Without", "With")),
    Stain = factor(Stain, levels = c("pos", "neg"), labels = c("Gram-positive", "Gram-negative")),
    # 设置 4 个箱线图的精确横轴位置
    x_pos = case_when(
      Stain == "Gram-positive" & Method == "Without" ~ 1,
      Stain == "Gram-positive" & Method == "With" ~ 1.4,
      Stain == "Gram-negative" & Method == "Without" ~ 2.4,
      Stain == "Gram-negative" & Method == "With" ~ 2.8
    )
  )

# ==========================================
# 3. 统计检验 (Paired Wilcoxon)
# ==========================================
stat_res <- df_plot %>%
  group_by(Stain) %>%
  summarise(
    # 对配对数据进行检验
    p_val = wilcox.test(Value[Method == "With"], Value[Method == "Without"], paired = TRUE)$p.value,
    y_top = max(Value),
    .groups = 'drop'
  ) %>%
  mutate(
    signif = case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**", p_val < 0.05 ~ "*", TRUE ~ "ns"),
    x_start = c(1, 2.4),
    x_end = c(1.4, 2.8)
  )

# ==========================================
# 4. 绘图
# ==========================================
color_border <- c("With" = "#2166AC", "Without" = "#B2182B")
color_fill   <- c("With" = "#4393C3", "Without" = "#D6604D")

p_final <- ggplot(df_plot, aes(x = x_pos, y = Value)) +
  # 1. 背景配对连线
  geom_line(aes(group = interaction(sample_id, Stain)),
            color = "grey88", linewidth = 0.5) +
  
  # 2. 箱线图
  geom_boxplot(aes(color = Method, group = x_pos),
               fill = "white", width = 0.25, linewidth = 1, outlier.shape = NA) +
  
  # 3. 叠加散点
  geom_point(aes(fill = Method, color = Method),
             shape = 21, size = 3, stroke = 0.8, alpha = 0.8) +
  
  # 4. 显著性标注 (横杆 + 星号)
  geom_segment(data = stat_res, aes(x = x_start, xend = x_end, y = y_top + 10, yend = y_top + 10),
               linewidth = 0.8, color = "black") +
  geom_text(data = stat_res, aes(x = (x_start + x_end)/2, y = y_top + 15, label = signif),
            size = 6, fontface = "bold", family = "sans") +
  
  # 5. 坐标轴与比例尺
  scale_x_continuous(breaks = c(1.2, 2.6), labels = c("Gram-positive", "Gram-negative")) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.05, 0.2))) +
  scale_color_manual(values = color_border) +
  scale_fill_manual(values = color_fill) +
  
  # 6. 主题定制
  theme_classic() +
  theme(
    text = element_text(family = "sans"),
    plot.tag = element_text(size = 22, face = "bold"),
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  ) +
  labs(x = "Bacterial Classification", y = "Bacterial Count", tag = "F")

# ==========================================
# 5. 导出
# ==========================================
ggsave("Gram_Stain_NewData_Nature.pdf",
       p_final, width = 5.5, height = 6, device = cairo_pdf)

message(">>> 绘图完成。Without/With 对比已根据新数据更新。")