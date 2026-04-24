# 1. 加载必要的库
library(ggplot2)
library(ggpubr)
library(tidyr)

# 2. 数据准备
df_wide <- data.frame(
  sample = c(942, 4143, 8366, 8373, 8915),
  without_pretreatment = c(4.297633, 3.430881, 4.842118, 4.824123, 5.086915),
  with_pretreatment = c(5.120135, 5.338203, 5.303962, 5.574334, 5.635038)
)

# 转换为长格式
data_long <- pivot_longer(df_wide, 
                          cols = c("without_pretreatment", "with_pretreatment"),
                          names_to = "treatment", 
                          values_to = "shannon_index")

# 设置因子顺序
data_long$treatment <- factor(data_long$treatment, 
                              levels = c("without_pretreatment", "with_pretreatment"))

# 3. 颜色与位置设置
fill_colors <- c("without_pretreatment" = "#4393C3", "with_pretreatment" = "#D6604D")
line_colors <- c("without_pretreatment" = "#2166AC", "with_pretreatment" = "#B2182B")
y_max <- max(data_long$shannon_index, na.rm = TRUE)

# 4. 绘图主体
p_final <- ggplot(data_long, aes(x = treatment, y = shannon_index)) +
  
  # 配对连线 (确保居中且不位移)
  geom_line(aes(group = sample), 
            color = "grey85", 
            linewidth = 0.5,
            position = position_dodge(0)) + 
  
  # 箱线图 (设置 alpha 半透明以增加层次感)
  geom_boxplot(aes(fill = treatment, color = treatment),
               width = 0.45, 
               linewidth = 0.8, 
               alpha = 0.6, 
               outlier.shape = NA,
               position = position_dodge(0)) + 
  
  # 散点 (使用 shape 21 以便同时设置填充色和边框色)
  geom_point(aes(fill = treatment),
             size = 3.5, 
             alpha = 0.9, 
             shape = 21, 
             stroke = 0.8, 
             color = "black",
             position = position_dodge(0)) +
  
  # 统计检验 (Wilcoxon paired test)
  stat_compare_means(
    method = "wilcox.test",
    paired = TRUE,
    label = "p.signif",
    comparisons = list(c("without_pretreatment", "with_pretreatment")),
    label.y = y_max * 1.08, 
    bracket.size = 0.6,
    size = 5
  ) +
  
  # 映射设置与标签美化
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = line_colors) +
  # 重点：修改 X 轴显示名称，增加换行符 \n 使排版更紧凑
  scale_x_discrete(labels = c("without_pretreatment" = "Without\nPretreatment", 
                              "with_pretreatment" = "With\nPretreatment")) +
  labs(y = "Mp4 Shannon Diversity Index", x = NULL) +
  
  # 主题定制 (SCI 标准)
  theme_classic() +
  theme(
    # 坐标轴线条加粗
    axis.line = element_line(linewidth = 0.8, color = "black"),
    # Y 轴文字设置
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 10)),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    # X 轴文字设置 (恢复显示)
    axis.text.x = element_text(size = 14, face = "bold", color = "black", vjust = 0.5),
    axis.ticks.x = element_line(linewidth = 0.8, color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "none",
    # 增加边距确保标签不被切掉
    plot.margin = margin(10, 10, 10, 10)
  )

# 打印查看预览
print(p_final)

# 5. 高质量导出 (cairo_pdf)
ggsave(
  filename = "Shannon_Index_Paired_Final.pdf", 
  plot = p_final, 
  width = 4.8,           # 宽度略微增加以容纳 X 轴文字
  height = 5.8, 
  units = "in", 
  device = cairo_pdf,
  bg = "white"
)