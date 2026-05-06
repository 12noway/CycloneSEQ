# ============================================================================
# 污染水平分析 - Nature/Cell 风格密度曲线图 (Refined Version)
# ============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# 1. 数据预处理 (保持逻辑不变，优化因子水平)
data <- read.csv("../表格/组装主图_组装污染度密度.csv")
colnames(data) <- c("Flye", "MetamDBG", "Myloasm", "Flye_NextPolish", 
                    "MetamDBG_NextPolish", "Myloasm_NextPolish", "HybridSPAdes", "OPERA-MS")

data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Contamination") %>%
  filter(!is.na(Contamination)) %>%
  mutate(
    Group = factor(ifelse(grepl("NextPolish", Method), "After Polish", "Original"),
                   levels = c("Original", "After Polish")),
    Method_Simple = factor(gsub("_NextPolish", "", Method),
                           levels = c("Flye", "MetamDBG", "Myloasm", "HybridSPAdes", "OPERA-MS"))
  )

# 2. 计算中位数用于绘制垂直线
median_data <- data_long %>%
  group_by(Method_Simple, Group) %>%
  summarise(Median = median(Contamination, na.rm = TRUE), .groups = "drop")

# 3. 定义 Nature/Cell 风格配色 (Cold-Warm Contrast)
# Original: 深蓝色系 | After Polish: 深红色系
sci_palette_fill <- c("Original" = "#4393C3", "After Polish" = "#D6604D")
sci_palette_line <- c("Original" = "#2166AC", "After Polish" = "#B2182B")

# 4. 绘图
density_plot <- ggplot(data_long, aes(x = Contamination, fill = Group, color = Group)) +
  # 密度曲线：增加线条宽度与填充透明度
  geom_density(alpha = 0.5, linewidth = 0.8, adjust = 1.2, trim = TRUE) +
  
  # 中位数虚线：强化对比
  geom_vline(data = median_data, aes(xintercept = Median, color = Group),
             linetype = "dashed", linewidth = 0.6) +
  
  # 分面设置
  facet_wrap(~ Method_Simple, ncol = 3, scales = "fixed") +
  
  # 坐标轴与标签
  labs(x = "Contamination Level (%)", y = "Density", fill = "Stage", color = "Stage") +
  
  # 标尺优化
  scale_fill_manual(values = sci_palette_fill) +
  scale_color_manual(values = sci_palette_line) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  # === Nature/Cell 主题风格定制 ===
  theme_classic(base_size = 14) + 
  theme(
    text = element_text(family = "sans", color = "black"),
    
    # 标题与标签：加粗，提升可读性
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12, color = "black"),
    
    # 分面标签：顶部留白，背景透明
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 14, v_just = 1),
    
    # 坐标轴线：精细化
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    
    # 图例：置于底部或右侧，去除边框
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_blank(),
    
    # 面板间距
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# 5. 导出 (采用 cairo_pdf 确保矢量字体兼容性)
ggsave("Fig10_Refined_Density.pdf", density_plot, width = 10, height = 7, device = cairo_pdf)
ggsave("Fig10_Refined_Density.png", density_plot, width = 10, height = 7, dpi = 300)

print(density_plot)