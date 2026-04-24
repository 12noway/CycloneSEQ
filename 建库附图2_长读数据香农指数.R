# 1. 加载必要的库
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr) # 用于统计检验和星号标注

# 2. 读取并预处理数据
# 确保路径正确，读取你上传的 csv
data <- read.csv("../表格/建库附图2_建库长读数据sourmash香农指数.csv")

# 数据清洗：宽表转长表，并重命名因子水平
data_long <- data %>%
  pivot_longer(cols = c("without_pretreatment_shannon", "with_pretreatment_shannon"),
               names_to = "Group",
               values_to = "Shannon") %>%
  mutate(Group = factor(Group, 
                        levels = c("without_pretreatment_shannon", "with_pretreatment_shannon"),
                        labels = c("Without Pretreatment", "Pretreatment")))

# 3. 定义核心视觉参数 (Core Aesthetic)
# CP/without 对应冷色调，CZ/pretreatment 对应暖色调
colors_border <- c("Without Pretreatment" = "#2166AC", "Pretreatment" = "#B2182B")
colors_fill   <- c("Without Pretreatment" = "#4393C3", "Pretreatment" = "#D6604D")

# 4. 开始绘图
p <- ggplot(data_long, aes(x = Group, y = Shannon, fill = Group, color = Group)) +
  # A. 配对连线 (Paired Lines)：展示 15 个样本的变化趋势
  # 使用原始 CSV 中的 sample 列作为配对依据
  geom_line(aes(group = sample), color = "grey88", size = 0.5) +
  
  # B. 箱线图 (Boxplot)
  geom_boxplot(width = 0.4, 
               size = 1, 
               outlier.shape = NA, # 异常值由 jitter 点呈现，此处隐藏
               alpha = 0.7) +
  
  # C. 原始数据散点 (Jittered points)
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  
  # D. 显著性标注 (Significance)
  # 使用配对 T 检验 (paired = TRUE)，符合同一批样本处理前后的逻辑
  stat_compare_means(comparisons = list(c("Without Pretreatment", "Pretreatment")),
                     method = "t.test", 
                     paired = TRUE,
                     label = "p.signif", # 自动显示 ***, **, * 或 ns
                     label.y = max(data_long$Shannon) * 1.1,
                     bracket.size = 0.8,
                     tip.length = 0.02,
                     color = "black") +
  
  # E. 颜色与坐标轴设置
  scale_color_manual(values = colors_border) +
  scale_fill_manual(values = colors_fill) +
  labs(x = NULL, y = "sourmash Shannon Index") +
  
  # F. 深度定制主题 (符合 Nature 导出要求)
  theme_classic() + 
  theme(
    text = element_text(family = "Arial"),
    axis.line = element_line(size = 0.8, color = "black"),
    axis.ticks = element_line(size = 0.8, color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 10)),
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "none" # 轴标签已足够清晰，无需图例
  ) +
  
  # G. 调整 Y 轴范围，防止显著性标注超出画布
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

# 5. 渲染预览
print(p)

# 6. 导出 PDF (Vector Graphics)
# cairo_pdf 渲染能确保 Arial 字体和半透明效果在 AI 或 PDF 中不乱码
ggsave("Shannon_Index_Comparison.pdf", 
       plot = p, 
       device = cairo_pdf, 
       width = 4.5, 
       height = 5.5, 
       units = "in")