# 1. 加载必要的包
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)

# 2. 数据准备
data <- data.frame(
  Sample = c("8373", "4143", "0942", "8366", "8915"),
  flye = c(167.3, 128.75, 221.16, 273.98, 173.31),
  metaMDBG = c(156.3, 120.17, 320.32, 382.67, 207.53),
  myloasm = c(215.01, 151.84, 291.65, 792.39, 328.32)
)

data_long <- data %>%
  pivot_longer(cols = c(flye, metaMDBG, myloasm), 
               names_to = "Method", 
               values_to = "N50")

data_long$Sample <- factor(data_long$Sample, levels = data$Sample)
data_long$Method <- factor(data_long$Method, levels = c("flye", "metaMDBG", "myloasm"))

# 3. 定义 Nature 风格配色
nature_cols <- c("flye" = "#2166AC", "metaMDBG" = "#B2182B", "myloasm" = "#1A9850")

# 4. 绘图
p_n50 <- ggplot(data_long, aes(x = Sample, y = N50, group = Method, color = Method)) +
  # 绘制折线
  geom_line(linewidth = 1, alpha = 0.8) +
  # 绘制点
  geom_point(aes(fill = Method), size = 3, shape = 21, color = "black", stroke = 0.5) +
  
  # 数据标签
  geom_text_repel(
    aes(label = round(N50, 0)),
    size = 3.5,
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.3,
    show.legend = FALSE
  ) +
  
  # 颜色映射：在 scale 中直接定义 name，确保 color 和 fill 的图例合并
  scale_color_manual(name = "Assembly Method", values = nature_cols) +
  scale_fill_manual(name = "Assembly Method", values = nature_cols) +
  
  # Y 轴优化
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, NA)
  ) +
  
  # 标签设定：x 轴和 y 轴标签
  labs(
    x = "Metagenome Samples",
    y = "N50 (kb)"
  ) +
  
  # 5. Nature/Cell 风格主题定制
  theme_classic() + 
  theme(
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.title = element_text(size = 16, face = "bold", color = "black"),
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    # 图例设置
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11, face = "bold"),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# 6. 打印与保存
print(p_n50)

ggsave("N50_Comparison_Nature_Style_Final.pdf", 
       plot = p_n50, 
       width = 8, height = 6, 
       device = cairo_pdf)