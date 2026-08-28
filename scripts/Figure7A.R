# 安装 showtext（若未安装）
if (!require("showtext")) install.packages("showtext")

library(showtext)
library(ggplot2)
library(tidyr)
library(dplyr)

# ---- 启用 showtext 并添加 Arial ----
showtext_auto()
font_add("Arial", regular = "arial.ttf")  # Windows 下通常无需指定路径

# ---- 读取数据 ----
df <- read.csv("../表格/Figure 7A.csv", check.names = FALSE)
target_tools <- c("flye2.8.3", "flye2.9.6", "raven1.8.3", "MyLoasm", "MetaMDBG")
df_filtered <- df %>% filter(Tool %in% target_tools)
df_plot <- df_filtered %>% select(Directory, Tool, N50)

# ---- 【新增】将 N50 转换为 Kb ----
df_plot$N50 <- df_plot$N50 / 1000

# ---- 整理样本顺序 ----
df_plot$Directory <- factor(df_plot$Directory, 
                            levels = sort(unique(df_plot$Directory)))

# ---- 配色与形状 ----
my_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
my_shapes <- c(16, 17, 15, 18, 8)

# ---- 绘图 ----
p <- ggplot(df_plot, aes(x = Directory, y = N50, 
                         group = Tool, color = Tool, shape = Tool)) +
  geom_line(size = 1.2) +
  geom_point(size = 3.5, stroke = 1) +
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = my_shapes) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial", size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(size = 0.8),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "cm"),
    panel.grid.major = element_line(color = "gray92", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Sample ID", 
       y = "N50 (Kb)",                     # 修改为 Kb
       color = "Assembler", 
       shape = "Assembler") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

# ---- 保存 PDF ----
ggsave("Figure_N50_mSystems_Kb.pdf", plot = p, 
       width = 7.5, height = 5, dpi = 300, device = "pdf")