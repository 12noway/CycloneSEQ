# ============================================================================
# MAGs 占比分析 - Nature/Cell 风格堆叠柱状图 (双数值标注版)
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. 数据准备
total_vOTUs <- 314

assembler_data <- data.frame(
  Assembler = c("flye","metamdbg","myloasm",
                "flye_short_polish","metamdbg_short_polish","myloasm_short_polish",
                "hybridspades","opera_ms"),
  Single_tool = c(235,247,229,252,262,259,275,252),
  # 修改术语：TGS -> Long-read, TGS_nextpolish -> Long-read+Short-read
  Group = c(rep("Long-read",3),
            rep("Long-read+Short-read",3),
            rep("HYB",2))
)

# 计算差值（未组装出来的数量）
assembler_data$Expansion <- total_vOTUs - assembler_data$Single_tool

plot_data <- assembler_data %>%
  pivot_longer(cols = c("Single_tool","Expansion"),
               names_to = "Type",
               values_to = "Value") %>%
  group_by(Assembler) %>%
  mutate(Proportion = Value / sum(Value))

# 因子顺序固化
plot_data$Type <- factor(plot_data$Type, levels = c("Expansion","Single_tool"))
plot_data$Assembler <- factor(plot_data$Assembler, levels = assembler_data$Assembler)
# 修正 Group 的因子顺序
plot_data$Group <- factor(plot_data$Group, levels = c("Long-read", "Long-read+Short-read", "HYB"))

# 2. 绘图
p <- ggplot(plot_data, aes(x = Assembler, y = Proportion, fill = Type)) +
  
  # 柱状图：精细黑边
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.5) +
  
  # 数值标注核心逻辑：
  # 1. 下方蓝色区域：显示组装出来的数量 (Value)，白色加粗
  # 2. 上方灰色区域：显示未组装的数量 (Value)，深灰色
  geom_text(aes(label = Value, 
                color = Type,
                fontface = ifelse(Type == "Single_tool", "bold", "plain")),
            position = position_stack(vjust = 0.5),
            size = 4) +
  
  # 分面设置
  facet_grid(~Group, scales = "free_x", space = "free_x") +
  
  # 配色方案
  scale_fill_manual(values = c(
    "Single_tool" = "#285291",   # 深蓝色
    "Expansion"   = "#F2F2F2"    # 浅灰色
  )) +
  
  # 文字颜色映射：确保蓝色背景配白字，灰色背景配灰黑字
  scale_color_manual(values = c(
    "Single_tool" = "white",
    "Expansion"   = "grey40"
  )) +
  
  # 轴刻度
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0),
    labels = c("0","25%","50%","75%","100%")
  ) +
  
  labs(y = "Proportion of Total MAGs", x = NULL) +
  
  # === Nature 主题定制 ===
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "sans", color = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    panel.spacing.x = unit(1.2, "lines"),
    legend.position = "none", # 隐藏图例，颜色已自解释
    plot.margin = margin(20, 20, 10, 10)
  )

# 3. 保存
ggsave("MAGs_Proportion_LongRead_Style.pdf", p, 
       width = 9.5, height = 5.5, device = cairo_pdf) # 略微增加宽度以适应较长的标签

print(p)