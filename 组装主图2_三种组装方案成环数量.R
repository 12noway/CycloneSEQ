# ============================================================================
# SCI Heatmap – 无侧边名、加粗标题专业版
# 策略分组: Long-read / Long-read + Short-read Polish / HYB
# ============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(grid)
library(gridExtra)

# ----------------------------
# 1. 数据准备
# ----------------------------
cycle_data <- data.frame(
  sample = paste0("Sample_", 1:5),
  flye_cycle = c(2, 2, 7, 5, 6),
  flye_short_polish_cycle = c(3, 2, 7, 5, 6), # 内部变量名同步更新
  hybridspades_cycle = c(2, 0, 1, 3, 0),
  metamdbg_cycle = c(4, 6, 8, 11, 7),
  metamdbg_short_polish_cycle = c(4, 5, 8, 10, 7),
  myloasm_cycle = c(8, 9, 18, 13, 16),
  myloasm_short_polish_cycle = c(9, 8, 18, 6, 16),
  opera_ms_cycle = c(0, 1, 1, 0, 0)
)

# ----------------------------
# 2. 数据转换与因子排序
# ----------------------------
cycle_long <- cycle_data %>%
  pivot_longer(cols = -sample,
               names_to = "tool",
               values_to = "cycle") %>%
  mutate(
    tool = gsub("_cycle", "", tool),
    tool_display = case_when(
      tool == "flye" ~ "Flye",
      tool == "metamdbg" ~ "MetaMDBG",
      tool == "myloasm" ~ "MyLoAsm",
      tool == "flye_short_polish" ~ "Flye + Polished",
      tool == "metamdbg_short_polish" ~ "MetaMDBG + Polished",
      tool == "myloasm_short_polish" ~ "MyLoAsm + Polished",
      tool == "hybridspades" ~ "HybridSPAdes",
      tool == "opera_ms" ~ "OPERA-MS"
    )
  )

# 严格定义工具显示顺序 (Long-read -> Polished -> HYB)
tool_order <- c(
  "Flye", "MetaMDBG", "MyLoAsm",                            # Long-read
  "Flye + Polished", "MetaMDBG + Polished", "MyLoAsm + Polished", # Long-read + Polished
  "HybridSPAdes", "OPERA-MS"                                # HYB
)

cycle_long$tool_display <- factor(cycle_long$tool_display, levels = tool_order)
# 样本仅用于绘图层级，不显示标签
cycle_long$sample <- factor(cycle_long$sample, levels = rev(paste0("Sample_", 1:5))) 

# ----------------------------
# 3. 热图主体 (取消 Y 轴标签)
# ----------------------------
heatmap_main <- ggplot(cycle_long, aes(x = tool_display, y = sample, fill = cycle)) +
  geom_tile(color = "white", linewidth = 0.8) +
  # 动态文字颜色优化
  geom_text(aes(label = cycle, color = cycle > 12), 
            size = 5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("white", "black")) + 
  # 配色方案：mako
  scale_fill_viridis_c(
    option = "mako", 
    direction = -1, 
    name = "Cycle Count",
    begin = 0.1, end = 0.9
  ) +
  # 组间分割线
  geom_vline(xintercept = c(3.5, 6.5), color = "black", linewidth = 1.2) +
  labs(x = "Assembly Tools", y = NULL) +
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.margin = unit(c(0.2, 0.5, 0.5, 2), "cm")
  )

# ----------------------------
# 4. 策略分组标签 (顶部)
# ----------------------------
group_annotation <- ggplot() +
  annotate("segment", x = c(0.6, 3.6, 6.6), xend = c(3.4, 6.4, 8.4), y = 0, yend = 0, size = 1.2, color = "black") +
  annotate("text", 
           x = c(2, 5, 7.5), 
           y = 0.4, 
           # 术语修改点：Long-read / Short-read
           label = c("Long-read Only", "Long-read + Short-read Polished", "Hybrid (HYB)"), 
           size = 5, fontface = "bold", color = "black") +
  xlim(0.5, 8.5) + ylim(-0.2, 1) +
  theme_void()

# ----------------------------
# 5. 标题组合 (加黑加粗)
# ----------------------------
title_label <- textGrob("Comparative Analysis of Assembly Cycle Counts", 
                        gp = gpar(fontsize = 20, fontface = "bold", col = "black"))

# 使用 grid.arrange 拼装
final_plot <- grid.arrange(
  title_label,
  group_annotation,
  heatmap_main,
  nrow = 3,
  heights = c(0.12, 0.08, 0.80)
)

# ----------------------------
# 6. 保存导出
# ----------------------------
# PDF 格式 (适合投稿)
ggsave("Assembly_Cycle_Heatmap_LongRead_Style.pdf", 
       final_plot, 
       device = cairo_pdf, 
       width = 11, height = 8.5)

# PNG 格式 (适合预览)
ggsave("Assembly_Cycle_Heatmap_LongRead_Style.png", 
       final_plot, 
       width = 11, height = 8.5, dpi = 300, bg = "white")

print("绘图完成！已将 TGS 替换为 Long-read，NGS 替换为 Short-read。")