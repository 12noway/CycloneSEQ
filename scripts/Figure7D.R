# ============================================================================
# 污染水平分析 - mSystems 投稿标准
# 7 个子图：每个工具 Original vs Polish 叠加
# 仅显示中位数虚线，不标注数值
# ============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(scales)
})

# ---- 1. 读取数据 ----
data <- read.csv("../表格/Figure 7D.csv", check.names = FALSE)
cat("列名:", colnames(data), "\n")

# ---- 2. 定义工具映射 ----
tools <- list(
  list(display = "Flye 2.8.3", orig = "metaflye_2_8_3_contamination", 
       polish = "metaflye_2_8_3_sr_contamination"),
  list(display = "Flye 2.9.6", orig = "metaflye_2_9_6_contamination", 
       polish = "metaflye_2_9_6_sr_contamination"),
  list(display = "MetaMDBG", orig = "metaMDBG_contamination", 
       polish = "metaMDBG_sr_contamination"),
  list(display = "MyLoAsm", orig = "myloasm_contamination", 
       polish = "myloasm_sr_contamination"),
  list(display = "Raven", orig = "raven_contamination", 
       polish = "raven_sr_contamination"),
  list(display = "HybridSPAdes", orig = "hybridspades_contamination", 
       polish = NA),
  list(display = "OPERA-MS", orig = "opera_ms_contamination", 
       polish = NA)
)

# ---- 3. 构建长格式 ----
records <- list()
for (t in tools) {
  # Original
  if (t$orig %in% colnames(data)) {
    vals <- data[[t$orig]][!is.na(data[[t$orig]])]
    if (length(vals) > 0) {
      records[[length(records) + 1]] <- data.frame(
        Tool = t$display,
        Stage = "Original",
        Contamination = vals,
        stringsAsFactors = FALSE
      )
    }
  }
  # Polish
  if (!is.na(t$polish) && t$polish %in% colnames(data)) {
    vals <- data[[t$polish]][!is.na(data[[t$polish]])]
    if (length(vals) > 0) {
      records[[length(records) + 1]] <- data.frame(
        Tool = t$display,
        Stage = "After Polish",
        Contamination = vals,
        stringsAsFactors = FALSE
      )
    }
  }
}

data_long <- bind_rows(records)

# 因子顺序
tool_order <- c("Flye 2.8.3", "Flye 2.9.6", "MetaMDBG", "MyLoAsm",
                "Raven", "HybridSPAdes", "OPERA-MS")
data_long$Tool  <- factor(data_long$Tool, levels = tool_order)
data_long$Stage <- factor(data_long$Stage, levels = c("Original", "After Polish"))

cat("数据行数:", nrow(data_long), "\n")

# ---- 4. 计算中位数（仅用于画虚线）----
median_data <- data_long %>%
  group_by(Tool, Stage) %>%
  summarise(Median = median(Contamination, na.rm = TRUE), .groups = "drop")

print(median_data)

# ---- 5. mSystems 配色 ----
msystem_fill <- c("Original" = "#56B4E9", "After Polish" = "#D55E00")
msystem_line <- c("Original" = "#0072B2", "After Polish" = "#B2182B")

# ---- 6. 绘图（无数值标注）----
p <- ggplot(data_long, aes(x = Contamination, fill = Stage, color = Stage)) +
  
  # 密度曲线叠加
  geom_density(alpha = 0.40, linewidth = 1.0, adjust = 1.0, trim = TRUE) +
  
  # 中位数虚线（仅虚线，无文字）
  geom_vline(data = median_data, aes(xintercept = Median, color = Stage),
             linetype = "dashed", linewidth = 0.8) +
  
  # 分面：3 行 × 3 列，右下角留空
  facet_wrap(~ Tool, nrow = 3, ncol = 3, scales = "fixed") +
  
  # 标签
  labs(x = "Contamination (%)", y = "Density", fill = "Stage", color = "Stage") +
  
  # 配色
  scale_fill_manual(values = msystem_fill) +
  scale_color_manual(values = msystem_line) +
  
  # 坐标轴
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1), expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # === mSystems 主题 ===
  theme_classic(base_size = 13) +
  theme(
    text = element_text(family = "sans", color = "#1A1A1A"),
    axis.title = element_text(face = "bold", size = 13),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(face = "bold", size = 10, color = "#1A1A1A"),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    axis.ticks.length = unit(0.12, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 6, t = 4)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.width = unit(1.0, "cm"),
    legend.margin = margin(b = 8),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(15, 15, 10, 15),
    panel.grid = element_blank()
  )

# ---- 7. 导出 ----
ggsave("Fig_Contamination_Overlay_mSystems.pdf", p,
       device = cairo_pdf, width = 12, height = 9, dpi = 600)
ggsave("Fig_Contamination_Overlay_mSystems.tiff", p,
       width = 12, height = 9, units = "in", dpi = 600,
       compression = "lzw", bg = "white")
ggsave("Fig_Contamination_Overlay_mSystems.png", p,
       width = 12, height = 9, dpi = 600, bg = "white")

print(p)
cat("\n✅ 图表已生成：\n")
cat("  PDF:  Fig_Contamination_Overlay_mSystems.pdf\n")
cat("  TIFF: Fig_Contamination_Overlay_mSystems.tiff\n")
cat("  PNG:  Fig_Contamination_Overlay_mSystems.png\n")