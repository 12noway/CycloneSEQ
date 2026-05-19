# ============================================================================
# 长读长数据质量指标箱线图 - GigaScience风格（投稿增强版）
# 修改内容：
# 1. 显著性符号放大
# 2. 坐标轴文字放大
# 3. Panel标签(B/C/D/E)放大
# 4. 线条加粗
# 5. 输出为投稿级高清PDF + PNG
# ============================================================================

# ============================================================================
# 1. 加载必要的库
# ============================================================================
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(grid)
library(cowplot)

# ============================================================================
# 2. 全局投稿参数（重点放大）
# ============================================================================
BASE_TEXT_SIZE <- 18
AXIS_TEXT_SIZE <- 17
AXIS_TITLE_SIZE <- 19
TAG_SIZE <- 24
SIG_SIZE <- 7
POINT_SIZE <- 4
BOX_LINE_WIDTH <- 1.2
AXIS_LINE_WIDTH <- 1.1
PAIR_LINE_WIDTH <- 0.9

# ============================================================================
# 3. 读取并清洗数据
# ============================================================================
file_path <- "../表格/建库主图_建库长读数据长度分布.csv"

df <- read.csv(
  file_path,
  check.names = FALSE,
  fill = TRUE,
  stringsAsFactors = FALSE
)

target_cols <- c(
  "File",
  "N50(kb)",
  "Max(kb)",
  "Bases(Gb)",
  "Passed Classified Bases Rate(%)"
)

plot_data_full <- df %>%
  select(all_of(target_cols)) %>%
  
  separate(
    File,
    into = c("Sample", "Group"),
    sep = "-",
    extra = "drop"
  ) %>%
  
  mutate(
    Group = gsub("_fitted_raw.fastq.gz", "", Group)
  ) %>%
  
  mutate(
    Group = case_when(
      Group == "1" ~ "without_pretreatment",
      Group == "2" ~ "with_pretreatment",
      TRUE ~ Group
    )
  ) %>%
  
  rename(
    N50_kb = `N50(kb)`,
    Max_kb = `Max(kb)`,
    Bases_Gb = `Bases(Gb)`,
    Passed_Rate = `Passed Classified Bases Rate(%)`
  ) %>%
  
  mutate(
    across(
      c(N50_kb, Max_kb, Bases_Gb, Passed_Rate),
      as.numeric
    )
  )

# 因子顺序
plot_data_full$Group <- factor(
  plot_data_full$Group,
  levels = c(
    "without_pretreatment",
    "with_pretreatment"
  )
)

# ============================================================================
# 4. 数据检查
# ============================================================================
cat("=== 数据配对检查 ===\n")
cat("样本总数:", length(unique(plot_data_full$Sample)), "\n")

cat("每个处理组的样本数:\n")
print(table(plot_data_full$Group))

# ============================================================================
# 5. GigaScience风格配色
# ============================================================================
gigascience_color_palette <- c(
  "without_pretreatment" = "#3498DB",
  "with_pretreatment" = "#2C3E50"
)

gigascience_border_palette <- c(
  "without_pretreatment" = "#2980B9",
  "with_pretreatment" = "#1C2833"
)

# ============================================================================
# 6. 投稿级绘图函数（字体与显著性放大）
# ============================================================================
draw_gigascience_paired_boxplot <- function(
    data,
    target_var,
    y_label,
    sub_tag
) {
  
  y_vals <- data[[target_var]]
  
  y_min <- min(y_vals, na.rm = TRUE)
  y_max <- max(y_vals, na.rm = TRUE)
  y_range <- y_max - y_min
  
  p <- ggplot(
    data,
    aes(x = Group, y = .data[[target_var]])
  ) +
    
    # 配对连线
    geom_line(
      aes(group = Sample),
      color = "#7F8C8D",
      linewidth = PAIR_LINE_WIDTH,
      alpha = 0.9
    ) +
    
    # 箱线图
    geom_boxplot(
      aes(fill = Group, color = Group),
      
      width = 0.58,
      
      linewidth = BOX_LINE_WIDTH,
      
      outlier.shape = 21,
      outlier.size = 3.5,
      
      outlier.color = "#E74C3C",
      outlier.fill = "#E74C3C",
      
      alpha = 0.9
    ) +
    
    # 数据点
    geom_point(
      aes(fill = Group, color = Group),
      
      shape = 21,
      
      size = POINT_SIZE,
      
      stroke = 1,
      
      color = "white"
    ) +
    
    # 配色
    scale_fill_manual(
      values = gigascience_color_palette
    ) +
    
    scale_color_manual(
      values = gigascience_border_palette
    ) +
    
    # =========================================================================
  # 显著性检验（重点放大）
  # =========================================================================
  stat_compare_means(
    comparisons = list(
      c(
        "without_pretreatment",
        "with_pretreatment"
      )
    ),
    
    method = "wilcox.test",
    
    paired = TRUE,
    
    label = "p.signif",
    
    bracket.size = 1.1,
    
    tip.length = 0.015,
    
    vjust = 0.6,
    
    size = SIG_SIZE,
    
    fontface = "bold",
    
    label.y = y_max + (y_range * 0.15)
  ) +
    
    # 标签
    labs(
      y = y_label,
      x = NULL,
      tag = sub_tag
    ) +
    
    # =========================================================================
  # 投稿级主题（全部放大）
  # =========================================================================
  theme_minimal(base_size = BASE_TEXT_SIZE) +
    
    theme(
      
      text = element_text(
        family = "sans",
        color = "#2C3E50"
      ),
      
      # Panel标签
      plot.tag = element_text(
        size = TAG_SIZE,
        face = "bold",
        color = "#2C3E50"
      ),
      
      plot.tag.position = c(0.02, 0.98),
      
      # 坐标轴线
      axis.line = element_line(
        linewidth = AXIS_LINE_WIDTH,
        color = "#2C3E50"
      ),
      
      axis.ticks = element_line(
        linewidth = AXIS_LINE_WIDTH,
        color = "#2C3E50"
      ),
      
      axis.ticks.length = unit(0.18, "cm"),
      
      # Y轴标题
      axis.title.y = element_text(
        size = AXIS_TITLE_SIZE,
        face = "bold",
        color = "#2C3E50",
        margin = margin(r = 12)
      ),
      
      # Y轴刻度
      axis.text.y = element_text(
        size = AXIS_TEXT_SIZE,
        face = "bold",
        color = "#2C3E50"
      ),
      
      # X轴刻度
      axis.text.x = element_text(
        size = AXIS_TEXT_SIZE,
        face = "bold",
        color = "#2C3E50",
        lineheight = 1.1
      ),
      
      # 网格线
      panel.grid.major = element_line(
        color = "#ECF0F1",
        linewidth = 0.5
      ),
      
      panel.grid.minor = element_blank(),
      
      # 图例
      legend.position = "none",
      
      # 背景
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      # 边距
      plot.margin = margin(
        18,
        18,
        18,
        18
      )
    ) +
    
    # 给显著性符号留空间
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.18))
    ) +
    
    # X轴标签
    scale_x_discrete(
      labels = c(
        "without_pretreatment" = "Without\nPretreatment",
        "with_pretreatment" = "With\nPretreatment"
      )
    )
  
  return(p)
}

# ============================================================================
# 7. 指标定义
# ============================================================================
metrics_list <- list(
  "N50_kb" = "N50 Read Length (kb)",
  
  "Max_kb" = "Maximum Read Length (kb)",
  
  "Bases_Gb" = "Total Bases (Gb)",
  
  "Passed_Rate" = "Passed Classified Bases\nRate (%)"
)

# ============================================================================
# 8. 生成四个子图
# ============================================================================
plot_B <- draw_gigascience_paired_boxplot(
  plot_data_full,
  "N50_kb",
  metrics_list[["N50_kb"]],
  "B"
)

plot_C <- draw_gigascience_paired_boxplot(
  plot_data_full,
  "Max_kb",
  metrics_list[["Max_kb"]],
  "C"
)

plot_D <- draw_gigascience_paired_boxplot(
  plot_data_full,
  "Bases_Gb",
  metrics_list[["Bases_Gb"]],
  "D"
)

plot_E <- draw_gigascience_paired_boxplot(
  plot_data_full,
  "Passed_Rate",
  metrics_list[["Passed_Rate"]],
  "E"
)

# ============================================================================
# 9. 组合图
# ============================================================================
final_plot <- plot_grid(
  plot_B,
  plot_C,
  plot_D,
  plot_E,
  
  ncol = 2,
  nrow = 2,
  
  align = "hv",
  axis = "tblr"
)

# ============================================================================
# 10. 保存投稿级PDF（重点）
# ============================================================================
ggsave(
  filename = "GigaScience_Paired_Boxplots_Publication.pdf",
  
  plot = final_plot,
  
  width = 18,
  height = 14,
  
  device = cairo_pdf,
  
  bg = "white"
)

# ============================================================================
# 11. 保存高清PNG
# ============================================================================
ggsave(
  filename = "GigaScience_Paired_Boxplots_Publication.png",
  
  plot = final_plot,
  
  width = 18,
  height = 14,
  
  dpi = 600,
  
  bg = "white"
)

# ============================================================================
# 12. 单独保存每个Panel（PDF）
# ============================================================================
ggsave(
  "Panel_B_N50.pdf",
  plot_B,
  width = 9,
  height = 7,
  device = cairo_pdf
)

ggsave(
  "Panel_C_MaxLength.pdf",
  plot_C,
  width = 9,
  height = 7,
  device = cairo_pdf
)

ggsave(
  "Panel_D_Bases.pdf",
  plot_D,
  width = 9,
  height = 7,
  device = cairo_pdf
)

ggsave(
  "Panel_E_PassedRate.pdf",
  plot_E,
  width = 9,
  height = 7,
  device = cairo_pdf
)

# ============================================================================
# 13. 终端反馈
# ============================================================================
cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")

cat("✅ 投稿级GigaScience风格图已生成完成！\n")

cat(paste(rep("-", 80), collapse = ""))
cat("\n")

cat("📁 输出文件：\n")
cat("1. GigaScience_Paired_Boxplots_Publication.pdf\n")
cat("2. GigaScience_Paired_Boxplots_Publication.png\n")
cat("3. Panel_B_N50.pdf\n")
cat("4. Panel_C_MaxLength.pdf\n")
cat("5. Panel_D_Bases.pdf\n")
cat("6. Panel_E_PassedRate.pdf\n")

cat("\n")

cat("🎯 投稿优化内容：\n")
cat("- 显著性星号显著放大\n")
cat("- 坐标轴字体加粗加大\n")
cat("- Panel标签(B/C/D/E)放大\n")
cat("- 箱线图边框加粗\n")
cat("- 数据点放大\n")
cat("- 配对线加粗\n")
cat("- 输出矢量PDF格式\n")

cat("\n")

cat("📌 推荐投稿使用：\n")
cat("- PDF：用于正式投稿\n")
cat("- PNG：用于PPT或预览\n")

cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")