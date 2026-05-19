# ============================================================================
# 长读长数据长度分布堆叠柱状图 - GigaScience投稿增强版
# 修改内容：
# 1. 全部文字放大（投稿级）
# 2. Panel标签放大
# 3. 图例文字放大
# 4. 坐标轴文字放大
# 5. 线条加粗
# 6. 输出为矢量PDF（投稿推荐）
# ============================================================================

# ============================================================================
# 1. 加载库
# ============================================================================
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(grid)
library(cowplot)

# ============================================================================
# 2. 全局投稿参数
# ============================================================================
BASE_TEXT_SIZE <- 18
AXIS_TEXT_SIZE <- 17
AXIS_TITLE_SIZE <- 20
LEGEND_TITLE_SIZE <- 17
LEGEND_TEXT_SIZE <- 15
TAG_SIZE <- 26
TITLE_SIZE <- 22

AXIS_LINE_WIDTH <- 1.2
BAR_BORDER_WIDTH <- 0.35

# ============================================================================
# 3. 读取数据
# ============================================================================
file_path <- "../表格/建库附图_建库长读数据长度分布.csv"

df <- read.csv(
  file_path,
  check.names = FALSE
)

# ============================================================================
# 4. 数据清洗与组别重命名
# ============================================================================
plot_data_all <- df %>%
  
  separate(
    File,
    into = c("Sample", "Group"),
    sep = "-",
    extra = "drop"
  ) %>%
  
  mutate(
    Group = gsub(
      "_fitted_raw.fastq.gz",
      "",
      Group
    )
  ) %>%
  
  mutate(
    Group = case_when(
      Group == "1" ~ "without_pretreatment",
      Group == "2" ~ "with_pretreatment",
      TRUE ~ Group
    )
  ) %>%
  
  select(
    Sample,
    Group,
    `1-2kb(%)`,
    `2-3kb(%)`,
    `3-4kb(%)`,
    `4-5kb(%)`,
    `5-6kb(%)`,
    `6-7kb(%)`,
    `7-8kb(%)`,
    `8-9kb(%)`,
    `9-10kb(%)`,
    `>=10kb(%)`
  ) %>%
  
  pivot_longer(
    cols = ends_with("%)"),
    names_to = "Length_Range",
    values_to = "Percentage"
  )

# ============================================================================
# 5. 因子顺序
# ============================================================================
level_order <- c(
  "1-2kb(%)",
  "2-3kb(%)",
  "3-4kb(%)",
  "4-5kb(%)",
  "5-6kb(%)",
  "6-7kb(%)",
  "7-8kb(%)",
  "8-9kb(%)",
  "9-10kb(%)",
  ">=10kb(%)"
)

plot_data_all$Length_Range <- factor(
  plot_data_all$Length_Range,
  levels = rev(level_order)
)

plot_data_all$Group <- factor(
  plot_data_all$Group,
  levels = c(
    "without_pretreatment",
    "with_pretreatment"
  )
)

# ============================================================================
# 6. GigaScience风格蓝色渐变
# ============================================================================
gigascience_colors <- c(
  "#EAF2F8",
  "#D4E6F1",
  "#AED6F1",
  "#85C1E9",
  "#5DADE2",
  "#3498DB",
  "#2E86C1",
  "#2874A6",
  "#1F618D",
  "#154360"
)

# ============================================================================
# 7. 图例标签
# ============================================================================
length_labels <- c(
  "1-2kb(%)" = "1-2 kb",
  "2-3kb(%)" = "2-3 kb",
  "3-4kb(%)" = "3-4 kb",
  "4-5kb(%)" = "4-5 kb",
  "5-6kb(%)" = "5-6 kb",
  "6-7kb(%)" = "6-7 kb",
  "7-8kb(%)" = "7-8 kb",
  "8-9kb(%)" = "8-9 kb",
  "9-10kb(%)" = "9-10 kb",
  ">=10kb(%)" = "≥10 kb"
)

# ============================================================================
# 8. 投稿级绘图函数
# ============================================================================
draw_gigascience_stacked_plot <- function(
    data,
    is_facet = FALSE,
    figure_tag = NULL
) {
  
  p <- ggplot(
    data,
    aes(
      x = Group,
      y = Percentage,
      fill = Length_Range
    )
  ) +
    
    # =========================================================================
  # 堆叠柱状图
  # =========================================================================
  geom_bar(
    stat = "identity",
    
    width = 0.65,
    
    color = "white",
    
    linewidth = BAR_BORDER_WIDTH,
    
    alpha = 0.95
  ) +
    
    # =========================================================================
  # 配色
  # =========================================================================
  scale_fill_manual(
    values = gigascience_colors,
    labels = length_labels
  ) +
    
    # =========================================================================
  # Y轴
  # =========================================================================
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    limits = c(0, 101)
  ) +
    
    # =========================================================================
  # 标签
  # =========================================================================
  labs(
    y = "Percentage of Reads (%)",
    
    x = NULL,
    
    fill = "Read Length\nRange",
    
    tag = figure_tag
  ) +
    
    # =========================================================================
  # 投稿级主题
  # =========================================================================
  theme_minimal(base_size = BASE_TEXT_SIZE) +
    
    theme(
      
      # 全局字体
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
      
      plot.tag.position = c(0.01, 0.98),
      
      # 坐标轴线
      axis.line = element_line(
        linewidth = AXIS_LINE_WIDTH,
        color = "#2C3E50"
      ),
      
      axis.ticks = element_line(
        linewidth = AXIS_LINE_WIDTH,
        color = "#2C3E50"
      ),
      
      axis.ticks.length = unit(0.15, "cm"),
      
      # Y轴标题
      axis.title.y = element_text(
        size = AXIS_TITLE_SIZE,
        face = "bold",
        color = "#2C3E50",
        margin = margin(r = 15)
      ),
      
      # X轴文字（重点放大）
      axis.text.x = element_text(
        size = AXIS_TEXT_SIZE,
        face = "bold",
        color = "#2C3E50",
        
        angle = 45,
        
        hjust = 1,
        
        vjust = 1,
        
        lineheight = 1.1,
        
        margin = margin(t = 8)
      ),
      
      # Y轴文字
      axis.text.y = element_text(
        size = AXIS_TEXT_SIZE,
        face = "bold",
        color = "#2C3E50"
      ),
      
      # =========================================================================
      # 图例（重点放大）
      # =========================================================================
      legend.position = "right",
      
      legend.title = element_text(
        size = LEGEND_TITLE_SIZE,
        face = "bold",
        color = "#2C3E50",
        margin = margin(b = 8)
      ),
      
      legend.text = element_text(
        size = LEGEND_TEXT_SIZE,
        face = "bold",
        color = "#2C3E50"
      ),
      
      legend.key.size = unit(0.6, "cm"),
      
      legend.spacing.y = unit(0.25, "cm"),
      
      # 网格线
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
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
        25,
        25,
        35,
        25
      )
    )
  
  # ===========================================================================
  # 分面
  # ===========================================================================
  if(is_facet) {
    
    p <- p +
      
      facet_wrap(
        ~Sample,
        nrow = 1
      ) +
      
      theme(
        
        strip.background = element_blank(),
        
        strip.text = element_text(
          size = 16,
          face = "bold",
          color = "#2C3E50",
          margin = margin(b = 10)
        ),
        
        axis.text.x = element_text(
          size = 14,
          face = "bold",
          color = "#2C3E50",
          
          angle = 45,
          
          hjust = 1,
          
          vjust = 1
        )
      )
  }
  
  return(p)
}

# ============================================================================
# 9. 汇总图（Panel A）
# ============================================================================
p_summary <- draw_gigascience_stacked_plot(
  plot_data_all,
  is_facet = TRUE,
  figure_tag = "A"
) +
  
  labs(
    title = "Read Length Distribution Across Samples"
  ) +
  
  theme(
    
    plot.title = element_text(
      size = TITLE_SIZE,
      
      face = "bold",
      
      hjust = 0.5,
      
      color = "#2C3E50",
      
      margin = margin(b = 25)
    )
  )

# ============================================================================
# 10. 保存投稿级PDF（重点）
# ============================================================================
ggsave(
  filename = "GigaScience_Summary_Stacked_Panel_A_Publication.pdf",
  
  plot = p_summary,
  
  width = 18,
  
  height = 10,
  
  device = cairo_pdf,
  
  bg = "white",
  
  limitsize = FALSE
)

# ============================================================================
# 11. 同时保存高清PNG
# ============================================================================
ggsave(
  filename = "GigaScience_Summary_Stacked_Panel_A_Publication.png",
  
  plot = p_summary,
  
  width = 18,
  
  height = 10,
  
  dpi = 600,
  
  bg = "white",
  
  limitsize = FALSE
)

# ============================================================================
# 12. 单个样本图
# ============================================================================
samples <- unique(plot_data_all$Sample)

for(i in seq_along(samples)) {
  
  s <- samples[i]
  
  sample_df <- plot_data_all %>%
    filter(Sample == s)
  
  p_single <- draw_gigascience_stacked_plot(
    sample_df,
    figure_tag = paste0("A", i)
  ) +
    
    labs(
      title = paste("Sample:", s)
    ) +
    
    theme(
      
      plot.title = element_text(
        size = 18,
        
        face = "bold",
        
        hjust = 0.5,
        
        color = "#2C3E50",
        
        margin = margin(b = 18)
      )
    )
  
  file_name_pdf <- paste0(
    "GigaScience_Stacked_Sample_",
    s,
    "_A",
    i,
    "_Publication.pdf"
  )
  
  ggsave(
    file_name_pdf,
    
    plot = p_single,
    
    width = 7.5,
    
    height = 8.5,
    
    device = cairo_pdf,
    
    bg = "white"
  )
  
  message(paste("已保存:", file_name_pdf))
}

# ============================================================================
# 13. 组合图（2×2）
# ============================================================================
if(length(samples) >= 4) {
  
  selected_samples <- samples[1:4]
  
  plot_list <- list()
  
  for(i in 1:4) {
    
    s <- selected_samples[i]
    
    sample_df <- plot_data_all %>%
      filter(Sample == s)
    
    p <- draw_gigascience_stacked_plot(
      sample_df,
      figure_tag = LETTERS[i]
    ) +
      
      labs(
        title = paste("Sample:", s)
      ) +
      
      theme(
        
        plot.title = element_text(
          size = 15,
          
          face = "bold",
          
          hjust = 0.5,
          
          margin = margin(b = 10)
        ),
        
        legend.position = ifelse(i == 1, "right", "none")
      )
    
    plot_list[[i]] <- p
  }
  
  combined_plot <- plot_grid(
    plotlist = plot_list,
    
    ncol = 2,
    
    nrow = 2,
    
    align = "hv",
    
    axis = "tblr"
  )
  
  title_plot <- ggplot() +
    
    theme_void() +
    
    labs(
      title = "Read Length Distribution for Selected Samples"
    ) +
    
    theme(
      
      plot.title = element_text(
        size = 20,
        
        face = "bold",
        
        hjust = 0.5,
        
        color = "#2C3E50",
        
        margin = margin(b = 15)
      )
    )
  
  final_combined_plot <- plot_grid(
    title_plot,
    
    combined_plot,
    
    ncol = 1,
    
    rel_heights = c(0.08, 0.92)
  )
  
  # ===========================================================================
  # 保存组合PDF
  # ===========================================================================
  ggsave(
    "GigaScience_Combined_Stacked_Plot_Publication.pdf",
    
    plot = final_combined_plot,
    
    width = 14,
    
    height = 12,
    
    device = cairo_pdf,
    
    bg = "white"
  )
  
  # PNG
  ggsave(
    "GigaScience_Combined_Stacked_Plot_Publication.png",
    
    plot = final_combined_plot,
    
    width = 14,
    
    height = 12,
    
    dpi = 600,
    
    bg = "white"
  )
}

# ============================================================================
# 14. 终端反馈
# ============================================================================
cat("\n")
cat(paste(rep("=", 85), collapse = ""))
cat("\n")

cat("✅ 投稿级GigaScience堆叠柱状图已生成完成！\n")

cat(paste(rep("-", 85), collapse = ""))
cat("\n")

cat("📁 输出文件：\n")
cat("1. GigaScience_Summary_Stacked_Panel_A_Publication.pdf\n")
cat("2. GigaScience_Summary_Stacked_Panel_A_Publication.png\n")
cat("3. 单样本Publication.pdf文件\n")
cat("4. GigaScience_Combined_Stacked_Plot_Publication.pdf\n")
cat("5. GigaScience_Combined_Stacked_Plot_Publication.png\n")

cat("\n")

cat("🎯 投稿增强内容：\n")
cat("- 所有文字显著放大\n")
cat("- 图例文字加粗加大\n")
cat("- Panel标签放大\n")
cat("- 坐标轴文字加粗\n")
cat("- 坐标轴线加粗\n")
cat("- PDF矢量输出\n")
cat("- 600 DPI高清PNG\n")

cat("\n")

cat("📌 推荐：\n")
cat("- PDF用于正式投稿\n")
cat("- PNG用于PPT/预览\n")

cat("\n")
cat(paste(rep("=", 85), collapse = ""))
cat("\n")