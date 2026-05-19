library(dplyr)
library(fmsb)
library(scales)

# 1. 加载数据
data <- read.csv("../表格/组装主图_组装MAGs指标统计.csv")

# 定义组合 (增加 Combo 4)
combo1_methods <- c("flye", "metamdbg", "myloasm")
combo2_methods <- c("flye_nextpolish", "metamdbg_nextpolish", "myloasm_nextpolish")
combo3_methods <- c("hybridspades", "opera_ms")
# 新增：特定工具对比组
combo4_methods <- c("myloasm_nextpolish", "metamdbg_nextpolish", "hybridspades")

# 2. 归一化函数
normalize <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  (x - min(x)) / (max(x) - min(x))
}

# 3. 数据预处理
summary_data <- data %>%
  group_by(assembly) %>%
  summarise(
    largest_length = mean(largest_length, na.rm = TRUE),
    N50 = mean(N50, na.rm = TRUE),
    total_length = mean(total_length, na.rm = TRUE),
    contamination = mean(contamination, na.rm = TRUE),
    hm_mags = mean(hm_mags, na.rm = TRUE)
  ) %>%
  ungroup()

summary_norm <- summary_data %>%
  mutate(
    Largest = normalize(largest_length),
    N50 = normalize(N50),
    Total = normalize(total_length),
    Contam = normalize(contamination), 
    HQMAGs = normalize(hm_mags)
  ) %>%
  select(assembly, Largest, N50, Total, Contam, HQMAGs)

# 4. 绘图配置
combo1_colors <- c("#D73027", "#4575B4", "#1A9850")
combo2_colors <- c("#984EA3", "#FF7F00", "#FFD92F")
combo3_colors <- c("#A65628", "#F781BF")
# 为第四个图定义颜色（可以根据喜好调整）
combo4_colors <- c("#FFD92F", "#FF7F00", "#A65628") 

# 调整 PDF 宽度以容纳 4 张图
pdf("Assembly_Radar_Summary_4Charts.pdf", width = 24, height = 6)
# 设置 1 行 4 列布局
par(mfrow = c(1, 4), mar = c(2, 2, 2, 10), xpd = TRUE)

# 5. 核心绘图函数 (逻辑保持不变)
draw_summary_radar <- function(methods, colors) {
  plot_df <- summary_norm %>% filter(assembly %in% methods)
  
  # 按照 methods 提供的顺序重新排序数据，确保颜色与工具对应
  plot_df <- plot_df[match(methods, plot_df$assembly), ]
  plot_df <- na.omit(plot_df) # 防止某些工具名在数据中不存在
  
  radar_data <- as.data.frame(plot_df[, -1])
  rownames(radar_data) <- plot_df$assembly
  
  max_min <- data.frame(Largest=c(1,0), N50=c(1,0), Total=c(1,0),
                        Contam=c(1,0), HQMAGs=c(1,0))
  radar_df <- rbind(max_min, radar_data)
  
  radarchart(
    radar_df,
    axistype = 0,
    seg = 5,
    vlabels = c("Largest\nlength", "N50", "Total\nlength",
                "Contamination", "HM-MAGs"),
    vlcex = 1.4,
    pcol = colors[1:nrow(radar_data)],
    pfcol = alpha(colors[1:nrow(radar_data)], 0.2),
    plwd = 4,
    cglcol = "grey80",
    cglty = 1,
    cglwd = 0.8
  )
  
  legend(
    x = 1.3, y = 1,
    legend = rownames(radar_data),
    col = colors[1:nrow(radar_data)],
    pch = 16,
    pt.cex = 2.5,
    cex = 1.3,
    bty = "n"
  )
}

# 6. 执行生成四个雷达图
draw_summary_radar(combo1_methods, combo1_colors)
draw_summary_radar(combo2_methods, combo2_colors)
draw_summary_radar(combo3_methods, combo3_colors)
draw_summary_radar(combo4_methods, combo4_colors)

dev.off()