# ==============================================================================
# Project: Genome Assembly & MAG Statistics Visualization (mSystems Revised)
# Target Output: 4 Subplots (E, F, G, H) with 1 Global Legend at Bottom
# ==============================================================================

# 1. 加载依赖包
suppressPackageStartupMessages({
  library(dplyr)
  library(fmsb)
  library(scales)
  library(stringr)
})

# 2. 读取数据并解析工具名称
data_path <- "../表格/Figure 7E-H.tsv"
if (!file.exists(data_path)) {
  data_path <- "../表格/Figure 7E-H.tsv"
}

raw_data <- read.delim(data_path, stringsAsFactors = FALSE)

# 清理工具名称与映射
df_clean <- raw_data %>%
  mutate(
    tool_raw = sapply(strsplit(file, "/"), function(x) x[5]),
    tool_clean = case_when(
      tolower(tool_raw) == "flye2.8.3"       ~ "flye2.8.3",
      tolower(tool_raw) == "flye2.8.3_sr"    ~ "flye2.8.3_sr",
      tolower(tool_raw) == "flye2.9.6"       ~ "flye2.9.6",
      tolower(tool_raw) == "flye2.9.6_sr"    ~ "flye2.9.6_sr",
      tolower(tool_raw) == "metamdbg"        ~ "metamdbg",
      tolower(tool_raw) == "metamdbg_sr"     ~ "metamdbg_sr",
      tolower(tool_raw) == "myloasm"         ~ "myloasm",
      tolower(tool_raw) == "myloasm_sr"      ~ "myloasm_sr",
      tolower(tool_raw) == "raven1.8.3"      ~ "raven",
      tolower(tool_raw) == "raven1.8.3_sr"   ~ "raven_sr",
      tolower(tool_raw) == "hybridspades"    ~ "hybridspades",
      tolower(tool_raw) == "opera_ms_polish" ~ "opera_ms",
      TRUE ~ tolower(tool_raw)
    )
  )

# 3. 按工具计算 9 个样本的均值 (Mean Metrics)
tool_means <- df_clean %>%
  group_by(tool_clean) %>%
  summarise(
    max_len       = mean(max_len, na.rm = TRUE),
    N50           = mean(N50, na.rm = TRUE),
    sum_len       = mean(sum_len, na.rm = TRUE),
    contamination = mean(contamination, na.rm = TRUE),
    sum_hm_mags   = mean(sum_hm_mags, na.rm = TRUE),
    .groups       = "drop"
  )

# 4. 定义 4 个子图包含的工具集合
combo1 <- c("flye2.8.3", "flye2.9.6", "metamdbg", "myloasm", "raven")
combo2 <- c("flye2.8.3_sr", "flye2.9.6_sr", "metamdbg_sr", "myloasm_sr", "raven_sr")
combo3 <- c("hybridspades", "opera_ms")
combo4 <- c("flye2.8.3_sr", "flye2.9.6_sr", "metamdbg_sr", "myloasm_sr", "raven_sr", "hybridspades", "opera_ms")

# 专业学术级配色板（确保每个工具颜色唯一）
tool_colors <- c(
  "flye2.8.3"     = "#1F78B4", "flye2.9.6"     = "#33A02C",
  "metamdbg"      = "#6A3D9A", "myloasm"       = "#FF7F00", "raven" = "#B15928",
  "flye2.8.3_sr"  = "#E31A1C", "flye2.9.6_sr"  = "#A6CEE3",
  "metamdbg_sr"   = "#B2DF8A", "myloasm_sr"    = "#CAB2D6", "raven_sr" = "#FDBF6F",
  "hybridspades"  = "#FB9A99", "opera_ms"      = "#B15928"
)

# 5. 安全归一化函数
safe_norm <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  (x - min(x)) / (max(x) - min(x))
}

safe_rev_norm <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  1 - ((x - min(x)) / (max(x) - min(x)))
}

# 6. 单个雷达子图绘制函数（纯绘图，无局部图例）
plot_subplot_radar <- function(tools_list, subplot_title) {
  sub_df <- tool_means %>% filter(tool_clean %in% tools_list)
  
  df_norm <- sub_df %>%
    mutate(
      Largest = safe_norm(max_len),
      N50     = safe_norm(N50),
      Total   = safe_norm(sum_len),
      Contam  = safe_rev_norm(contamination),
      HQMAGs  = safe_norm(sum_hm_mags)
    ) %>%
    select(tool_clean, Largest, N50, Total, Contam, HQMAGs)
  
  radar_mat <- as.data.frame(df_norm[, -1])
  rownames(radar_mat) <- df_norm$tool_clean
  
  max_min <- data.frame(
    Largest = c(1, 0), N50 = c(1, 0), Total = c(1, 0),
    Contam  = c(1, 0), HQMAGs = c(1, 0)
  )
  radar_df <- rbind(max_min, radar_mat)
  curr_colors <- tool_colors[rownames(radar_mat)]
  
  # 设置适合 2x2 内部子图的边距
  par(mar = c(2.5, 2, 3, 2), xpd = TRUE)
  
  radarchart(
    radar_df,
    axistype    = 1,
    seg         = 4,
    vlabels     = c("Largest length", "N50", "Total length", "Contamination (Low)", "HM-MAGs"),
    vlcex       = 0.90,
    pcol        = curr_colors,
    pfcol       = alpha(curr_colors, 0.18),
    plwd        = 2.0,
    cglcol      = "grey80",
    cglty       = 1,
    cglwd       = 0.8,
    axislabcol  = NA,
    caxislabels = rep("", 5)
  )
  
  title(main = subplot_title, cex.main = 1.15, font.main = 2)
}

# 7. 全局图例绘制函数
plot_global_legend <- function() {
  par(mar = c(0, 0, 0, 0), xpd = TRUE)
  plot.new()
  
  all_tools <- names(tool_colors)
  
  legend(
    "center",
    legend = all_tools,
    col    = tool_colors,
    pch    = 16,
    pt.cex = 1.5,
    cex    = 0.90,
    bty    = "n",
    ncol   = 4,
    x.intersp = 0.8,
    y.intersp = 1.2
  )
}

# 8. 主绘制流程（更新序号为 E, F, G, H）
draw_all_figures <- function() {
  lay_matrix <- matrix(c(1, 2,
                         3, 4,
                         5, 5), nrow = 3, byrow = TRUE)
  
  # --- A. 输出 PDF 矢量图 ---
  pdf("Assembly_4Subplots_EFGH.pdf", width = 10, height = 10.5)
  layout(lay_matrix, heights = c(1, 1, 0.28))
  
  plot_subplot_radar(combo1, "E. Unpolished Long-Read")
  plot_subplot_radar(combo2, "F. Polished Long-Read (SR)")
  plot_subplot_radar(combo3, "G. Hybrid Assemblers")
  plot_subplot_radar(combo4, "H. Polished vs. Hybrid Comparison")
  plot_global_legend()
  
  dev.off()
  
  # --- B. 输出 TIFF 高清位图 (300 DPI) ---
  tiff("Assembly_4Subplots_EFGH.tiff", width = 10, height = 10.5, units = "in", res = 300, compression = "lzw")
  layout(lay_matrix, heights = c(1, 1, 0.28))
  
  plot_subplot_radar(combo1, "E. Unpolished Long-Read")
  plot_subplot_radar(combo2, "F. Polished Long-Read (SR)")
  plot_subplot_radar(combo3, "G. Hybrid Assemblers")
  plot_subplot_radar(combo4, "H. Polished vs. Hybrid Comparison")
  plot_global_legend()
  
  dev.off()
  
  message("Successfully generated Assembly_4Subplots_EFGH.pdf and Assembly_4Subplots_EFGH.tiff!")
}

# 执行绘图
draw_all_figures()