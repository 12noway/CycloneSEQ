# ==========================================
# 1. 加载必要包
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, ggsci, grDevices)

# ==========================================
# 2. 原始数据录入
# ==========================================
data_raw <- tibble(
  `样本名称` = c('DT2510681588-1', 'DT2510681588-2', 'DT2510681589-1', 'DT2510681589-2',
             'DT2510681590-1', 'DT2510681590-2', 'DT2510681591-1', 'DT2510681591-2',
             'DT2510681592-1', 'DT2510681592-2', 'DT2510681593-1', 'DT2510681593-2',
             'DT2510681594-1', 'DT2510681594-2', 'DT2510681595-1', 'DT2510681595-2',
             'DT2510681596-1', 'DT2510681596-2', 'DT2510681597-1', 'DT2510681597-2',
             'DT2510681598-1', 'DT2510681598-2', 'DT2510681599-1', 'DT2510681599-2',
             'DT2510681600-1', 'DT2510681600-2', 'DT2510681601-1', 'DT2510681601-2',
             'DT2510681602-1', 'DT2510681602-2'),
  `ReadNum` = c(145.35, 103.21, 133.29, 113.31, 129.36, 91.71, 126.42, 115.88, 94.88, 117.79,
                100.14, 77.22, 121.65, 132.82, 104.59, 107.70, 103.49, 98.77, 108.25, 73.65,
                105.27, 126.07, 95.97, 117.19, 112.02, 116.78, 99.16, 110.33, 119.70, 125.26),
  `BaseNum` = c(29.06, 20.64, 26.66, 22.66, 25.88, 18.34, 25.28, 23.18, 18.98, 23.56,
                20.02, 15.44, 24.32, 26.56, 20.92, 21.54, 20.70, 19.76, 21.64, 14.74,
                21.06, 25.22, 19.20, 23.44, 22.40, 23.36, 19.84, 22.06, 23.94, 25.06),
  `samGC` = c(43.72, 44.08, 45.06, 45.03, 43.77, 43.25, 45.41, 46.94, 46.57, 44.23,
              45.73, 43.86, 43.68, 43.14, 46.45, 44.61, 44.64, 43.29, 45.64, 45.34,
              46.29, 45.62, 45.16, 43.94, 43.41, 43.12, 45.16, 43.01, 46.03, 44.84),
  `samQ10` = c(99.69, 99.78, 99.75, 99.78, 99.81, 99.79, 99.80, 99.84, 99.76, 99.80,
               99.78, 99.79, 99.78, 99.79, 99.82, 99.80, 99.72, 99.81, 99.69, 99.72,
               99.77, 99.78, 99.76, 99.79, 99.76, 99.80, 99.72, 99.76, 99.79, 99.80),
  `samQ20` = c(98.62, 98.91, 98.84, 98.92, 99.04, 98.95, 98.96, 99.16, 98.86, 98.98,
               98.88, 98.97, 98.86, 98.95, 99.09, 99.00, 98.69, 99.03, 98.65, 98.70,
               98.84, 98.87, 98.87, 98.96, 98.84, 99.02, 98.70, 98.83, 98.94, 99.00),
  `samQ30Read1` = c(95.14, 96.11, 95.17, 95.36, 96.28, 96.29, 96.13, 96.31, 95.84, 96.26,
                    95.92, 96.15, 96.08, 96.17, 96.39, 96.41, 94.84, 96.20, 94.74, 94.84,
                    95.98, 96.19, 96.10, 96.29, 95.98, 96.40, 95.67, 96.22, 96.30, 96.22),
  `samQ30Read2` = c(92.92, 94.37, 94.52, 95.01, 95.25, 94.53, 94.66, 96.04, 94.08, 94.78,
                    94.30, 94.72, 94.06, 94.65, 95.47, 94.74, 93.65, 95.16, 93.33, 93.87,
                    93.93, 94.14, 93.93, 94.57, 93.89, 94.88, 93.02, 93.84, 94.44, 94.91),
  `samQ30Total` = c(94.03, 95.24, 94.84, 95.19, 95.77, 95.41, 95.39, 96.18, 94.96, 95.52,
                    95.11, 95.44, 95.07, 95.41, 95.93, 95.57, 94.25, 95.68, 94.03, 94.36,
                    94.96, 95.16, 95.02, 95.43, 94.94, 95.64, 94.34, 95.03, 95.37, 95.56)
)

# ==========================================
# 3. 数据清洗与配对转换（修复版本）
# ==========================================
df_plot <- data_raw %>%
  mutate(
    Pair_ID = str_remove(`样本名称`, "-[12]$"),
    # 修复：-1结尾的是CZ，-2结尾的是CP
    Method = ifelse(str_detect(`样本名称`, "-1$"), "CZ", "CP")
  ) %>%
  pivot_longer(cols = -c(`样本名称`, Pair_ID, Method), names_to = "Parameter", values_to = "Value") %>%
  mutate(
    Method = factor(Method, levels = c("CP", "CZ")),
    Method_Num = as.numeric(Method)
  )

# ==========================================
# 4. 绘图函数 (Nature 风格) - 修复颜色对应
# ==========================================
create_qc_box_plot <- function(target_param, y_title, unit = NULL) {
  
  sub_data <- df_plot %>% filter(Parameter == target_param)
  
  # 连线数据准备
  line_data <- sub_data %>%
    select(Pair_ID, Method, Value) %>%
    pivot_wider(names_from = Method, values_from = Value)
  
  # 统计检验 - 确保顺序正确
  test_res <- wilcox.test(line_data$CZ, line_data$CP, paired = TRUE, exact = FALSE)
  p_val <- test_res$p.value
  signif_label <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01  ~ "**",
    p_val < 0.05  ~ "*",
    TRUE          ~ "ns"
  )
  
  y_max <- max(sub_data$Value)
  y_min <- min(sub_data$Value)
  y_range <- y_max - y_min
  
  ggplot(sub_data, aes(x = Method_Num, y = Value)) +
    geom_segment(data = line_data, aes(x = 1, xend = 2, y = CP, yend = CZ),  # 修复连线方向
                 color = "grey88", linewidth = 0.5, alpha = 0.7) +
    geom_boxplot(aes(color = factor(Method_Num), group = Method_Num), 
                 width = 0.4, linewidth = 1, outlier.shape = NA, fill = "white") +
    geom_jitter(aes(fill = factor(Method_Num)), shape = 21, size = 3, width = 0.08, color = "white") +
    # 动态显著性标注
    annotate("segment", x = 1, xend = 2, y = y_max + y_range * 0.1, yend = y_max + y_range * 0.1, linewidth = 0.8) +
    annotate("text", x = 1.5, y = y_max + y_range * 0.15, label = signif_label, size = 6, fontface = "bold") +
    scale_x_continuous(breaks = c(1, 2), labels = c("CP", "CZ"), limits = c(0.5, 2.5)) +
    scale_color_manual(values = c("1" = "#2166AC", "2" = "#B2182B")) +  # CP:蓝色, CZ:红色
    scale_fill_manual(values = c("1" = "#4393C3", "2" = "#D6604D")) +    # 填充颜色对应
    labs(x = NULL, y = if (!is.null(unit)) paste0(y_title, " (", unit, ")") else y_title) +
    theme_classic(base_size = 12) +
    theme(
      axis.line = element_line(linewidth = 0.7),
      axis.text = element_text(color = "black", face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

# ==========================================
# 5. 执行循环绘图与保存
# ==========================================
# 参数列表：指标名，显示标题，单位
qc_params <- list(
  c("ReadNum", "Clean Reads", "M"),
  c("BaseNum", "Clean Bases", "Gb"),
  c("samGC", "GC Content", "%"),
  c("samQ10", "Q10 Score", "%"),
  c("samQ20", "Q20 Score", "%"),
  c("samQ30Read1", "Q30 (Read1)", "%"),
  c("samQ30Read2", "Q30 (Read2)", "%"),
  c("samQ30Total", "Total Q30", "%")
)

all_plots <- list()

for (i in seq_along(qc_params)) {
  p <- create_qc_box_plot(qc_params[[i]][1], qc_params[[i]][2], qc_params[[i]][3])
  all_plots[[i]] <- p
}

# 组合 8 张图 (2行4列)
combined_qc_final <- ggarrange(plotlist = all_plots, ncol = 4, nrow = 2, labels = "AUTO")

# 保存结果
ggsave("Figure_QC_Full_Analysis_Corrected.pdf", combined_qc_final, width = 16, height = 9, device = cairo_pdf)

# 也保存PNG格式用于预览
ggsave("Figure_QC_Full_Analysis_Corrected.png", combined_qc_final, width = 16, height = 9, dpi = 300)

message("质控图表已重新绘制并保存！")
message("文件名: Figure_QC_Full_Analysis_Corrected.pdf")
message("文件名: Figure_QC_Full_Analysis_Corrected.png")

# 可选：显示统计摘要
cat("\n=== 统计检验结果汇总 ===\n")
for (i in seq_along(qc_params)) {
  param <- qc_params[[i]][1]
  sub_data <- df_plot %>% filter(Parameter == param)
  line_data <- sub_data %>%
    select(Pair_ID, Method, Value) %>%
    pivot_wider(names_from = Method, values_from = Value)
  
  test_res <- wilcox.test(line_data$CZ, line_data$CP, paired = TRUE, exact = FALSE)
  p_val <- test_res$p.value
  signif_label <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01  ~ "**",
    p_val < 0.05  ~ "*",
    TRUE          ~ "ns"
  )
  
  cat(sprintf("%-15s: p = %.4f %s\n", param, p_val, signif_label))
}