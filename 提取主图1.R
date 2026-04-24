# ==========================================
# 1. 环境准备
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, grDevices, vegan, patchwork)

# 定义通用的Wilcoxon检验函数
perform_wilcox_test <- function(data, x_var, y_var, group_var = NULL, paired = FALSE) {
  if (paired) {
    if (!is.null(group_var)) {
      # 配对检验，需要宽格式数据
      data_wide <- data %>%
        pivot_wider(names_from = {{x_var}}, values_from = {{y_var}}, id_cols = {{group_var}})
      # 假设只有两个组
      groups <- unique(data[[x_var]])
      if (length(groups) == 2) {
        test_result <- wilcox.test(data_wide[[groups[1]]], data_wide[[groups[2]]], paired = TRUE)
        return(test_result$p.value)
      }
    }
  } else {
    formula <- as.formula(paste(y_var, "~", x_var))
    test_result <- wilcox.test(formula, data = data)
    return(test_result$p.value)
  }
  return(NA)
}

# 通用的显著性标记函数
get_significance_label <- function(p_value) {
  if (is.na(p_value)) return("ns")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("ns")
}

# 修改绘图函数，移除连接到x轴的虚线
modern_paired_plot_with_axis_sig <- function(p_name, y_title, unit = NULL, lines = NULL, tag_label) {
  df <- raw_data_dna %>% filter(Param == p_name) %>% drop_na()
  
  # 计算Wilcoxon检验p值
  p_value <- perform_wilcox_test(df, "Method", "Value", "Sample", paired = TRUE)
  sig_label <- get_significance_label(p_value)
  
  # 计算统计量用于绘图
  y_max <- max(df$Value, na.rm = TRUE)
  y_min <- min(df$Value, na.rm = TRUE)
  y_sig <- y_max + 0.1 * (y_max - y_min)
  
  p <- ggplot(df, aes(x = Method, y = Value)) +
    # 样本连线
    geom_line(aes(group = Sample), color = "grey90", linewidth = 0.5) +
    # 箱线图
    geom_boxplot(aes(fill = Method), width = 0.4, color = "black", alpha = 0.7, 
                 outlier.shape = NA, linewidth = 0.8) +
    # 散点
    geom_point(aes(color = Method), size = 2) +
    # 颜色
    scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    # 标签
    labs(y = if(!is.null(unit)) paste0(y_title, " (", unit, ")") else y_title, 
         x = NULL,
         tag = tag_label) +
    # 主题 - 统一坐标轴大小和粗细
    theme_pubr() + 
    theme(legend.position = "none", 
          axis.title = element_text(face = "bold", size = 12, family = "sans"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
          axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
          axis.line = element_line(color = "black", linewidth = 0.8),
          axis.ticks = element_line(color = "black", linewidth = 0.8),
          axis.ticks.length = unit(0.2, "cm"),
          plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
          plot.margin = margin(5, 5, 5, 5))
  
  # 添加显著性标记（只有横线和星号，没有连接到x轴的虚线）
  p <- p +
    # 显著性横线
    annotate("segment", x = 1, xend = 2, y = y_sig, yend = y_sig, 
             linewidth = 0.8) +
    # 显著性标记文本（没有连接到x轴的虚线）
    annotate("text", x = 1.5, y = y_sig + 0.05 * (y_max - y_min), 
             label = sig_label, size = 5)
  
  # 添加参考线（如果需要）- 修复A260/A230的参考线
  if(!is.null(lines)) {
    if(p_name == "A260_230") {
      # A260/A230: 只保留2.0一条线
      p <- p +
        annotate("segment", x = 0.5, xend = 2.5, y = 2.0, yend = 2.0, 
                 linetype = "dotted", color = "grey40")
    } else if(p_name == "A260_280") {
      # A260/280: 1.7和2.0两条线
      p <- p +
        annotate("segment", x = 0.5, xend = 2.5, y = 1.7, yend = 1.7, 
                 linetype = "dotted", color = "grey40") +
        annotate("segment", x = 0.5, xend = 2.5, y = 2.0, yend = 2.0, 
                 linetype = "dotted", color = "grey40")
    } else {
      # 其他参数
      for(line in lines) {
        p <- p +
          annotate("segment", x = 0.5, xend = 2.5, y = line, yend = line, 
                   linetype = "dotted", color = "grey40")
      }
    }
  }
  
  return(p)
}

# ==========================================
# 2. 生成Panel 1: DNA质量指标 (A-E)
# ==========================================
cat("生成Panel 1: DNA质量指标 (A-E)...\n")

# 2.1 DNA质量数据准备
raw_data_dna <- tibble(
  Sample = c('0919-1', '2121-1', '8915-1', '8366-1', '0927-1', '0942-1', '0980-1', '8373-1', '8343-1', '2725-1', '4141-1', '2835-1', '0996-1', '0934-1', '4143-1'),
  CP_Conc = c(52.2, 182, 384, 278, 97.2, 232, 155, 334, 134, 372, 114, 202, 310, 250, 318),
  CP_Yield = c(1.044, 3.64, 7.68, 5.56, 1.944, 4.64, 3.1, 6.68, 2.68, 7.44, 2.28, 4.04, 6.2, 5, 6.36),
  CP_A260_280 = c(2.15, 2.08, 2.01, 1.97, 2.1, 1.98, 2.09, 2.08, 2.08, 1.92, 2.08, 2.02, 2.07, 1.99, 1.74),
  CP_A260_230 = c(2.24, 1.95, 1.99, 1.71, 2.07, 1.9, 2.09, 1.94, 2.09, 1.63, 2.09, 1.96, 2.06, 1.74, 1.15),
  CP_Frag = c(NA, 20.161, 15.648, 22.458, 23.337, 20.831, 2.089, 19.277, 11.675, 14.283, 24.484, 11.656, 30.957, 15.884, 15.125),
  CZ_Conc = c(46.4, 134, 308, 183, 29.8, 104, 79, 147, 35.4, 216, 36.2, 109, 169, 117, 183),
  CZ_Yield = c(0.928, 2.68, 6.16, 3.66, 0.596, 2.08, 1.58, 2.94, 0.708, 4.32, 0.724, 2.18, 3.38, 2.34, 3.66),
  CZ_A260_280 = c(2.1, 2.07, 2.03, 2.06, 2.15, 2.07, 2.13, 2.01, 2.12, 2.06, 2.13, 2.06, 2.08, 2.09, 2.06),
  CZ_A260_230 = c(2.26, 2.28, 2.12, 2.22, 2.18, 2.13, 2.29, 1.68, 2.25, 2.1, 2.18, 2.1, 2.19, 2.25, 2.22),
  CZ_Frag = c(12.374, 9.423, 9.178, 7.033, 9.31, 12.976, 2.48, 8.466, 8.577, 7.495, 13.173, 9.73, 2.763, 7.755, 9.066)
) %>%
  pivot_longer(cols = -Sample, names_to = "Key", values_to = "Value") %>%
  separate(Key, into = c("Method", "Param"), sep = "_", extra = "merge")

# 2.3 生成子图（使用新函数）
p_a <- modern_paired_plot_with_axis_sig("Conc", "Concentration", "ng/µL", tag_label = "A")
p_b <- modern_paired_plot_with_axis_sig("Yield", "Yield", "µg", tag_label = "B")
p_c <- modern_paired_plot_with_axis_sig("A260_280", "A260/A280", lines = TRUE, tag_label = "C")
p_d <- modern_paired_plot_with_axis_sig("A260_230", "A260/A230", lines = TRUE, tag_label = "D")
p_e <- modern_paired_plot_with_axis_sig("Frag", "Fragment Length", "kb", tag_label = "E")

# 2.4 组合Panel 1（三个图一行）
panel_1_row1 <- ggarrange(p_a, p_b, p_c, ncol = 3, widths = c(1, 1, 1))
panel_1_row2 <- ggarrange(p_d, p_e, NULL, ncol = 3, widths = c(1, 1, 1))
panel_1 <- ggarrange(panel_1_row1, panel_1_row2, nrow = 2, heights = c(1, 1))
ggsave("Figure_1_DNA_Quality.pdf", panel_1, width = 12, height = 7, device = cairo_pdf)

# ==========================================
# 3. 生成Panel 2: 革兰氏染色物种数量 (F)
# ==========================================
cat("生成Panel 2: 革兰氏染色物种数量 (F)...\n")

# 3.1 数据准备
doc_data <- c(
  60, 52, 76, 48, 29, 38, 40, 35, 62, 42, 74, 39,
  77, 59, 94, 53, 75, 72, 97, 64, 60, 61, 65, 47,
  80, 44, 88, 45, 43, 26, 59, 25, 60, 45, 69, 35,
  89, 61, 94, 52, 45, 44, 61, 35, 67, 48, 76, 42,
  52, 26, 67, 26, 68, 32, 78, 32, 67, 44, 79, 41
)

df <- as_tibble(matrix(doc_data, ncol = 4, byrow = TRUE,
                       dimnames = list(NULL, c("cp_pos", "cp_neg", "cz_pos", "cz_neg"))))

df_long <- df %>%
  mutate(Sample = factor(1:n())) %>%
  pivot_longer(-Sample, names_to = "Group", values_to = "Value") %>%
  separate(Group, into = c("Method", "Stain"), sep = "_") %>%
  mutate(
    Method = toupper(Method),
    Stain = recode(Stain,
                   pos = "Gram-positive",
                   neg = "Gram-negative"),
    Stain = factor(Stain, levels = c("Gram-positive", "Gram-negative")),
    X = interaction(Stain, Method, sep = "_"),
    X = factor(X, levels = c(
      "Gram-positive_CP", "Gram-positive_CZ",
      "Gram-negative_CP", "Gram-negative_CZ"
    ))
  )

# 3.2 统计检验
gp_p <- wilcox.test(df$cp_pos, df$cz_pos, paired = TRUE)$p.value
gn_p <- wilcox.test(df$cp_neg, df$cz_neg, paired = TRUE)$p.value

get_sig <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "ns")))
}

gp_label <- get_sig(gp_p)
gn_label <- get_sig(gn_p)

# 3.3 生成Panel 2
y_max <- max(df_long$Value)
sig_height <- y_max + 5

p_f <- ggplot(df_long, aes(x = X, y = Value)) +
  # 样本连线
  geom_line(aes(group = interaction(Sample, Stain)), color = "grey90", linewidth = 0.5) +
  # 箱线图
  geom_boxplot(aes(fill = Method), width = 0.5, color = "black", alpha = 0.7, outlier.shape = NA) +
  # 散点
  geom_point(aes(color = Method), size = 2) +
  # 其他图形元素
  scale_x_discrete(labels = c("CP", "CZ", "CP", "CZ")) +
  scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1.5, y = y_max + 12, label = "Gram-positive", 
           size = 5, fontface = "bold") +
  annotate("text", x = 3.5, y = y_max + 12, label = "Gram-negative", 
           size = 5, fontface = "bold") +
  labs(x = NULL, y = "Number of Species", tag = "F") +
  # 统一坐标轴大小和粗细
  theme_pubr() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 12, family = "sans"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
    axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ylim(0, y_max + 15)

# 添加显著性标记 - 革兰氏阳性（只有横线和星号）
p_f <- p_f +
  annotate("segment", x = 1, xend = 2, y = sig_height, yend = sig_height, 
           linewidth = 0.8) +
  annotate("text", x = 1.5, y = sig_height + 2, label = gp_label, size = 5) +
  # 添加显著性标记 - 革兰氏阴性
  annotate("segment", x = 3, xend = 4, y = sig_height, yend = sig_height, 
           linewidth = 0.8) +
  annotate("text", x = 3.5, y = sig_height + 2, label = gn_label, size = 5)

ggsave("Figure_2_Gram.pdf", p_f, width = 8, height = 5, device = cairo_pdf)

# ==========================================
# 4. 生成Panel 3: 多样性指数 (G-H)
# ==========================================
cat("生成Panel 3: 多样性指数 (G-H)...\n")

# 注意：这里需要提供实际数据文件路径
# 假设数据文件在"../表格/提取主图1_短读数据物种数量以及香农指数.csv"
tryCatch({
  # 4.1 数据准备
  df_raw_div <- read_csv("../表格/提取主图1_短读数据物种数量以及香农指数.csv")
  
  df_div_long <- df_raw_div %>%
    mutate(
      CP_Species = common_species_number + cp_unique_number,
      CZ_Species = common_species_number + cz_unique_number
    ) %>%
    select(sample_name,
           CP_Species, CZ_Species,
           CP_Shannon = cp_shannon,
           CZ_Shannon = cz_shannon) %>%
    pivot_longer(-sample_name, names_to = "Key", values_to = "Value") %>%
    separate(Key, into = c("Method", "Metric")) %>%
    mutate(Method = factor(Method, levels = c("CP", "CZ")))
  
  # 4.2 修改绘图函数
  create_div_plot_with_sig <- function(met, y_lab, tag_lab) {
    df <- df_div_long %>% filter(Metric == met)
    
    # 计算Wilcoxon检验
    p_value <- perform_wilcox_test(df, "Method", "Value", "sample_name", paired = TRUE)
    sig_label <- get_significance_label(p_value)
    
    y_max <- max(df$Value, na.rm = TRUE)
    y_min <- min(df$Value, na.rm = TRUE)
    y_sig <- y_max + 0.1 * (y_max - y_min)
    
    p <- ggplot(df, aes(x = Method, y = Value, fill = Method)) +
      geom_line(aes(group = sample_name), color = "grey85", linewidth = 0.5) +
      geom_boxplot(width = 0.4, color = "black", alpha = 0.7, outlier.shape = NA, linewidth = 0.8) +
      geom_point(shape = 21, size = 2.5, color = "black", stroke = 0.5, alpha = 0.7) +
      scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
      labs(y = y_lab, x = NULL, tag = tag_lab) +
      # 统一坐标轴大小和粗细
      theme_pubr() +
      theme(
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12, family = "sans"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
        axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
        axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 0.8),
        axis.ticks.length = unit(0.2, "cm"),
        plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
        plot.margin = margin(5, 5, 5, 5)
      )
    
    # 添加显著性标记（只有横线和星号）
    p <- p +
      annotate("segment", x = 1, xend = 2, y = y_sig, yend = y_sig, 
               linewidth = 0.8) +
      annotate("text", x = 1.5, y = y_sig + 0.05 * (y_max - y_min), 
               label = sig_label, size = 5)
    
    return(p)
  }
  
  # 4.3 生成子图
  p_g <- create_div_plot_with_sig("Species", "Observed Species", "G")
  p_h <- create_div_plot_with_sig("Shannon", "Shannon Index", "H")
  
  # 4.4 组合Panel 3（两个图一行，中间留空）
  panel_3 <- ggarrange(p_g, p_h, NULL, ncol = 3, widths = c(1, 1, 1))
  ggsave("Figure_3_Diversity.pdf", panel_3, width = 12, height = 5, device = cairo_pdf)
  
}, error = function(e) {
  cat("警告: 无法加载多样性数据文件\n")
  cat("错误信息:", e$message, "\n")
  p_g <- ggplot() + 
    theme_void() +
    labs(tag = "G") +
    theme(plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)))
  p_h <- ggplot() + 
    theme_void() +
    labs(tag = "H") +
    theme(plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)))
  panel_3 <- ggarrange(p_g, p_h, NULL, ncol = 3, widths = c(1, 1, 1))
})

# ==========================================
# 5. 生成Panel 4: Bray-Curtis距离 (I)
# ==========================================
cat("生成Panel 4: Bray-Curtis距离 (I)...\n")

tryCatch({
  # 5.1 数据处理
  data <- read.csv("../表格/提取主图1_短读数据bray_distance.csv")
  samples <- unique(data$sample)
  
  # Within距离
  paired_dist <- map_dbl(samples, function(s) {
    df_s <- data %>%
      filter(sample == s) %>%
      group_by(specie) %>%
      summarise(cp = sum(cp_abundance),
                cz = sum(cz_abundance),
                .groups = "drop") %>%
      mutate(cp = cp/sum(cp), cz = cz/sum(cz))
    mat <- t(as.matrix(df_s[, c("cp", "cz")]))
    as.numeric(vegdist(mat, method = "bray"))
  })
  
  # Between CP距离
  cp_mat <- data %>%
    group_by(sample, specie) %>%
    summarise(val = sum(cp_abundance), .groups = "drop") %>%
    pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
    column_to_rownames("sample")
  cp_mat <- cp_mat / rowSums(cp_mat)
  cp_dist <- as.vector(vegdist(cp_mat, method = "bray"))
  
  # Between CZ距离
  cz_mat <- data %>%
    group_by(sample, specie) %>%
    summarise(val = sum(cz_abundance), .groups = "drop") %>%
    pivot_wider(names_from = specie, values_from = val, values_fill = 0) %>%
    column_to_rownames("sample")
  cz_mat <- cz_mat / rowSums(cz_mat)
  cz_dist <- as.vector(vegdist(cz_mat, method = "bray"))
  
  # 合并数据
  df_plot <- tibble(
    Distance = c(paired_dist, cp_dist, cz_dist),
    Group = factor(
      c(rep("Within", length(paired_dist)),
        rep("Between_CP", length(cp_dist)),
        rep("Between_CZ", length(cz_dist))),
      levels = c("Within", "Between_CP", "Between_CZ")
    )
  )
  
  # 5.2 统计检验
  p1 <- wilcox.test(paired_dist, cp_dist)$p.value
  p2 <- wilcox.test(cp_dist, cz_dist)$p.value
  p3 <- wilcox.test(paired_dist, cz_dist)$p.value
  
  lab1 <- get_sig(p1)
  lab2 <- get_sig(p2)
  lab3 <- get_sig(p3)
  
  # 5.3 生成Panel 4
  y_max <- max(df_plot$Distance)
  
  p_i <- ggplot(df_plot, aes(x = Group, y = Distance)) +
    geom_boxplot(aes(color = Group), fill = "white", width = 0.5, linewidth = 1, outlier.shape = NA) +
    geom_jitter(aes(fill = Group, color = Group), width = 0.15, size = 2.5, 
                shape = 21, stroke = 0.6, alpha = 0.7) +
    # 其他图形元素
    scale_color_manual(values = c("Within" = "#636363", "Between_CP" = "#2166AC", "Between_CZ" = "#B2182B")) +
    scale_fill_manual(values = c("Within" = "#D1D1D1", "Between_CP" = "#4393C3", "Between_CZ" = "#D6604D")) +
    scale_x_discrete(labels = c("Within\n(CP vs CZ)", "Between\n(CP)", "Between\n(CZ)")) +
    labs(y = "Bray-Curtis Dissimilarity", x = NULL, tag = "I") +
    # 统一坐标轴大小和粗细
    theme_classic() +
    theme(
      plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      axis.ticks.length = unit(0.2, "cm"),
      axis.text.x = element_text(size = 12, face = "bold", family = "sans"),
      axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
      axis.title.y = element_text(size = 12, face = "bold", family = "sans"),
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    ) +
    ylim(0, y_max + 0.2)
  
  # 添加显著性标记1: Within vs Between_CP（只有横线和星号）
  p_i <- p_i +
    annotate("segment", x = 1, xend = 2, y = y_max + 0.05, yend = y_max + 0.05,
             linewidth = 0.8) +
    annotate("text", x = 1.5, y = y_max + 0.06, label = lab1, size = 5) +
    # 添加显著性标记2: Between_CP vs Between_CZ
    annotate("segment", x = 2, xend = 3, y = y_max + 0.10, yend = y_max + 0.10,
             linewidth = 0.8) +
    annotate("text", x = 2.5, y = y_max + 0.11, label = lab2, size = 5) +
    # 添加显著性标记3: Within vs Between_CZ
    annotate("segment", x = 1, xend = 3, y = y_max + 0.15, yend = y_max + 0.15,
             linewidth = 0.8) +
    annotate("text", x = 2, y = y_max + 0.16, label = lab3, size = 5)
  
  ggsave("Figure_4_BrayCurtis.pdf", p_i, width = 8, height = 6, device = cairo_pdf)
  
}, error = function(e) {
  cat("警告: 无法加载Bray-Curtis数据文件\n")
  cat("错误信息:", e$message, "\n")
  p_i <- ggplot() + 
    theme_void() +
    labs(tag = "I") +
    theme(plot.tag = element_text(size = 16, face = "bold", margin = margin(0, 0, 0, 5)))
})

# ==========================================
# 6. 组合所有子图 (A-I) - 使用patchwork确保对齐
# ==========================================
cat("组合所有子图 (A-I)...\n")

# 创建占位符图形
create_placeholder <- function(label) {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = paste("Panel", label), size = 10) + 
    theme_void() +
    labs(tag = label) +
    theme(plot.tag = element_text(size = 20, face = "bold", margin = margin(0, 0, 0, 5)))
}

# 确保所有面板都存在
if (!exists("p_a")) p_a <- create_placeholder("A")
if (!exists("p_b")) p_b <- create_placeholder("B")
if (!exists("p_c")) p_c <- create_placeholder("C")
if (!exists("p_d")) p_d <- create_placeholder("D")
if (!exists("p_e")) p_e <- create_placeholder("E")
if (!exists("p_f")) p_f <- create_placeholder("F")
if (!exists("p_g")) p_g <- create_placeholder("G")
if (!exists("p_h")) p_h <- create_placeholder("H")
if (!exists("p_i")) p_i <- create_placeholder("I")

# 使用patchwork组合图形，确保对齐
library(patchwork)

# 第一行: A B C
row1 <- (p_a + p_b + p_c) + 
  plot_layout(ncol = 3, widths = c(1, 1, 1))

# 第二行: D E F
row2 <- (p_d + p_e + p_f) + 
  plot_layout(ncol = 3, widths = c(1, 1, 1))

# 第三行: G H I
row3 <- (p_g + p_h + p_i) + 
  plot_layout(ncol = 3, widths = c(1, 1, 1))

# 垂直组合所有行
final_figure <- row1 / row2 / row3 +
  plot_layout(nrow = 3, heights = c(1, 1, 1))

# 保存最终图形
ggsave("Figure_Complete_A_to_I.pdf", final_figure, 
       width = 15, height = 15, device = cairo_pdf)
ggsave("Figure_Complete_A_to_I.png", final_figure, 
       width = 15, height = 15, dpi = 300, bg = "white")

cat("所有图形生成完成！\n")
cat("已保存文件:\n")
cat("1. Figure_1_DNA_Quality.pdf (A-E)\n")
cat("2. Figure_2_Gram.pdf (F)\n")
cat("3. Figure_3_Diversity.pdf (G-H)\n")
cat("4. Figure_4_BrayCurtis.pdf (I)\n")
cat("5. Figure_Complete_A_to_I.pdf (完整图A-I)\n")
cat("6. Figure_Complete_A_to_I.png (完整图A-I)\n")