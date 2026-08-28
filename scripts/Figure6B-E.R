# 1. 加载库
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(gridExtra)

# 2. 读取数据
file_path <- "../表格/Figure 6B-E.csv"
df <- read.csv(file_path, check.names = FALSE, fill = TRUE, stringsAsFactors = FALSE)

target_cols <- c("File", "N50(kb)", "Max(kb)", "Bases(Gb)", "Passed Classified Bases Rate(%)")

plot_data_full <- df %>%
  select(all_of(target_cols)) %>%
  separate(File, into = c("Sample", "Group"), sep = "-", extra = "drop") %>%
  mutate(Group = gsub("_fitted_raw.fastq.gz", "", Group)) %>%
  mutate(Group = case_when(
    Group == "1" ~ "without_pretreatment",
    Group == "2" ~ "with_pretreatment",
    TRUE ~ Group
  )) %>%
  rename(
    N50_kb = `N50(kb)`, 
    Max_kb = `Max(kb)`, 
    Bases_Gb = `Bases(Gb)`, 
    Passed_Rate = `Passed Classified Bases Rate(%)`
  ) %>%
  mutate(across(c(N50_kb, Max_kb, Bases_Gb, Passed_Rate), as.numeric))

# 因子化分组
plot_data_full$Group <- factor(plot_data_full$Group, 
                               levels = c("without_pretreatment", "with_pretreatment"))

# ----- 清洗：保留完整配对样本（两个组均有值且无NA）-----
complete_samples <- plot_data_full %>%
  group_by(Sample) %>%
  filter(n_distinct(Group) == 2) %>%
  summarise(across(c(N50_kb, Max_kb, Bases_Gb, Passed_Rate), ~ !any(is.na(.)))) %>%
  filter(N50_kb & Max_kb & Bases_Gb & Passed_Rate) %>%
  pull(Sample)

plot_data_clean <- plot_data_full %>% filter(Sample %in% complete_samples)

# 转为宽格式（用于连线）
wide_data <- plot_data_clean %>%
  pivot_wider(
    id_cols = Sample,
    names_from = Group,
    values_from = c(N50_kb, Max_kb, Bases_Gb, Passed_Rate)
  )

# 3. 配色
color_palette  <- c("without_pretreatment" = "#4393C3", "with_pretreatment" = "#D6604D")
border_palette <- c("without_pretreatment" = "#2166AC", "with_pretreatment" = "#B2182B")

# 4. 绘图函数（使用 geom_segment 绘制配对连线，禁用继承）
draw_msystems_boxplot <- function(data_long, data_wide, target_var, y_label, sub_tag) {
  # 构造宽格式中对应指标的前后列名
  col_without <- paste0(target_var, "_without_pretreatment")
  col_with   <- paste0(target_var, "_with_pretreatment")
  
  # 提取用于连线的数据（确保无NA）
  seg_data <- data_wide %>%
    select(Sample, !!col_without := !!sym(col_without), !!col_with := !!sym(col_with)) %>%
    filter(!is.na(!!sym(col_without)) & !is.na(!!sym(col_with)))
  
  # 箱线图 + 散点
  p <- ggplot(data_long, aes(x = Group, y = .data[[target_var]], fill = Group)) +
    geom_boxplot(aes(color = Group), width = 0.4, linewidth = 1, 
                 outlier.shape = NA, alpha = 0.8, 
                 position = position_dodge(width = 0.4)) +
    geom_point(aes(color = Group), 
               position = position_dodge(width = 0.4), size = 2.5) +
    # 使用 geom_segment 绘制连线（精确控制起点终点，且不继承全局 aes）
    geom_segment(
      data = seg_data,
      aes(x = 1, xend = 2, 
          y = !!sym(col_without), yend = !!sym(col_with)),
      color = "grey50", linewidth = 0.6,
      inherit.aes = FALSE   # 关键修正：不继承全局 aes
    ) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = border_palette) +
    # 统计检验（配对 Wilcoxon）
    stat_compare_means(
      comparisons = list(c("without_pretreatment", "with_pretreatment")),
      method = "wilcox.test",
      paired = TRUE,
      label = "p.signif",
      bracket.size = 0.8,
      tip.length = 0.03,
      vjust = 0.5,
      size = 5
    ) +
    labs(y = y_label, x = NULL, tag = sub_tag) +
    theme_classic() +
    theme(
      text = element_text(family = "Arial"),
      plot.tag = element_text(size = 18, face = "bold"),
      plot.tag.position = "topleft",
      axis.line = element_line(linewidth = 0.8, color = "black"),
      axis.title.y = element_text(size = 16, face = "bold", color = "black"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      axis.text.x = element_text(size = 14, face = "bold", color = "black", angle = 15, hjust = 1),
      legend.position = "none"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
    scale_x_discrete(expand = expansion(add = 0.3))
  
  return(p)
}

# 5. 批量生成子图（B~E）
metrics <- list(
  "N50_kb" = "N50 Read Length (kb)",
  "Max_kb" = "Max Read Length (kb)",
  "Bases_Gb" = "Total Bases (Gb)",
  "Passed_Rate" = "Passed Classified Rate (%)"
)

tags <- c("B", "C", "D", "E")
plot_list <- list()

for (i in seq_along(metrics)) {
  m_name <- names(metrics)[i]
  p <- draw_msystems_boxplot(plot_data_clean, wide_data, m_name, metrics[[m_name]], tags[i])
  plot_list[[m_name]] <- p
  ggsave(paste0("msystems_Boxplot_", tags[i], ".pdf"), plot = p, 
         width = 4.5, height = 5.5, device = cairo_pdf)
}

# 6. 组合图
combined_plot <- do.call(grid.arrange, c(plot_list, list(ncol = 2, nrow = 2)))
ggsave("Summary_Metrics_Panel_msystems.pdf", plot = combined_plot, 
       width = 11, height = 10, device = cairo_pdf)