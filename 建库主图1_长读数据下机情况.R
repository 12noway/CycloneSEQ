# 1. 加载必要的库
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)   
library(gridExtra) 

# 2. 读取并清洗数据
file_path <- "../表格/建库主图1_建库长读数据长度分布.csv"
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

plot_data_full$Group <- factor(plot_data_full$Group, 
                               levels = c("without_pretreatment", "with_pretreatment"))

# 3. 定义 Nature/Cell 视觉参数
color_palette  <- c("without_pretreatment" = "#4393C3", "with_pretreatment" = "#D6604D")
border_palette <- c("without_pretreatment" = "#2166AC", "with_pretreatment" = "#B2182B")

# --- 定义通用绘图函数（新增 tag 参数用于标注序号） ---
draw_nature_boxplot <- function(data, target_var, y_label, sub_tag) {
  y_max <- max(data[[target_var]], na.rm = TRUE)
  y_range <- diff(range(data[[target_var]], na.rm = TRUE))
  
  p <- ggplot(data, aes(x = Group, y = .data[[target_var]], fill = Group)) +
    geom_line(aes(group = Sample), color = "grey88", linewidth = 0.5,
              position = position_dodge(0.4)) +
    geom_boxplot(aes(color = Group), width = 0.4, linewidth = 1, 
                 outlier.shape = NA, alpha = 0.8, position = position_dodge(0.4)) +
    geom_point(aes(color = Group), position = position_dodge(0.4), size = 2.5) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = border_palette) +
    
    # 显著性连线
    stat_compare_means(
      comparisons = list(c("without_pretreatment", "with_pretreatment")),
      method = "t.test", paired = TRUE, label = "p.signif",
      bracket.size = 0.8, tip.length = 0.03, vjust = 0.5
    ) +
    
    # 【核心修改】添加子图序号标签 (Tag)
    labs(y = y_label, x = NULL, tag = sub_tag) +
    
    theme_classic() +
    theme(
      text = element_text(family = "Arial"),
      # 序号样式：左上角，Arial，加粗，18pt
      plot.tag = element_text(size = 18, face = "bold"),
      plot.tag.position = "topleft",
      axis.line = element_line(linewidth = 0.8, color = "black"),
      axis.title.y = element_text(size = 16, face = "bold", color = "black"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      axis.text.x = element_text(size = 14, face = "bold", color = "black", angle = 15, hjust = 1),
      legend.position = "none" 
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.3)))
  
  return(p)
}

# 4. 批量生成图表并分配序号 B1, B2, B3, B4
metrics <- list(
  "N50_kb" = "N50 Read Length (kb)",
  "Max_kb" = "Max Read Length (kb)",
  "Bases_Gb" = "Total Bases (Gb)",
  "Passed_Rate" = "Passed Classified Rate (%)"
)

# 定义序号列表
tags <- c("B", "C", "D", "E")
plot_list <- list()

for (i in seq_along(metrics)) {
  m_name <- names(metrics)[i]
  p <- draw_nature_boxplot(plot_data_full, m_name, metrics[[m_name]], tags[i])
  plot_list[[m_name]] <- p
  
  ggsave(paste0("Nature_Boxplot_", tags[i], ".pdf"), plot = p, 
         width = 4.5, height = 5.5, device = cairo_pdf)
}

# 5. 生成汇总组合图
combined_plot <- do.call(grid.arrange, c(plot_list, list(ncol = 2, nrow = 2)))

ggsave("Summary_Metrics_Panel_B.pdf", plot = combined_plot, 
       width = 11, height = 10, device = cairo_pdf)

