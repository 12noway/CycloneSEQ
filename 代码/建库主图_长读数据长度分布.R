# 1. 加载库
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)

# 2. 读取数据
file_path <- "../表格/建库附图_建库长读数据长度分布.csv"
df <- read.csv(file_path, check.names = FALSE)

# 3. 数据清洗与组别重命名
plot_data_all <- df %>%
  separate(File, into = c("Sample", "Group"), sep = "-", extra = "drop") %>%
  mutate(Group = gsub("_fitted_raw.fastq.gz", "", Group)) %>%
  # 替换组别名称
  mutate(Group = case_when(
    Group == "1" ~ "without_pretreatment",
    Group == "2" ~ "with_pretreatment",
    TRUE ~ Group
  )) %>%
  select(Sample, Group, `1-2kb(%)`, `2-3kb(%)`, `3-4kb(%)`, `4-5kb(%)`, 
         `5-6kb(%)`, `6-7kb(%)`, `7-8kb(%)`, `8-9kb(%)`, `9-10kb(%)`, `>=10kb(%)`) %>%
  pivot_longer(cols = ends_with("%)"), names_to = "Length_Range", values_to = "Percentage")

# 锁定长度分布顺序（从大到小堆叠）
level_order <- c("1-2kb(%)", "2-3kb(%)", "3-4kb(%)", "4-5kb(%)", "5-6kb(%)", 
                 "6-7kb(%)", "7-8kb(%)", "8-9kb(%)", "9-10kb(%)", ">=10kb(%)")
plot_data_all$Length_Range <- factor(plot_data_all$Length_Range, levels = rev(level_order))
plot_data_all$Group <- factor(plot_data_all$Group, levels = c("without_pretreatment", "with_pretreatment"))

# 定义 Nature 风格配色 (Spectral 渐变色)
my_colors <- colorRampPalette(brewer.pal(11, "Spectral"))(10)

# 4. 定义通用的绘图函数
draw_nature_stacked_plot <- function(data, is_facet = FALSE, figure_tag = NULL) {
  p <- ggplot(data, aes(x = Group, y = Percentage, fill = Length_Range)) +
    geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 0.2) +
    scale_fill_manual(values = my_colors) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    # 【核心修改】添加序号标签 A
    labs(y = "Percentage of Reads (%)", x = NULL, fill = "Read Length", tag = figure_tag) +
    theme_classic() +
    theme(
      text = element_text(family = "Arial"),
      # 序号样式：左上角，18pt，加粗
      plot.tag = element_text(size = 18, face = "bold"),
      plot.tag.position = c(0.01, 0.98), # 精准定位在左上角
      axis.title.y = element_text(size = 16, face = "bold", color = "black"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      axis.text.x = element_text(size = 12, face = "bold", color = "black", angle = 45, hjust = 1),
      axis.line = element_line(linewidth = 0.8, color = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "right",
      plot.margin = unit(c(1, 1, 1, 1), "cm") # 留出边距防止序号被切掉
    )
  
  if(is_facet) {
    p <- p + facet_wrap(~Sample, nrow = 1)
  }
  return(p)
}

# --- 任务 1: 每个样本单独保存 PDF (带序号 A1, A2...) ---
samples <- unique(plot_data_all$Sample)

for (i in seq_along(samples)) {
  s <- samples[i]
  sample_df <- plot_data_all %>% filter(Sample == s)
  # 为单图标记序号如 A1, A2
  p_single <- draw_nature_stacked_plot(sample_df, figure_tag = paste0("A", i)) + 
    labs(title = paste("Sample:", s))
  
  file_name <- paste0("Sample_", s, "_Panel_A", i, ".pdf")
  ggsave(file_name, plot = p_single, width = 5, height = 7, device = cairo_pdf)
}

# --- 任务 2: 生成汇总 PDF (整体标为序号 A) ---
p_summary <- draw_nature_stacked_plot(plot_data_all, is_facet = TRUE, figure_tag = "A")

# 导出汇总图
ggsave("Summary_All_Samples_Panel_A.pdf", plot = p_summary, 
       width = 16, height = 8, device = cairo_pdf, limitsize = FALSE)

message("已生成带有序号 A 的堆叠柱状图汇总 PDF！")