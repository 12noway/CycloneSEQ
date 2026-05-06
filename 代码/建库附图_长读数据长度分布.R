# 1. 加载库
library(ggplot2)
library(tidyr)
library(dplyr)

# 2. 读取数据
df <- read.csv("../表格/建库附图_建库长读数据长度分布.csv", check.names = FALSE)

# 3. 数据清洗
plot_data_all <- df %>%
  separate(File, into = c("Sample", "Group"), sep = "-", extra = "drop") %>%
  mutate(Group = gsub("_fitted_raw.fastq.gz", "", Group)) %>%
  mutate(Group = case_when(
    Group == "1" ~ "without_pretreatment",
    Group == "2" ~ "with_pretreatment",
    TRUE ~ Group
  )) %>%
  select(Sample, Group, `1-2kb(%)`, `2-3kb(%)`, `3-4kb(%)`, `4-5kb(%)`, 
         `5-6kb(%)`, `6-7kb(%)`, `7-8kb(%)`, `8-9kb(%)`, `9-10kb(%)`, `>=10kb(%)`) %>%
  pivot_longer(cols = ends_with("%)"), names_to = "Length_Range", values_to = "Percentage")

# 锁定 X 轴长度顺序 (折线图通常从短到长)
level_order <- c("1-2kb(%)", "2-3kb(%)", "3-4kb(%)", "4-5kb(%)", "5-6kb(%)", 
                 "6-7kb(%)", "7-8kb(%)", "8-9kb(%)", "9-10kb(%)", ">=10kb(%)")
plot_data_all$Length_Range <- factor(plot_data_all$Length_Range, levels = level_order)

# 4. 定义绘图函数 (Nature 风格折线图)
draw_nature_line_plot <- function(data, is_facet = FALSE) {
  p <- ggplot(data, aes(x = Length_Range, y = Percentage, color = Group, group = Group)) +
    # 绘制折线和点
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    # 设定 Nature 风格配色 (深蓝 vs 深红)
    scale_color_manual(values = c("without_pretreatment" = "#2166AC", 
                                  "with_pretreatment" = "#B2182B")) +
    scale_y_continuous(limits = c(0, max(data$Percentage) * 1.1), expand = c(0, 0.5)) +
    labs(y = "Percentage of Reads (%)", x = "Read Length Range", color = NULL) +
    theme_classic() +
    theme(
      text = element_text(family = "Arial"),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      # X 轴标签倾斜以防重叠
      axis.text.x = element_text(size = 12, face = "bold", color = "black", angle = 45, hjust = 1),
      axis.line = element_line(linewidth = 0.8, color = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "top"
    )
  
  if(is_facet) {
    p <- p + facet_wrap(~Sample, scales = "free_y") # 汇总图建议允许 y 轴独立，观察趋势更清
  }
  return(p)
}

# --- 任务 1: 每个样本单独保存一个 PDF ---
samples <- unique(plot_data_all$Sample)

for (s in samples) {
  sample_df <- plot_data_all %>% filter(Sample == s)
  p_single <- draw_nature_line_plot(sample_df) + labs(title = paste("Sample:", s))
  
  file_name <- paste0("Line_Sample_", s, ".pdf")
  ggsave(file_name, plot = p_single, width = 7, height = 6, device = cairo_pdf)
  message(paste("Saved:", file_name))
}

# --- 任务 2: 生成汇总图 ---
p_summary <- draw_nature_line_plot(plot_data_all, is_facet = TRUE)

ggsave("Summary_All_Samples_Lines.pdf", plot = p_summary, 
       width = 16, height = 8, device = cairo_pdf, limitsize = FALSE)

message("All line plots completed.")