# ============================================================================
# 标题：不同基因组组装方法与纠错策略对 MAGs 回收数量的影响
# 风格：mSystems / ASM Publication Standard (High Contrast, Crisp Typography)
# 输入：Figure 7B.csv
# ============================================================================

# 1. 自动检测并加载必要的 R 包
required_pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "scales", "forcats", "grDevices")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(forcats)
library(grDevices)

# 2. 读取数据与自动降维/转换 (Data Wrangling)
file_path <- "Figure 7B.csv"
if (!file.exists(file_path)) {
  if (file.exists("../表格/Figure 7B.csv")) {
    file_path <- "../表格/Figure 7B.csv"
  } else {
    stop("【错误】未找到 Figure 7B.csv，请确认文件路径是否正确！")
  }
}

raw_data <- read_csv(file_path, show_col_types = FALSE)

# 对所有样本的 MAG 数量求和汇总
summed_data <- raw_data %>%
  select(-name) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Count"
  )

# 3. 映射软件、策略与质量分类
parsed_data <- summed_data %>%
  mutate(
    # 区分质量级别 (_m -> MQ, _h -> HQ)
    Quality = ifelse(grepl("_h$", Metric), "High-quality", "Medium-quality"),
    
    # 提取基础工具名称前缀
    Base_Metric = gsub("_[mh]$", "", Metric),
    
    # 映射组装策略 Method
    Method = case_when(
      grepl("_sr$", Base_Metric) ~ "Long-read + Short-read polishing",
      Base_Metric %in% c("opera_ms", "hybridspades") ~ "Hybrid",
      TRUE ~ "Long-read"
    ),
    
    # 映射软件名称 Software
    Software = case_when(
      grepl("metaflye_2_8_3", Base_Metric) ~ "Flye v2.8.3",
      grepl("metaflye_2_9_6", Base_Metric) ~ "Flye v2.9.6",
      grepl("metaMDBG", Base_Metric)       ~ "metaMDBG",
      grepl("myloasm", Base_Metric)        ~ "MyLoAsm",
      grepl("raven", Base_Metric)          ~ "Raven",
      grepl("opera_ms", Base_Metric)       ~ "OPERA-MS",
      grepl("hybridspades", Base_Metric)   ~ "hybridSPAdes",
      TRUE ~ Base_Metric
    )
  ) %>%
  mutate(
    Method = factor(Method, levels = c("Long-read", "Long-read + Short-read polishing", "Hybrid")),
    Software = factor(Software, levels = c("Flye v2.8.3", "Flye v2.9.6", "metaMDBG", "MyLoAsm", "Raven", "OPERA-MS", "hybridSPAdes")),
    Quality = factor(Quality, levels = c("Medium-quality", "High-quality")) # 下层为 MQ，上层为 HQ
  )

# 4. 配色方案 (ASM/mSystems 经典高对比冷暖调)
quality_colors <- c(
  "High-quality"   = "#1B5A9D",  # 经典普鲁士深蓝
  "Medium-quality" = "#B2182B"   # 经典深朱红
)

# 计算 Y 轴刻度上限 (留出顶部标注空间)
max_total <- parsed_data %>%
  group_by(Software, Method) %>%
  summarise(Total = sum(Count), .groups = "drop") %>%
  pull(Total) %>%
  max()

y_limit <- ceiling(max_total * 1.15 / 100) * 100

# 5. 构建符合 mSystems 标准的 ggplot2 图形
p_mSystems <- ggplot(parsed_data, aes(x = Software, y = Count, fill = Quality)) +
  # 绘制柱状图，细黑边框增加立体清晰度
  geom_bar(stat = "identity", position = "stack", width = 0.72, color = "#111111", linewidth = 0.5) +
  
  # 柱内显示具体的 MAG 数量数值 (当 Count > 30 时显示，防止微小堆叠挤压)
  geom_text(
    aes(label = ifelse(Count > 30, Count, "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  
  # 按组装/纠错策略分面展示
  facet_grid(. ~ Method, scales = "free_x", space = "free_x") +
  
  # 颜色设置与图例反转 (High-quality 置顶)
  scale_fill_manual(
    values = quality_colors,
    name = "MAG Quality",
    guide = guide_legend(reverse = TRUE)
  ) +
  
  # Y 轴尺度设置
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, y_limit, by = 100),
    limits = c(0, y_limit)
  ) +
  
  # 坐标轴与标题
  labs(
    x = "Assembly Tool / Pipeline",
    y = "Total Number of Recovered MAGs"
  ) +
  
  # 6. mSystems/ASM 出版级主题样式定制
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", family = "sans"),
    
    # 坐标轴刻度与实线
    axis.line = element_line(linewidth = 0.7, color = "black"),
    axis.ticks = element_line(linewidth = 0.7, color = "black"),
    axis.ticks.length = unit(0.18, "cm"),
    
    # 坐标轴文本与标签
    axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 10)),
    axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 10)),
    axis.text.y  = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x  = element_text(size = 10.5, color = "black", face = "bold", angle = 35, hjust = 1),
    
    # 分面 (Facet Strip) 头部样式
    strip.background = element_rect(fill = "#ECEFF1", color = "black", linewidth = 0.7),
    strip.text = element_text(face = "bold", size = 11, color = "black"),
    
    # 各分面独立边框与间距
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    panel.spacing = unit(0.8, "lines"),
    
    # 图例配置 (位置居中顶部)
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10.5),
    legend.key.size = unit(0.45, "cm"),
    legend.margin = margin(b = 5),
    
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

# 7. 导出高分辨率文件 (PDF 矢量图与 300 DPI TIFF 图片)
pdf_out  <- "MAGs_Recovery_mSystems_Style.pdf"
tiff_out <- "MAGs_Recovery_mSystems_Style.tiff"

ggsave(filename = pdf_out, plot = p_mSystems, device = cairo_pdf, width = 11, height = 7, units = "in")
ggsave(filename = tiff_out, plot = p_mSystems, device = "tiff", dpi = 300, width = 11, height = 7, units = "in", compression = "lzw")

cat("=== 绘图及导出完成 ===\n")
cat("1. 矢量 PDF 保存至: ", pdf_out, "\n")
cat("2. 300 DPI TIFF 保存至: ", tiff_out, "\n")

# 输出图形
print(p_mSystems)