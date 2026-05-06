# ==========================================
# 1. 环境准备
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggsci, grDevices)

# ==========================================
# 2. 原始数据录入 (物种数量统计数据)
# ==========================================
# 基于你提供的样本名称和物种分类数据
samples_raw <- c('25S02882835', '25S02898343', '25S02898366', '25S02898373', '25S03142121', 
                 '25S03142725', '25S03148915', '25S03150919', '25S03150927', '25S03150934', 
                 '25S03150942', '25S03150980', '25S03150996', '25S03154141', '25S03154143')
sample_labels <- paste0("S", 1:15)

shared_counts <- c(239, 132, 219, 325, 324, 260, 329, 121, 169, 349, 153, 207, 161, 182, 257)
unique_cz <- c(10, 15, 8, 42, 15, 39, 20, 5, 21, 55, 23, 27, 10, 7, 16)
unique_cp <- c(41, 49, 43, 77, 77, 26, 41, 81, 32, 33, 51, 42, 52, 36, 41)

# 数据转换逻辑
df_counts <- data.frame(
  Sample_Raw = rep(samples_raw, 2), 
  Method = rep(c("CZ", "CP"), each = 15),
  Shared = rep(shared_counts, 2), 
  Unique = c(unique_cz, unique_cp)
) %>%
  mutate(Sample_ID = factor(Sample_Raw, levels = samples_raw, labels = sample_labels)) %>%
  pivot_longer(cols = c(Shared, Unique), names_to = "Category", values_to = "Count") %>%
  # 填充逻辑：共有部分灰色，独有部分蓝/红
  mutate(Fill_Group = factor(case_when(
    Category == "Shared" ~ "Shared Species", 
    Category == "Unique" & Method == "CZ" ~ "CZ Unique",
    Category == "Unique" & Method == "CP" ~ "CP Unique"
  ), levels = c("CP Unique", "CZ Unique", "Shared Species")),
  x_numeric = as.numeric(Sample_ID),
  # 柱子左右偏移避让设计
  x_final = ifelse(Method == "CP", x_numeric - 0.22, x_numeric + 0.22))

# ==========================================
# 3. 样式参数定义 (严格遵循 Nature 标准)
# ==========================================
# 配色方案
nature_pal <- c(
  "Shared Species" = "#D1D1D1",  # 灰色 (中性)
  "CZ Unique"      = "#B2182B",  # 深红色
  "CP Unique"      = "#2166AC"   # 深蓝色
)

# ==========================================
# 4. 绘图执行
# ==========================================
p_species <- ggplot(df_counts, aes(x = x_final, y = Count, fill = Fill_Group)) +
  # 绘制堆叠柱状图
  geom_col(position = "stack", width = 0.4, color = "black", linewidth = 0.6) +
  # 设置坐标轴
  scale_x_continuous(breaks = 1:15, labels = sample_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = nature_pal) +
  # 核心主题定制
  theme_classic() +
  theme(
    text = element_text(family = "sans"),
    # 坐标轴标题：16pt, 加粗 (Bold)
    axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 12)),
    axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 12)),
    # 刻度标签：14pt, 加粗 (Bold)
    axis.text = element_text(size = 14, face = "bold", color = "black"),
    # 轴线：加粗至 0.8
    axis.line = element_line(linewidth = 0.8, color = "black"),
    # 图例设置
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(x = "Samples", y = "Number of Species")

# ==========================================
# 5. 高清导出 (Vector PDF)
# ==========================================
# 尺寸：8.5 x 6 英寸 (适合 SCI 论文单页排版)
ggsave("Figure_Species_Counts_Final.pdf", 
       plot = p_species, 
       width = 8.5, 
       height = 6, 
       device = cairo_pdf)

message(">>> 物种数量统计图绘制完成！文件已保存为：Figure_Species_Counts_Final.pdf")