# ============================================================================
# 标题：不同基因组组装方法与纠错策略对中高质量MAGs回收数量的影响
# 风格：Nature/Cell Publication Style (High Contrast, High Clarity)
# ============================================================================

# 1. 加载必要的R包
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)
library(forcats)

# 2. 构建与处理数据
plot_data <- data.frame(
  Method = factor(c(rep("TGS", 3), rep("TGS+NGS polishing", 3), rep("HYB", 2)),
                  levels = c("TGS", "TGS+NGS polishing", "HYB")),
  Software = factor(c("Flye", "MetaMDBG", "MyLoAsm",
                      "Flye", "MetaMDBG", "MyLoAsm",
                      "HybridSPAdes", "OPERA-MS"),
                    levels = c("Flye", "MetaMDBG", "MyLoAsm", "HybridSPAdes", "OPERA-MS")),
  NC_MAGs = c(120, 156, 101, 173, 207, 139, 190, 194),
  MQ_MAGs = c(226, 201, 179, 223, 204, 232, 258, 183)
)

plot_data_long <- plot_data %>%
  pivot_longer(cols = c(NC_MAGs, MQ_MAGs),
               names_to = "Quality",
               values_to = "Count",
               names_pattern = "(.*)_MAGs") %>%
  mutate(Quality = factor(Quality, 
                          levels = c("MQ", "NC"), 
                          labels = c("Medium-quality", "Near-complete")))

# 3. 定义 Nature 风格配色方案 (Cold-Warm 对比)
# Near-complete: 深蓝色系 (#2166AC)
# Medium-quality: 深红色系 (#B2182B)
quality_colors <- c("Near-complete" = "#2166AC", 
                    "Medium-quality" = "#B2182B")

# 4. 绘图核心代码
p_nature <- ggplot(plot_data_long, aes(x = Software, y = Count, fill = Quality)) +
  # 绘制堆积柱状图：边框设为黑色，增加 0.6 的粗度确保轮廓坚挺
  geom_bar(stat = "identity", position = "stack", width = 0.75, color = "black", linewidth = 0.6) +
  
  # 分面设置
  facet_grid(. ~ Method, scales = "free_x", space = "free_x") +
  
  # 颜色映射与图例顺序
  scale_fill_manual(values = quality_colors,
                    name = "MAG Quality",
                    guide = guide_legend(reverse = TRUE)) +
  
  # Y 轴优化：严谨的 SCI 格式，刻度加粗
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     breaks = seq(0, 500, by = 100),
                     limits = c(0, 500)) +
  
  # 标签设定
  labs(x = "Assembly Tool",
       y = "Number of recovered MAGs") +
  
  # 5. 应用 Nature 出版级主题定制
  theme_classic() + # 基础：纯白背景，无网格线
  theme(
    # 字体设定：Arial/Sans-serif
    text = element_text(color = "black"),
    
    # 坐标轴：加粗至 0.8 unit，确保缩小排版依然清晰
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.ticks = element_line(linewidth = 0.8, color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    
    # 标题与刻度文本：遵循 16pt/14pt 规范
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, color = "black", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), # 45度倾斜避免重叠
    
    # 分面条 (Strip) 样式：Nature 风格的灰色背景与黑色边框
    strip.background = element_rect(fill = "#E0E0E0", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 13),
    
    # 分组边框：强化每个分面的独立性
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.spacing = unit(1, "lines"),
    
    # 图例：置于顶部，精简间距
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    
    # 移除任何残余网格
    panel.grid = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

# ----------------------------
# 6. 保存导出 (cairo_pdf)
# ----------------------------
# 导出为 8x6 英寸，适合大多数 SCI 期刊通栏或半通栏排版
ggsave(filename = "MAGs_Yield_Nature_Style.pdf",
       plot = p_nature,
       device = cairo_pdf,
       width = 10, height = 7,
       units = "in")

print(p_nature)