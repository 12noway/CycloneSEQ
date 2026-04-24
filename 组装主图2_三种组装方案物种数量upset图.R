# ============================================================================
# MAGs 组装集合分析 - Nature/Cell 风格 UpSet 图
# ============================================================================

if (!require("UpSetR")) install.packages("UpSetR")
library(UpSetR)
library(grDevices)

# 1. 定义表达量 (保持逻辑一致)
expressions <- c(
  TGS = 0,
  TGS_nextpolish = 17,
  HYB = 2,
  "TGS&TGS_nextpolish" = 5,
  "TGS&HYB" = 11,
  "TGS_nextpolish&HYB" = 18,
  "TGS&TGS_nextpolish&HYB" = 261
)

# 2. 导出设置 (使用 cairo_pdf 以获得更好的字体嵌入效果)
# Nature 建议宽度在 8-10 inch 左右，适合单栏或 1.5 栏排版
cairo_pdf("MAGs_Intersection_UpSet_NatureStyle.pdf", width = 10, height = 7)

# 3. 绘图核心
upset(
  fromExpression(expressions),
  nsets = 3,
  
  # --- 排序与布局 (Logic & Ratio) ---
  order.by = "freq",            # 按频率降序排列核心交集
  decreasing = TRUE,
  mb.ratio = c(0.6, 0.4),       # 稍微增加上方柱状图高度，突出 261 这个核心数值
  
  # --- 几何细节 (Geometry) ---
  point.size = 5,               # 增大圆点，增强视觉锚点
  line.size = 1.5,              # 加粗连接线，体现工业质感
  
  # --- 标签设置 (Labels) ---
  mainbar.y.label = "Number of MAGs",
  sets.x.label = "Total MAGs Per Method",
  
  # --- 字体缩放 (Typography - SCI 大字体标准) ---
  # 顺序：c(交集轴名, 交集刻度, 集合轴名, 集合刻度, 集合名称, 柱状图上方数字)
  text.scale = c(2.0, 1.5, 1.8, 1.5, 2.2, 1.8), 
  
  # --- 配色方案 (Nature 风格高对比度) ---
  # 左侧条形图：采用经典的学术三原色调，深邃且饱和度适中
  sets.bar.color = c("#2166AC", "#B2182B", "#1A9850"), 
  main.bar.color = "#222222",   # 主柱状图使用碳黑色
  matrix.color = "#222222",     # 点阵使用碳黑色
  shade.color = "#F5F5F5",      # 极浅灰背景阴影
  
  # --- 辅助设置 ---
  scale.intersections = "identity",
  number.angles = 0             # 柱头数字保持水平，易于阅读
)

dev.off()

cat("\n=== 样式设计说明：Gemini 说 ===\n")
cat("1. 配色：左侧使用了 Cold-Warm-Nature (蓝/红/绿) 组合，便于在正文中区分不同组装策略。\n")
cat("2. 层次：主柱状图统一为黑色，旨在引导读者的注意力集中在 '核心共有物种 (261)' 上。\n")
cat("3. 细节：去除了 DINGBATS 依赖，改用 Cairo 引擎导出，确保在任何 PDF 阅读器中字体均不跑飞。\n")