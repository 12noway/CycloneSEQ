# ============================================================================
# MAGs 组装集合分析 - Nature/Cell 风格 UpSet 图 (Revised)
# ============================================================================

if (!require("UpSetR")) install.packages("UpSetR")
library(UpSetR)
library(grDevices)

# 1. 定义表达量 (修改术语：TGS -> Long-read, NGS -> Short-read)
# 注意：为了保持 UpSet 逻辑，变量名中的特殊字符建议用引号包裹
expressions <- c(
  "Long-read" = 0,
  "Long-read (Short-read polished)" = 17,
  "HYB" = 2,
  "Long-read&Long-read (Short-read polished)" = 5,
  "Long-read&HYB" = 11,
  "Long-read (Short-read polished)&HYB" = 18,
  "Long-read&Long-read (Short-read polished)&HYB" = 261
)

# 2. 导出设置 (使用 cairo_pdf 确保跨平台字体表现一致)
# 宽度 10 inch, 高度 7 inch 适合 Nature 标准的宽版插图
cairo_pdf("MAGs_Intersection_UpSet_LongRead_Style.pdf", width = 10, height = 7)

# 3. 绘图核心
upset(
  fromExpression(expressions),
  nsets = 3,
  
  # --- 排序与布局 (Logic & Ratio) ---
  order.by = "freq",             # 按交集大小降序排列
  decreasing = TRUE,
  mb.ratio = c(0.6, 0.4),        # 调整主图与点阵比例，给上方柱状图更多空间
  
  # --- 几何细节 (Geometry) ---
  point.size = 5,                # 显著的连接点
  line.size = 1.2,               # 连接线粗细适中
  
  # --- 标签设置 (Labels) ---
  mainbar.y.label = "Number of MAGs",
  sets.x.label = "Total MAGs Per Method",
  
  # --- 字体缩放 (Typography) ---
  # 顺序：c(交集轴标题, 交集刻度, 集合轴标题, 集合刻度, 集合名称, 柱头数字)
  text.scale = c(1.8, 1.3, 1.5, 1.3, 1.8, 1.5), 
  
  # --- 配色方案 (Nature 风格高对比度) ---
  # 分别对应 Long-read, Long-read (polished), HYB 的条形颜色
  sets.bar.color = c("#2166AC", "#B2182B", "#1A9850"), 
  main.bar.color = "#222222",    # 主柱状图采用碳黑
  matrix.color = "#222222",      # 点阵一致性
  shade.color = "#F5F5F5",       # 浅灰色背景阴影增强层次感
  
  # --- 辅助设置 ---
  scale.intersections = "identity",
  number.angles = 0              # 柱状图上方数字保持水平
)

dev.off()

cat("\n=== 术语更新说明：Gemini 说 ===\n")
cat("1. 替换完成：TGS 已更新为 'Long-read'，纠错策略已明确标注为 'Short-read polished'。\n")
cat("2. 导出建议：使用 cairo_pdf 生成的文件在 Adobe Illustrator 中二次编辑时，文本层会更清晰。\n")
cat("3. 视觉焦点：261 个共有 MAGs 的核心地位在 'Long-read' 体系下显得更加严谨。\n")