# ============================================================================
# 物种鉴定分析 - Nature/Cell 风格 UpSet 图 (修复版)
# ============================================================================

# 1. 加载必要的包
if (!require("UpSetR")) install.packages("UpSetR")
library(UpSetR)
library(grDevices)

# 2. 读取与预处理数据
# 注意：请确保路径 "../表格/组装主图1_组装物种交集upset.csv" 正确
raw_data <- read.csv("../表格/组装主图_组装物种交集upset.csv", 
                     stringsAsFactors = FALSE, 
                     na.strings = c("", "NA"))

# 清理列表逻辑
clean_list <- lapply(raw_data, function(x) {
  x_clean <- x[!is.na(x) & x != ""]
  return(unique(x_clean))
})

# 转换为 0/1 矩阵 (UpSetR 标准输入)
all_species <- unique(unlist(clean_list))
upset_data <- as.data.frame(sapply(clean_list, function(x) {
  as.integer(all_species %in% x)
}))
rownames(upset_data) <- all_species

# 3. 生成并保存图形 (Nature/Cell 风格定制)
# ----------------------------------------------------------------------------
# 核心修复：移除了 line.alpha 参数
# 视觉强化：增加了点的大小和连线粗细，确保 SCI 论文缩放后的可读性
# ----------------------------------------------------------------------------

cairo_pdf("Bacterial_Species_UpSet_Plot_NatureStyle.pdf", width = 12, height = 9)

upset(upset_data, 
      nsets = ncol(upset_data), 
      
      # --- 排序与比例 ---
      order.by = "freq",            
      decreasing = TRUE,
      mb.ratio = c(0.55, 0.45),      
      
      # --- 字体大小调节 (SCI 论文建议大字体) ---
      # c(交集轴名, 交集刻度, 集合轴名, 集合刻度, 集合名称, 柱状图上方数字)
      text.scale = c(2.0, 1.5, 1.8, 1.5, 2.2, 1.6), 
      
      # --- 图形细节 ---
      point.size = 4.5,             
      line.size = 1.4,              
      number.angles = 0,            
      
      # --- 配色方案 (Nature 风格冷色调) ---
      main.bar.color = "#222222",   # 碳黑色主柱
      sets.bar.color = "#285291",   # 经典蓝侧边柱
      matrix.color = "#222222",     # 点阵颜色
      shade.color = "#F0F0F0",      # 矩阵底色阴影
      
      # --- 标签设置 ---
      mainbar.y.label = "Intersection Size (Species)", 
      sets.x.label = "Total Species Per Tool"
)

dev.off()

# 4. 终端状态反馈
cat("\n=== 修复成功 ===")
cat("\n错误原因：'line.alpha' 不是 UpSetR 的原生参数，已移除。")
cat("\n文件保存：Bacterial_Species_UpSet_Plot_NatureStyle.pdf")
cat("\n建议：若需要进一步调整连线透明度，可在 Illustrator 中打开生成的 PDF 进行微调。\n")