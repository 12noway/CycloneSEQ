# ============================================================================
# 物种鉴定分析 - mSystems 风格 UpSet 图
# 数据来源: Figure 7C.csv (读取交集表格直接绘图)
# 过滤条件: 仅显示交集物种数 >= 4 的组合
# ============================================================================

# 1. 检测并自动安装/加载必要的 R 包
required_packages <- c("UpSetR", "grDevices")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

library(UpSetR)
library(grDevices)

# 2. 读取交集汇总数据表
table_path <- "Figure 7C.csv"

if (!file.exists(table_path)) {
  if (file.exists("../表格/Figure 7C.csv")) {
    table_path <- "../表格/Figure 7C.csv"
  } else {
    stop("【错误】未找到 Figure 7C.csv 文件，请检查路径！")
  }
}

summary_df <- read.csv(table_path, stringsAsFactors = FALSE)

# 3. 数据解析：从表格提取所有工具/集合名称与物种分布
# ----------------------------------------------------------------------------
min_intersection_size <- 4  # 筛选阈值：>= 4

# 展开提取每个交集包含的物种明细与工具名称
species_record_list <- list()
all_tools <- c()

for (i in 1:nrow(summary_df)) {
  comb_str <- summary_df$Intersection_Combination[i]
  tools <- trimws(unlist(strsplit(comb_str, " & ")))
  all_tools <- unique(c(all_tools, tools))
  
  # 解析物种列表
  sp_list <- trimws(unlist(strsplit(summary_df$Species_List[i], ";")))
  sp_list <- sp_list[sp_list != ""]
  
  for (sp in sp_list) {
    species_record_list[[sp]] <- tools
  }
}

all_species <- names(species_record_list)
all_tools   <- sort(all_tools)

# 构建 0/1 矩阵 (UpSetR 必需的基本输入格式)
upset_matrix <- as.data.frame(matrix(0, nrow = length(all_species), ncol = length(all_tools)))
colnames(upset_matrix) <- all_tools
rownames(upset_matrix) <- all_species

for (sp in all_species) {
  tools_present <- species_record_list[[sp]]
  upset_matrix[sp, tools_present] <- 1
}

cat("=== 数据解析完成 ===\n")
cat("解析到的评估工具数量 (Sets):", length(all_tools), "\n")
cat("总独特物种数 (Total Species):", length(all_species), "\n")

# 4. 筛选满足 Intersection_Size >= 4 的组合列表
# ----------------------------------------------------------------------------
filtered_df <- summary_df[summary_df$Intersection_Size >= min_intersection_size, ]

if (nrow(filtered_df) == 0) {
  stop(paste0("【警告】表中没有 Intersection_Size >= ", min_intersection_size, " 的数据！"))
}

# 还原为 UpSetR 所需的 list 结构
valid_intersections <- lapply(strsplit(filtered_df$Intersection_Combination, " & "), function(x) trimws(x))

# 计算最高的交集柱高度，动态留出 Y 轴空间（防止柱顶数字被顶端截断）
max_bar_height <- max(filtered_df$Intersection_Size)
y_axis_max     <- ceiling(max_bar_height * 1.18)  # 上浮 18% 留出数字空间

cat("满足 >=", min_intersection_size, "的交集组合数:", length(valid_intersections), "\n")
cat("最高柱交集物种数:", max_bar_height, " (Y轴上限设置为:", y_axis_max, ")\n\n")


# 5. 图形参数与 mSystems 顶级期刊配色方案
# ----------------------------------------------------------------------------
color_main_bar <- "#1B263B"   # 主交集柱：深钛黑蓝 (Slate Navy)
color_set_bar  <- "#2B5C8F"   # 左侧集合柱：经典普鲁士蓝 (Prussian Blue)
color_matrix   <- "#1B263B"   # 点阵颜色
color_bg_shade <- "#F4F5F7"   # 背景交替阴影

# 字体比例配置 c(交集Y轴名, 交集Y轴刻度, 侧边X轴名, 侧边X轴刻度, 组名, 柱顶数字)
font_scale_config <- c(1.6, 1.3, 1.5, 1.3, 1.4, 1.3)


# 6. 封装绘图逻辑
# ----------------------------------------------------------------------------
draw_mSystems_upset <- function() {
  upset(
    upset_matrix,
    nsets = ncol(upset_matrix),
    
    # 读取筛选后的交集组合
    intersections = valid_intersections,
    order.by = "freq",
    decreasing = TRUE,
    
    # 柱顶数字完整显示核心设置
    show.numbers = "yes",                 # 显式显示柱顶数字 ("yes")
    number.angles = 0,                    # 数字保持水平 (0度)
    mainbar.y.max = y_axis_max,           # 动态扩展 Y 轴上限，防止柱顶数字被边缘裁剪
    
    # 布局与比例优化
    mb.ratio = c(0.58, 0.42),             # 主柱图与点阵区比例
    text.scale = font_scale_config,
    point.size = 3.8,                     # 组合点大小
    line.size = 1.2,                      # 连接线粗细
    
    # 配色方案
    main.bar.color = color_main_bar,
    sets.bar.color = color_set_bar,
    matrix.color   = color_matrix,
    shade.color    = color_bg_shade,
    shade.alpha    = 0.4,
    
    # 坐标轴标签 (ASM / mSystems 规范)
    mainbar.y.label = "Intersection Size (Species Count)",
    sets.x.label    = "Total Species Detected Per Tool"
  )
}


# 7. 导出矢量 PDF 文件 (可直接用于 InDesign/Illustrator 组图)
pdf_filename <- "Species_Intersection_UpSet_Cutoff4.pdf"
cairo_pdf(pdf_filename, width = 11, height = 8, pointsize = 12)
draw_mSystems_upset()
dev.off()


# 8. 导出 300 DPI 高清 TIFF 图像 (符合 mSystems 投稿线上系统标准)
tiff_filename <- "Species_Intersection_UpSet_Cutoff4.tiff"
tiff(tiff_filename, width = 11, height = 8, units = "in", res = 300, compression = "lzw")
draw_mSystems_upset()
dev.off()

cat("=== 绘图完成并成功导出 ===\n")
cat("1. 矢量 PDF 保存至: ", pdf_filename, "\n")
cat("2. 300 DPI TIFF 保存至: ", tiff_filename, "\n")