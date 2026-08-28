library(UpSetR)
library(readxl) # 用于读取 xls 文件

# 该文件扩展名虽为 .csv，但内部实际上是 Excel (xls) 格式
df <- read_excel("../表格/Figure 8A.csv")

# 动态获取第一行的数据
data <- c(
  "Long_read" = as.numeric(df$Long_read[1]),
  "HYB" = as.numeric(df$HYB[1]),
  "Long_read_sr" = as.numeric(df$Long_read_sr[1]),
  "Long_read&HYB" = as.numeric(df$Long_read_and_HYB[1]),
  "Long_read&Long_read_sr" = as.numeric(df$Long_read_and_Long_read_sr[1]),
  "HYB&Long_read_sr" = as.numeric(df$HYB_and_Long_read_sr[1]),
  "Long_read&HYB&Long_read_sr" = as.numeric(df$all[1])
)

# 输出符合标准的 PDF 矢量图
pdf("./UpSet_plot_mSystems.pdf", width=8, height=6)
upset(fromExpression(data), 
      order.by = "freq",
      main.bar.color = "black",
      sets.bar.color = "black",
      matrix.color = "black",
      point.size = 3.5, 
      line.size = 1, 
      mainbar.y.label = "Intersection Size", 
      sets.x.label = "Set Size", 
      text.scale = c(1.5, 1.3, 1.5, 1.3, 1.3, 1.3))
dev.off()

# 输出 300dpi 高清 PNG
png("./UpSet_plot_mSystems.png", width=8, height=6, units="in", res=300)
upset(fromExpression(data), 
      order.by = "freq",
      main.bar.color = "black",
      sets.bar.color = "black",
      matrix.color = "black",
      point.size = 3.5, 
      line.size = 1, 
      mainbar.y.label = "Intersection Size", 
      sets.x.label = "Set Size", 
      text.scale = c(1.5, 1.3, 1.5, 1.3, 1.3, 1.3))
dev.off()
