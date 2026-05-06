library(dplyr)
library(fmsb)
library(scales)

data <- read.csv("../表格/组装主图_组装MAGs指标统计.csv")

combo1_methods <- c("flye", "metamdbg", "myloasm")
combo2_methods <- c("flye_nextpolish", "metamdbg_nextpolish", "myloasm_nextpolish")
combo3_methods <- c("hybridspades", "opera_ms")

normalize <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  (x - min(x)) / (max(x) - min(x))
}

reverse_normalize <- function(x) {
  if (max(x) == min(x)) return(rep(0.5, length(x)))
  1 - ((x - min(x)) / (max(x) - min(x)))
}

samples <- unique(data$sample)

combo1_colors <- c("#D73027", "#4575B4", "#1A9850")
combo2_colors <- c("#984EA3", "#FF7F00", "#FFD92F")
combo3_colors <- c("#A65628", "#F781BF")

pdf("Assembly_Radar_Charts_FINAL_clean.pdf", width = 14, height = 14)

plot_radar <- function(df, colors, title_text, sample_name) {
  
  df_norm <- df %>%
    mutate(
      Largest = normalize(largest_length),
      N50 = normalize(N50),
      Total = normalize(total_length),
      Contam = reverse_normalize(contamination),
      HQMAGs = normalize(hm_mags)
    ) %>%
    select(assembly, Largest, N50, Total, Contam, HQMAGs)
  
  radar_data <- as.data.frame(df_norm[, -1])
  rownames(radar_data) <- df_norm$assembly
  
  max_min <- data.frame(Largest=c(1,0), N50=c(1,0), Total=c(1,0),
                        Contam=c(1,0), HQMAGs=c(1,0))
  radar_df <- rbind(max_min, radar_data)
  
  par(mar = c(5, 5, 5, 8))
  par(xpd = TRUE)
  
  radarchart(
    radar_df,
    axistype = 1,
    seg = 5,
    vlabels = c("Largest length", "N50", "Total length",
                "Contamination ↓", "HM-MAGs"),
    vlcex = 1.6,
    pcol = colors[1:nrow(radar_data)],
    pfcol = alpha(colors[1:nrow(radar_data)], 0.25),
    plwd = 3,
    cglcol = "grey85",
    cglty = 1,
    cglwd = 1,
    axislabcol = NA,             # ❌ remove % labels
    caxislabels = rep("", 6)     # ❌ remove numbers
  )
  
  mtext(paste("Sample", sample_name, "-", title_text),
        side = 3, line = 2, cex = 1.8, font = 2)
  
  legend("right",
         inset = c(-0.45, 0),
         legend = rownames(radar_data),
         col = colors[1:nrow(radar_data)],
         pch = 16,
         pt.cex = 3,
         cex = 1.6,
         bty = "n")
}

for (sample_name in samples) {
  
  par(mfrow = c(1,3))
  
  df1 <- data %>% filter(sample == sample_name, assembly %in% combo1_methods)
  if (nrow(df1) > 0) plot_radar(df1, combo1_colors, "Long-read", sample_name)
  
  df2 <- data %>% filter(sample == sample_name, assembly %in% combo2_methods)
  if (nrow(df2) > 0) plot_radar(df2, combo2_colors, "Long-read + Polish", sample_name)
  
  df3 <- data %>% filter(sample == sample_name, assembly %in% combo3_methods)
  if (nrow(df3) > 0) plot_radar(df3, combo3_colors, "Hybrid", sample_name)
}

dev.off()
