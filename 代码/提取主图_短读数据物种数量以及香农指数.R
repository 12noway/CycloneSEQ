pacman::p_load(tidyverse, ggpubr, grDevices)

# ==========================================
# 1. 数据准备
# ==========================================
df_raw_div <- read_csv("../表格/提取主图_短读数据物种数量以及香农指数.csv")

df_div_long <- df_raw_div %>%
  mutate(
    CP_Species = common_species_number + cp_unique_number,
    CZ_Species = common_species_number + cz_unique_number
  ) %>%
  select(sample_name,
         CP_Species, CZ_Species,
         CP_Shannon = cp_shannon,
         CZ_Shannon = cz_shannon) %>%
  pivot_longer(-sample_name, names_to = "Key", values_to = "Value") %>%
  separate(Key, into = c("Method", "Metric")) %>%
  mutate(Method = factor(Method, levels = c("CP", "CZ")))

# ==========================================
# 2. 绘图函数（统一风格）
# ==========================================
create_div_plot <- function(met, y_lab, tag_lab) {
  
  df <- df_div_long %>% filter(Metric == met)
  
  ggplot(df, aes(x = Method, y = Value, fill = Method)) +
    
    # ✔ 配对连线（保留你的核心优势）
    geom_line(
      aes(group = sample_name),
      color = "grey85",
      linewidth = 0.5
    ) +
    
    # ✔ 箱线图（统一风格）
    geom_boxplot(
      width = 0.4,
      color = "black",
      alpha = 0.7,
      outlier.shape = NA,
      linewidth = 0.8
    ) +
    
    # ✔ 散点（统一为 Nature 风格）
    geom_point(
      shape = 21,
      size = 2.5,
      color = "black",
      stroke = 0.5,
      alpha = 0.7
    ) +
    
    # ✔ 显著性（统一参数）
    stat_compare_means(
      method = "wilcox.test",
      paired = TRUE,
      label = "p.signif"
    ) +
    
    # ✔ 颜色统一
    scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    
    labs(
      y = y_lab,
      x = NULL,
      tag = tag_lab
    ) +
    
    # ✔ 主题统一（和 Bray 一致）
    theme_pubr() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      plot.tag = element_text(size = 14, face = "bold")
    )
}

# ==========================================
# 3. 生成图
# ==========================================
p_g <- create_div_plot("Species", "Observed Species", "G")
p_h <- create_div_plot("Shannon", "Shannon Index", "H")

final_p3 <- ggarrange(p_g, p_h, ncol = 2)

# ==========================================
# 4. 导出
# ==========================================
ggsave("Panel_3_Diversity_Modern.pdf",
       final_p3,
       width = 9,
       height = 5,
       device = cairo_pdf)

print(final_p3)