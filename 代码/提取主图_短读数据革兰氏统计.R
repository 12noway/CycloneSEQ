# ==========================================
# 1. 环境准备
# ==========================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, grDevices)

# ==========================================
# 2. 数据准备
# ==========================================
doc_data <- c(
  60, 52, 76, 48, 29, 38, 40, 35, 62, 42, 74, 39,
  77, 59, 94, 53, 75, 72, 97, 64, 60, 61, 65, 47,
  80, 44, 88, 45, 43, 26, 59, 25, 60, 45, 69, 35,
  89, 61, 94, 52, 45, 44, 61, 35, 67, 48, 76, 42,
  52, 26, 67, 26, 68, 32, 78, 32, 67, 44, 79, 41
)

df <- as_tibble(matrix(doc_data, ncol = 4, byrow = TRUE,
                       dimnames = list(NULL, c("cp_pos", "cp_neg", "cz_pos", "cz_neg"))))

df_long <- df %>%
  mutate(Sample = factor(1:n())) %>%
  pivot_longer(-Sample, names_to = "Group", values_to = "Value") %>%
  separate(Group, into = c("Method", "Stain"), sep = "_") %>%
  mutate(
    Method = toupper(Method),
    Stain = recode(Stain,
                   pos = "Gram-positive",
                   neg = "Gram-negative"),
    Stain = factor(Stain, levels = c("Gram-positive", "Gram-negative")),
    X = interaction(Stain, Method, sep = "_"),
    X = factor(X, levels = c(
      "Gram-positive_CP", "Gram-positive_CZ",
      "Gram-negative_CP", "Gram-negative_CZ"
    ))
  )

# ==========================================
# 3. 统计检验
# ==========================================
gp_p <- wilcox.test(df$cp_pos, df$cz_pos, paired = TRUE)$p.value
gn_p <- wilcox.test(df$cp_neg, df$cz_neg, paired = TRUE)$p.value

get_sig <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "ns")))
}

gp_label <- get_sig(gp_p)
gn_label <- get_sig(gn_p)

# ==========================================
# 4. 显著性线数据（关键！）
# ==========================================
y_max <- max(df_long$Value)

sig_df <- tibble(
  x_start = c(1, 3),
  x_end   = c(2, 4),
  y       = c(y_max + 5, y_max + 5),
  label   = c(gp_label, gn_label)
)

# ==========================================
# 5. 作图
# ==========================================
p <- ggplot(df_long, aes(x = X, y = Value)) +
  
  # ✔ 样本配对线
  geom_line(
    aes(group = interaction(Sample, Stain)),
    color = "grey90",
    linewidth = 0.5
  ) +
  
  # ✔ 箱线图
  geom_boxplot(
    aes(fill = Method),
    width = 0.5,
    color = "black",
    alpha = 0.7,
    outlier.shape = NA
  ) +
  
  # ✔ 散点
  geom_point(
    aes(color = Method),
    size = 2
  ) +
  
  # ✔ 显著性横线（核心）
  geom_segment(
    data = sig_df,
    aes(x = x_start, xend = x_end, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  
  # ✔ 显著性短竖线（两端）
  geom_segment(
    data = sig_df,
    aes(x = x_start, xend = x_start, y = y, yend = y - 2),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  geom_segment(
    data = sig_df,
    aes(x = x_end, xend = x_end, y = y, yend = y - 2),
    inherit.aes = FALSE,
    linewidth = 0.8
  ) +
  
  # ✔ 星号
  geom_text(
    data = sig_df,
    aes(x = (x_start + x_end)/2, y = y + 2, label = label),
    inherit.aes = FALSE,
    size = 5
  ) +
  
  # ✔ x轴标签
  scale_x_discrete(labels = c("CP", "CZ", "CP", "CZ")) +
  
  # ✔ 颜色
  scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
  
  # ✔ 分割线
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey50") +
  
  # ✔ Gram标签
  annotate("text", x = 1.5, y = y_max + 12, label = "Gram-positive",
           size = 5, fontface = "bold") +
  annotate("text", x = 3.5, y = y_max + 12, label = "Gram-negative",
           size = 5, fontface = "bold") +
  
  labs(
    x = NULL,
    y = "Number of Species",
    tag = "F"
  ) +
  
  theme_pubr() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.tag = element_text(size = 14, face = "bold")
  ) +
  
  ylim(0, y_max + 15)

# ==========================================
# 6. 输出
# ==========================================
print(p)

ggsave("Panel_2_Gram_Final.pdf",
       p,
       width = 7,
       height = 5,
       device = cairo_pdf)