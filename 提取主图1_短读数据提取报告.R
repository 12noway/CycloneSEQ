if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, grDevices)

# 1. 原始数据准备
raw_data_dna <- tibble(
  Sample = c('0919-1', '2121-1', '8915-1', '8366-1', '0927-1', '0942-1', '0980-1', '8373-1', '8343-1', '2725-1', '4141-1', '2835-1', '0996-1', '0934-1', '4143-1'),
  CP_Conc = c(52.2, 182, 384, 278, 97.2, 232, 155, 334, 134, 372, 114, 202, 310, 250, 318),
  CP_Yield = c(1.044, 3.64, 7.68, 5.56, 1.944, 4.64, 3.1, 6.68, 2.68, 7.44, 2.28, 4.04, 6.2, 5, 6.36),
  CP_A260_280 = c(2.15, 2.08, 2.01, 1.97, 2.1, 1.98, 2.09, 2.08, 2.08, 1.92, 2.08, 2.02, 2.07, 1.99, 1.74),
  CP_A260_230 = c(2.24, 1.95, 1.99, 1.71, 2.07, 1.9, 2.09, 1.94, 2.09, 1.63, 2.09, 1.96, 2.06, 1.74, 1.15),
  CP_Frag = c(NA, 20.161, 15.648, 22.458, 23.337, 20.831, 2.089, 19.277, 11.675, 14.283, 24.484, 11.656, 30.957, 15.884, 15.125),
  CZ_Conc = c(46.4, 134, 308, 183, 29.8, 104, 79, 147, 35.4, 216, 36.2, 109, 169, 117, 183),
  CZ_Yield = c(0.928, 2.68, 6.16, 3.66, 0.596, 2.08, 1.58, 2.94, 0.708, 4.32, 0.724, 2.18, 3.38, 2.34, 3.66),
  CZ_A260_280 = c(2.1, 2.07, 2.03, 2.06, 2.15, 2.07, 2.13, 2.01, 2.12, 2.06, 2.13, 2.06, 2.08, 2.09, 2.06),
  CZ_A260_230 = c(2.26, 2.28, 2.12, 2.22, 2.18, 2.13, 2.29, 1.68, 2.25, 2.1, 2.18, 2.1, 2.19, 2.25, 2.22),
  CZ_Frag = c(12.374, 9.423, 9.178, 7.033, 9.31, 12.976, 2.48, 8.466, 8.577, 7.495, 13.173, 9.73, 2.763, 7.755, 9.066)
) %>%
  pivot_longer(cols = -Sample, names_to = "Key", values_to = "Value") %>%
  separate(Key, into = c("Method", "Param"), sep = "_", extra = "merge")

# 2. 绘图执行
modern_paired_plot <- function(p_name, y_title, unit = NULL, lines = NULL) {
  df <- raw_data_dna %>% filter(Param == p_name) %>% drop_na()
  ggplot(df, aes(x = Method, y = Value)) +
    geom_line(aes(group = Sample), color = "grey90", linewidth = 0.5) + # 样本连线
    geom_boxplot(aes(fill = Method), width = 0.4, color = "black", alpha = 0.7, outlier.shape = NA, linewidth = 0.8) +
    geom_point(aes(color = Method), size = 2) +
    stat_compare_means(method = "wilcox.test", paired = TRUE, label = "p.signif", label.x = 1.5, size = 5) +
    {if(!is.null(lines)) geom_hline(yintercept = lines, linetype = "dotted", color = "grey40")} +
    scale_fill_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    scale_color_manual(values = c("CP" = "#08519C", "CZ" = "#EF6548")) +
    labs(y = if(!is.null(unit)) paste0(y_title, " (", unit, ")") else y_title, x = NULL) +
    theme_pubr() + theme(legend.position = "none", axis.title = element_text(face = "bold"))
}

p_list <- list(
  modern_paired_plot("Conc", "Concentration", "ng/µL"),
  modern_paired_plot("Yield", "Yield", "µg"),
  modern_paired_plot("A260_280", "A260/A280", lines = c(1.8, 2.0)),
  modern_paired_plot("A260_230", "A260/A230", lines = 2.0),
  modern_paired_plot("Frag", "Fragment Length", "kb")
)

final_p1 <- ggarrange(plotlist = p_list, ncol = 3, nrow = 2, labels = "AUTO")
ggsave("Panel_1_DNA_Quality.pdf", final_p1, width = 11, height = 7.5, device = cairo_pdf)