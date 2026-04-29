rm(list = ls())

library(tidyr)
library(ggtree)
library(ggplot2)
library(dplyr)
library(ggtreeExtra)
library(ggnewscale)
library(readr)
library(stringr)
library(RColorBrewer)
library(scales)
library(ape)
library(cowplot)

# ================= 读数据 =================
tree <- read.tree("../表格/组装主图_三种组装方案进化树/Phylogenetic_tree.unrooted.tree")
info <- read_tsv("../表格/组装主图_三种组装方案进化树/gtdbtk.bac120.summary.tsv")

plot_data <- info %>%
  mutate(
    label = user_genome,
    Family = str_extract(classification, "f__[^;]+"),
    Family = gsub("f__", "", Family)
  ) %>%
  select(label, Family) %>%
  mutate(
    Assembler = case_when(
      str_detect(label, "flye_nextpolish") ~ "flye_nextpolish",
      str_detect(label, "metamdbg_nextpolish") ~ "metamdbg_nextpolish",
      str_detect(label, "myloasm_nextpolish") ~ "myloasm_nextpolish",
      str_detect(label, "hybridspades") ~ "hybridspades",
      str_detect(label, "opera_ms") ~ "opera_ms",
      str_detect(label, "flye") ~ "flye",
      str_detect(label, "metamdbg") ~ "metamdbg",
      str_detect(label, "myloasm") ~ "myloasm",
      TRUE ~ NA_character_
    ),
    DataType = case_when(
      Assembler %in% c("flye","metamdbg","myloasm") ~ "Long-read",
      Assembler %in% c("flye_nextpolish","metamdbg_nextpolish","myloasm_nextpolish",
                       "hybridspades","opera_ms") ~ "Hybrid"
    ),
    ToolGroup = case_when(
      Assembler %in% c("flye","flye_nextpolish") ~ "flye",
      Assembler %in% c("metamdbg","metamdbg_nextpolish") ~ "metamdbg",
      Assembler %in% c("myloasm","myloasm_nextpolish") ~ "myloasm",
      Assembler == "hybridspades" ~ "hybridspades",
      Assembler == "opera_ms" ~ "opera_ms"
    )
  ) %>% drop_na()

# ================= Family 调色板 =================
fam_levels <- sort(unique(plot_data$Family))
n_fam <- length(fam_levels)

pal <- c(brewer.pal(12,"Set3"),
         brewer.pal(8,"Set2"),
         brewer.pal(8,"Dark2"),
         brewer.pal(12,"Paired"))

if(n_fam > length(pal)){
  pal <- colorRampPalette(pal)(n_fam)
} else {
  pal <- pal[1:n_fam]
}

names(pal) <- fam_levels

# ================= 基础树 =================
p_base <- ggtree(tree, layout="fan", open.angle=0, size=0.25) %<+% plot_data
d <- p_base$data
w <- 0.06
off <- 0.02

# ================= Family 背景分区带 =================
bg_df <- d %>%
  filter(isTip) %>%
  group_by(Family) %>%
  summarise(ymin=min(y)-0.5, ymax=max(y)+0.5)

# ================= ① 主图（三层环 + Family背景） =================
main_plot <- p_base +
  
  geom_rect(data=bg_df,
            aes(xmin=0, xmax=max(d$x)*1.03,
                ymin=ymin, ymax=ymax, fill=Family),
            inherit.aes=FALSE, alpha=0.08) +
  scale_fill_manual(values=pal, guide="none") +
  
  new_scale_fill() +
  geom_fruit(geom=geom_tile, mapping=aes(fill=DataType),
             width=w, offset=off,
             grid.params=list(color="white", size=0.1)) +
  scale_fill_manual(values=c("Long-read"="#1b9e77","Hybrid"="#d95f02")) +
  
  new_scale_fill() +
  geom_fruit(geom=geom_tile, mapping=aes(fill=ToolGroup),
             width=w, offset=off+0.015,
             grid.params=list(color="white", size=0.1)) +
  scale_fill_brewer(palette="Set2") +
  
  new_scale_fill() +
  geom_fruit(geom=geom_tile, mapping=aes(fill=Family),
             width=w, offset=off+0.015,
             grid.params=list(color="white", size=0.1)) +
  scale_fill_manual(values=pal) +
  
  theme(legend.position="none") +
  xlim(0, max(d$x)*1.4)

ggsave("01_phylogenomic_tree_full.pdf", main_plot, width=14, height=14)

# ================= 图例函数 =================
legend_only <- function(p){
  cowplot::get_legend(p + theme(legend.position="right")) %>%
    cowplot::ggdraw()
}

# ================= ② Assembler 图例 =================
p_tool <- ggplot(plot_data, aes(x=ToolGroup, fill=ToolGroup)) +
  geom_bar() +
  scale_fill_brewer(palette="Set2", name="Assembler") +
  theme_void()

ggsave("02_Assembler_legend.pdf",
       legend_only(p_tool),
       width=3, height=2.5)

# ================= ③ Data Type 图例 =================
p_type <- ggplot(plot_data, aes(x=DataType, fill=DataType)) +
  geom_bar() +
  scale_fill_manual(values=c("Long-read"="#1b9e77","Hybrid"="#d95f02"),
                    name="Data Type") +
  theme_void()

ggsave("03_DataType_legend.pdf",
       legend_only(p_type),
       width=3, height=2)

# ================= ④ Family 图例 =================
p_family <- ggplot(plot_data, aes(x=Family, fill=Family)) +
  geom_bar() +
  scale_fill_manual(values=pal, name="Family") +
  theme_void() +
  theme(legend.text=element_text(size=6))

ggsave("04_Family_legend.pdf",
       legend_only(p_family),
       width=4, height=6)
