library(ggplot2)
library(ggtree)
library(ggtreeExtra)
library(treeio)
library(dplyr)
library(ggnewscale)

# 1. 读取进化树和 metadata
# 您提供的实际文件路径如下，直接写绝对路径，避免相对路径造成误读

tree_file <- "../表格/Figure 8D/gtdbtk.bac120.user_msa.fasta.gz.treefile"
meta_file <- "../表格/Figure 8D/gtdbtk_checkm.csv"

tree <- read.tree(tree_file)
meta <- read.csv(meta_file, stringsAsFactors = FALSE)

# 清理 MAG 名称，确保和 tree$tip.label 一致
meta$MAG <- gsub("\\.fa$", "", meta$MAG)
meta$Is_Novel <- toupper(as.character(meta$Is_Novel))
meta$Polished <- toupper(as.character(meta$Polished))
meta$Contamination <- as.numeric(meta$Contamination)
meta$Contamination[is.na(meta$Contamination)] <- 0
meta$Contamination <- pmin(meta$Contamination, 10)

# 将 metadata 绑定到进化树
p <- ggtree(tree, layout="circular", size=0.4, branch.length="none") %<+% meta

# 2. 基础树
p1 <- p + 
  aes(color=Phylum) +
  geom_tippoint(aes(subset=(Is_Novel=="TRUE")), shape=8, color="#E74C3C", size=2.5)

# 3. Completeness 柱状图
p2 <- p1 + 
  new_scale_fill() +
  geom_fruit(
    geom=geom_col,
    mapping=aes(y=MAG, x=Completeness),
    orientation="y",
    fill="#2ECC71",
    pwidth=0.12,
    axis.params=list(
      axis="none",
      text.size=0
    ),
    grid.params=list(size=0.2, color="grey70", alpha=0.5)
  )

# 4. Contamination 柱状图
# 关键修正：ggtreeExtra 控制范围要用 axis.params 里的 limits，而不是单独写 xlim
# 这里显式锁死污染度范围为 0-10
p3 <- p2 + 
  geom_fruit(
    geom=geom_col,
    mapping=aes(y=MAG, x=Contamination),
    orientation="y",
    fill="#E74C3C",
    pwidth=0.12,
    offset=0.06,
    axis.params=list(
      axis="none",
      text.size=0,
      limits=c(0, 10)
    ),
    grid.params=list(
      size=0.2,
      color="grey70",
      alpha=0.5,
      vline=TRUE
    )
  )

# 5. Assembler 条带
p4 <- p3 + 
  new_scale_fill() +
  geom_fruit(
    geom=geom_tile,
    mapping=aes(y=MAG, fill=Assembler),
    width=1,
    pwidth=0.05,
    offset=0.1
  )

# 6. Polished 条带
p5 <- p4 + 
  new_scale_fill() +
  geom_fruit(
    geom=geom_tile,
    mapping=aes(y=MAG, fill=Polished),
    width=1,
    pwidth=0.05,
    offset=0.05
  ) +
  scale_fill_manual(values=c("YES"="#2E4053", "NO"="#E5E7E9"), na.translate=FALSE)

# 7. 图形样式
p_final <- p5 + 
  hexpand(0.3) +
  theme(
    legend.position = "right",
    legend.title = element_text(size=14, face="bold"),
    legend.text = element_text(size=12),
    legend.key.size = unit(0.7, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# 输出
ggsave("MAGs_Phylogenetic_Tree.pdf", p_final, width=14, height=14)
ggsave("MAGs_Phylogenetic_Tree.png", p_final, width=14, height=14, dpi=300)