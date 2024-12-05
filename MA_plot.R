##### Load packages #####
install.packages("tidyverse")
install.packages("latex2exp")

library(tidyverse)
library(magrittr)
library(glue)
library(data.table)
library(ggplot2)
library(ggrepel)
library(ggsci)
library(scales)
library(latex2exp)

# 读入数据
degdata <- fread("C:/Users/Lamarck/Desktop/MA_plot_data.csv")

# 将padj列中NA值替换为1
degdata[is.na(padj), padj := 1][]

# 对baseMean列取对数变换（以2为底）  存储到同名列中
degdata[, baseMean := log2(baseMean)][]

# MA图的绘制
degdata[, type := "ns"][]  # 初始化所有基因类型为"ns"
degdata[log2FoldChange > 1 & padj < 0.1, type := "up"][log2FoldChange < -1 & padj < 0.1, type := "down"][]  #  标记上调基因和下调基因

# 筛选 logFC 最高的 5 个上调基因
upGenes <- degdata[type == "up"][order(log2FoldChange, decreasing = TRUE)][1:5]

# 筛选 logFC 最低的 5 个下调基因
downGenes <- degdata[type == "down"][order(log2FoldChange, decreasing = FALSE)][1:5]

# 合并上下调基因
labelGene <- rbind(upGenes, downGenes)



pal_nejm()(8) %>% show_col()
typeColor <- structure(
  c(pal_nejm()(2), "gray80"),
  names = c("up", "down", "ns")
)

ggplot(degdata, aes(x = baseMean, y = log2FoldChange)) +
  geom_point(aes(color = type, size = pvalue), show.legend = T) +
  geom_text_repel(
    data = labelGene, aes(label = gene),
    size = 3, fontface = 3,
    nudge_x = .5, nudge_y = .5) +
  scale_radius(range = c(.1, 2)) +
  scale_color_manual(values = typeColor) +
  labs(
    x = TeX("$log_{2}(base\\,Mean)$"),
    y = TeX("$log_{2}(Fold\\,Change)$")) +
  theme(
    aspect.ratio = 1,
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line())

