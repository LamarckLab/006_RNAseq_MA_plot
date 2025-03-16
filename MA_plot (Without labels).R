##### Load packages #####
# install.packages("tidyverse")
# install.packages("latex2exp")

library(tidyverse)
library(magrittr)
library(glue)
library(data.table)
library(ggplot2)
library(ggsci)
library(scales)
library(latex2exp)

# 读入数据
degdata <- fread("C:/Users/Lamarck/Desktop/MA_plot_data.csv")

# 将 padj 列中 NA 值替换为 1
degdata[is.na(padj), padj := 1][]

# 对 baseMean 列取对数变换（以 2 为底）
degdata[, baseMean := log2(baseMean)][]

# MA 图的绘制
degdata[, type := "ns"][]  # 初始化所有基因类型为 "ns"
degdata[log2FoldChange > 1 & pvalue < 0.05, type := "up"]
degdata[log2FoldChange < -1 & pvalue < 0.05, type := "down"][]

# 设定颜色
typeColor <- structure(
  c(pal_nejm()(2), "gray80"),
  names = c("up", "down", "ns")
)

# 绘制 MA 图（不额外标记基因）
ggplot(degdata, aes(x = baseMean, y = log2FoldChange)) +
  geom_point(aes(color = type, size = pvalue), show.legend = T) +  # 按照 type 颜色区分
  scale_radius(range = c(.1, 2)) +  # 控制点的大小范围
  scale_color_manual(values = typeColor) +  # 设定颜色
  labs(
    x = TeX("$log_{2}(base\\,Mean)$"),
    y = TeX("$log_{2}(Fold\\,Change)$")) +
  theme(
    aspect.ratio = 1,
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line())
