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

# 设定颜色（使用 NEJM 主题）
typeColor <- structure(
  c(pal_nejm()(2), "gray80"),  # NEJM 颜色：上调（红色）、下调（蓝色），非显著（灰色）
  names = c("up", "down", "ns")
)

# 绘制 MA 图（确保显著点在最上层）
ggplot() +
  # 先绘制非显著基因（灰色）
  geom_point(data = degdata[type == "ns"], aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type), alpha = 1) +
  # 再绘制显著上调基因（红色）
  geom_point(data = degdata[type == "up"], aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type)) +
  # 最后绘制显著下调基因（蓝色）
  geom_point(data = degdata[type == "down"], aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type)) +
  scale_radius(range = c(.1, 2), name = "pvalue") +  # 设定点大小，并显示 pvalue 图例
  scale_color_manual(values = typeColor, name = "type", limits = c("up", "ns", "down")) +  # 调整 type 图例顺序
  labs(
    x = TeX("$log_{2}(base\\,Mean)$"),
    y = TeX("$log_{2}(Fold\\,Change)$")) +
  theme(
    aspect.ratio = 1,
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = "right")  # 将图例放置在右侧
