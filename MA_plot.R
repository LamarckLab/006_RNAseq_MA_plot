##### 加载所需包 #####
library(tidyverse)
library(magrittr)
library(glue)
library(data.table)
library(ggplot2)
library(ggsci)
library(scales)
library(latex2exp)

##### 读入数据 #####
degdata <- fread("C:/Users/Lamarck/Desktop/MA_plot_data.csv")

# 对 baseMean 列取对数变换（以 2 为底）
degdata[, baseMean := log2(baseMean)][]

##### 分组标注（显著性） #####
degdata[, type := "Not Significant"][]  # 初始化所有基因类型为 "Not Significant"
degdata[log2FoldChange > 1 & pvalue < 0.05, type := "Up Regulated"]
degdata[log2FoldChange < -1 & pvalue < 0.05, type := "Down Regulated"][]

##### 定义颜色 #####
typeColor <- structure(
  c(pal_nejm()(2), "gray80"),  # NEJM 主题颜色：Up（红）、Down（蓝）、非显著（灰）
  names = c("Up Regulated", "Down Regulated", "Not Significant")
)

##### 将图保存为 PDF #####
pdf(
  file = "C:/Users/Lamarck/Desktop/MA_plot.pdf",  # 保存路径及文件名
  width = 8,  # 图的宽度，可自行调整
  height = 8  # 图的高度，可自行调整
)

# 绘图
ggplot() +
  # 先绘制非显著基因（灰色）
  geom_point(
    data = degdata[type == "Not Significant"],
    aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type),
    alpha = 1
  ) +
  # 再绘制显著上调基因（红色）
  geom_point(
    data = degdata[type == "Up Regulated"],
    aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type)
  ) +
  # 最后绘制显著下调基因（蓝色）
  geom_point(
    data = degdata[type == "Down Regulated"],
    aes(x = baseMean, y = log2FoldChange, size = pvalue, color = type)
  ) +
  # 设置点大小与颜色
  scale_radius(range = c(.1, 2), name = "P value") +
  scale_color_manual(
    values = typeColor,
    name = "Type",  
    limits = c("Up Regulated", "Not Significant", "Down Regulated")
  ) +
  # 轴标签
  labs(
    x = TeX("$log_{2}(base\\,Mean)$"),
    y = TeX("$log_{2}(Fold\\,Change)$")
  ) +
  # 一些主题设置
  theme(
    aspect.ratio = 1,
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = c(0.8, 0.8),           # 可修改为 "right", "top", "bottom" 或一个 (x, y) 的坐标
    legend.background = element_rect(fill = NA),
    text = element_text(size = 14),          # 控制整体文字大小
    axis.text = element_text(size = 12),     # 坐标刻度文字大小
    axis.title = element_text(size = 14, face = "bold"),  # 坐标标题文字大小和加粗
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

dev.off()
