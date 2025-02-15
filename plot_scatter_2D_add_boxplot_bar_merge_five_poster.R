library(ggplot2)
library(patchwork)
library(dplyr)
library(cowplot) 



# ----------------------------
# 1. 定义通用绘图函数
# ----------------------------
generate_combined_plot <- function(data, x_label) {
  # 生成单个组合图（无图例）
  colors <- c("#999999", "#E69F00", "#56B4E9","#009E73","#0072B2","#9467BD","#D55E00","#CC79A7")
tool_colors <- c(
    "BactSNP" = "#999999",
     "GATK" = "#E69F00",
     "Samtools" = "#56B4E9",
     "VarScan" = "#009E73",
     "Snippy" = "#0072B2",
     "AccuSNV" = "#9467BD",
     "Breseq" = "#D55E00",
     "freeBayes" = "#CC79A7"
 )

colors_array <- tool_colors[data$Tools]
data$Tools <- factor(data$Tools,levels=c("AccuSNV","GATK","freeBayes","Samtools","BactSNP"
,"VarScan","Breseq","Snippy"))
data$Species <- as.factor(data$Species)

lab_title <- gsub("x$", "X", x_label)
# option 2 - deepseek for nature journal
# 主散点图代码（已根据你的代码调整）
plot_main <- ggplot(data, aes(x = Precision, y = Recall, shape = Species, color = Tools)) +
  geom_point(size = 2.5, alpha = 0.8, stroke = 0.5) +
  geom_vline(xintercept = 0.99, color = "red", linetype = "dotdash", size = 0.5) +
  annotate("text", x = 0.99, y = Inf, label = "0.99", color = "red", 
           vjust = -0.5, hjust = -0.1, size = 3, fontface = "bold") +
  scale_color_manual(values = tool_colors, name = "Tool") +
  scale_shape_discrete(name = "Species", solid = FALSE) +
  #scale_x_continuous( limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) + 
  #scale_y_continuous( limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) + 
  # 修改为对数坐标
     # 坐标轴设置
  scale_x_continuous(
    trans = scales::trans_new(
      name = "near_one_log",
      transform = function(x) -log10(1.0001 - x),  # 加0.0001避免出现log(0)
      inverse = function(x) 1 - 10^(-x)
    ),
    limits = c(0, 1),
    breaks = c(0, 0.9, 0.99,0.999, 1),
    #expand = c(0.02, 0.02),
 labels = scales::label_number(accuracy = 0.001)
  ) +
  scale_y_continuous(
    trans = scales::trans_new(
      name = "near_one_log",
      transform = function(x) -log10(1.0001 - x),
      inverse = function(x) 1 - 10^(-x)
    ),
    limits = c(0, 1),
    breaks = c(0, 0.8,  0.9, 0.95, 0.98,0.99,0.995,0.999, 1),
    #expand = c(0.02, 0.02),
    labels = scales::label_number(accuracy = 0.001)
  ) +
  labs(x = "Precision (log scale)", y = "Recall (log scale)", title = lab_title) +
  theme_classic(base_size = 11) +
  coord_cartesian(clip = "off")

if (x_label=="50x"){
 plot_main <- plot_main + theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "right",
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = margin(t = 30, unit = "pt"), 
    legend.box.margin = margin(t = 95, unit = "pt"),
    legend.background = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10))
  ) 
} else {
	plot_main<- plot_main+theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10))
  )
}

# 箱线图代码（工具顺序与主图图例一致）
plot_boxplot <- ggplot(data, aes(x = Precision, y = Tools, fill = Tools)) +
  geom_boxplot(width = 0.3, alpha = 0.5, outlier.shape = NA) +
  geom_point(
    aes(color = Tools, shape = Species),
    position = position_jitter(height = 0.1),
    size = 1.5,
    alpha = 0.8, stroke = 0.5
  ) + geom_vline(xintercept = 0.99, color = "red", linetype = "dotdash", size = 0.5) +
  scale_fill_manual(values = tool_colors) +
  scale_color_manual(values = tool_colors) +
  scale_y_discrete(limits = rev(levels(data$Tools))) +
  scale_x_continuous(
    limits = c(0.95, 1.001),
    breaks = seq(0.95, 1.0, 0.01),
    labels = c("0.95", "0.96", "0.97", "0.98", "0.99", "1.0")
  ) +
  labs(x = "Precision", y = "Tools") +
  theme_classic(base_size = 11) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # 倾斜刻度标签
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 1, 0.5, 1), "cm")
  )

data_summary <- data %>%
  group_by(Tools) %>%
  summarise(mean_F1 = mean(F1.score, na.rm = TRUE))

# ------ 新增F1-score柱状图 ------
plot_bar <- ggplot() +
  # 绘制平均F1柱状图
  geom_col(
    data = data_summary,
    aes(x = Tools, y = mean_F1, fill = Tools),
    width = 0.6,
    alpha = 0.8
  ) +
  # 叠加物种F1点
  geom_point(
    data = data,
    aes(x = Tools, y = F1.score, color = Tools, group = Tools),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.7
    ),
    size = 1.5,
    alpha = 0.6
  ) +
  # 添加平均值文本
  geom_text(
    data = data_summary,
    aes(x = Tools, y = mean_F1, label = sprintf("%.3f", mean_F1)),
    vjust = 0.63,  # 文本位置调整 # raw - vjust=-0.9
    hjust= -0.1,
    size = 4,
    fontface = "bold",
    color = "black",
    angle=90,
    
  ) +
  scale_fill_manual(values = tool_colors) +
  scale_color_manual(values = tool_colors) +
  scale_x_discrete(limits = levels(data$Tools)) +  # 确保顺序一致
  scale_y_continuous(
    limits = c(0, 1.2),  # 扩展y轴容纳文本
    breaks = seq(0, 1, 0.2)
  ) +
  labs(x = "", y = "F1 Score") +
  theme_classic(base_size = 11) +
  theme(
    text = element_text(family = "Arial"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1,
      vjust = 1.1,
      margin = margin(t = 8) 
    ),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.5, 1, 1, 1), "cm")
  ) +
  coord_cartesian(clip = "off") 

# ------ 组合三图 ------
combined_plot <- plot_main / plot_boxplot / plot_bar + 
  plot_layout(heights = c(3.5, 1, 2)) 
}

# ----------------------------
# 2. 批量生成5个组合图（10X-50X）
# ----------------------------
plot_list <- lapply(c("10x", "20x", "30x", "40x", "50x"), function(x) {
  data <- read.csv(paste0("t1_10strain_bench_mix_for_R_new_fix/res_", x, ".csv")) # 修改为实际路径
  #data$Tools <- factor(data$Tools, levels = names(tool_colors))
  generate_combined_plot(data,x_label=x)
})


# ----------------------------
# 4. 整合所有子图
# ----------------------------
final_plot <- wrap_plots(plot_list, nrow = 1) + # 横向排列
  plot_layout(guides = "collect") &
  theme(
    plot.tag = element_text(size = 12, face = "bold", family = "Arial"), # 子图标签样式
    plot.margin = unit(c(2, 2, 2, 2), "mm")
  )

# 如果想用plots里的export保存，到这里就可以了

# ----------------------------
# 5. 添加最终导出代码（续接原有代码）
# ----------------------------

# 定义科学期刊标准保存参数
save_plot <- function(plot, filename, width_cm, height_cm) {
  ggsave(
    filename = filename,
    plot = plot,
    device = "png",
    dpi = 400,  # 设置DPI为400
    units = "cm",
    width = width_cm,    # 根据5个组合图横向排列计算总宽度
    height = height_cm,  # 保持与单图比例一致
    
    bg = "white"
  )
}
# compression = "lzw",
# 计算最佳尺寸（基于单图比例）
single_plot_width <- 10   # 单个组合图的基准宽度（单位：cm）
single_plot_height <- 14  # 单个组合图的基准高度（单位：cm）

# 横向排列5个子图的推荐尺寸
final_width <- single_plot_width * 5 * 0.8  # 5个图宽度 + 紧凑排列系数
final_height <- single_plot_height * 1.2    # 保持高度余量

# 执行保存
save_plot(
  final_plot,
  filename = "combined_results_400dpi_poster.png",
  width_cm = final_width,
  height_cm = final_height
)