library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)

data <- read.csv("https://ourworldindata.org/grapher/fertility-rate-vs-mean-years-of-schooling.csv?v=1&csvType=full&useColumnShortNames=true")

colnames(data) <- c("Entity", "Code", "Year", "FertilityRate", "MeanYearsOfSchooling", 
                    "Population", "Region")
# 定义填充函数
fill_regions_overwrite_all <- function(data) {
  # 提取2015年具有非空地区值的国家和地区
  region_2023 <- data %>%
    filter(Year == 2023 & !is.na(Region)) %>%
    select(Entity, Region) %>%
    distinct()
  
  # 用2015年的地区值覆盖所有年份的数据
  updated_data <- data %>%
    left_join(region_2023, by = "Entity", suffix = c("", "_2023")) %>%
    mutate(Region = Region_2023) %>%
    select(-Region_2023)  # 删除临时列
  
  return(updated_data)
}

# 应用函数到您的数据集
cleaned_data <- fill_regions_overwrite_all(data)

# 定义函数：绘制指定年份的图表
plot_fertility_vs_schooling <- function(data, input_year) {
  # 清理 Region 中的空格并将其视为 NA
  data <- data %>%
    mutate(Region = ifelse(trimws(Region) == "", NA, Region))
  
  # 筛选出指定年份的数据，且所有必要列均非空
  filtered_data <- data %>%
    filter(Year == input_year, 
           !is.na(FertilityRate), 
           !is.na(MeanYearsOfSchooling), 
           !is.na(Region))  # 确保地区不为空
  
  # 计算各地区的相关系数
  correlation_data <- filtered_data %>%
    group_by(Region) %>%
    summarize(correlation = cor(FertilityRate, MeanYearsOfSchooling, use = "complete.obs")) %>%
    arrange(desc(correlation))  # 按相关系数排序
  
  # 生成相关系数文本
  correlation_text <- correlation_data %>%
    mutate(label = paste0(Region, ": r = ", round(correlation, 2))) %>%
    pull(label) %>%
    paste(collapse = "\n")
 
  x_limits <- c(0, 15)  
  y_limits <- c(0, 8)   
  # 绘制散点图
  scatter_plot <- ggplot(filtered_data, aes(x = MeanYearsOfSchooling, y = FertilityRate, color = Region)) +
    geom_point(alpha = 0.7, size = 5) +
    scale_x_continuous(
      breaks = seq(0, 15, 2.5),
      limits = c(0, 15)
    ) +
    scale_y_continuous(
      breaks = seq(0, 8, 2),
      limits = c(0, 8)
    )+
    labs(
      title = paste("Fruchtbarkeitsrate vs Bildungsjahre(",input_year,")"),
      x = "Durchschnittliche Bildungsjahre von Frauen",
      y = "Fruchtbarkeitsrate",
      color = "Region"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
      legend.position = "right",
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24)
    ) +
    annotate(
      "text", 
      x = x_limits[2] * 0.68, 
      y = y_limits[2], 
      label = correlation_text, 
      hjust = 0, vjust = 1,
      size = 8, color = "black"
    )
  
}

# 调用函数，绘制不同年份数据
p1 <- plot_fertility_vs_schooling(cleaned_data, 1960)
p2 <- plot_fertility_vs_schooling(cleaned_data, 2020)

final_plot <- (p1 | p2) + plot_layout(guides = "collect")
print(final_plot)

ggsave("Fertilityrate_schollingyears.png", path = "Results/", width = 25, height = 14,
       device='png', dpi=300, bg = "white")