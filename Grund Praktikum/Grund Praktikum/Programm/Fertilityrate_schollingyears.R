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
  
  # 绘制散点图
  scatter_plot <- ggplot(filtered_data, aes(x = MeanYearsOfSchooling, y = FertilityRate, color = Region)) +
    geom_point(alpha = 0.7, size = 5) +
    labs(
      title = paste("Fertility Rate vs. Mean Years of Schooling (Year:", input_year, ")"),
      x = "Mean Years of Schooling",
      y = "Fertility Rate",
      color = "Region"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    annotate(
      "text", 
      x = max(filtered_data$MeanYearsOfSchooling, na.rm = TRUE) * 0.9, 
      y = max(filtered_data$FertilityRate, na.rm = TRUE), 
      label = correlation_text, 
      hjust = 0, vjust = 1,
      size = 3, color = "black"
    )
  
}

# 调用函数，绘制不同年份数据
p1 <- plot_fertility_vs_schooling(cleaned_data, 1960)
p2 <- plot_fertility_vs_schooling(cleaned_data, 2020)

final_plot <- (p1 | p2) + plot_layout(guides = "collect")
print(final_plot)


