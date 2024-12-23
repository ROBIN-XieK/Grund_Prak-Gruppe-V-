# 加载必要的包
library(ggplot2)
library(dplyr)
library(patchwork)

#图3
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/urbanization-vs-gdp.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
data <- read.csv(url)
# 数据清洗：筛选有效数据，去除缺失值
data_clean <- data %>%
  filter(!is.na(gdp_per_capita), 
         !is.na(urbc_c_share),
         gdp_per_capita > 0)
# 绘制散点图
p1 <- ggplot(data_clean, aes(x = gdp_per_capita, y = urbc_c_share , color = owid_region)) +
  geom_point(alpha = 1 , size = 3) +  # 散点图
  scale_x_log10(  # X轴使用对数刻度
    limits = c(1000, 100000),  # 缩小X轴的范围
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),  # 设置刻度
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")  # 设置标签
  ) +
  labs(title = "GDP vs Urban Population Share",
       x = "GDP per Capita (Log Scale)",
       y = "Population Share in Urban Areas (%)" ,
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "right")


#图4
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/employment-in-agri-vs-urban-pop.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
data <- read.csv(url)
# 数据清洗：去除缺失值，并确保数据有效
data_clean <- data %>%
  filter(!is.na(sp_urb_totl_in_zs), 
         !is.na(sl_agr_empl_zs),
         sp_urb_totl_in_zs > 0,
         sl_agr_empl_zs > 0)
# 绘制散点图
p2 <- ggplot(data_clean, aes(x = sp_urb_totl_in_zs, 
                       y = sl_agr_empl_zs, color = owid_region)) +
  geom_point(alpha = 1 , size = 3) +  # 散点图
  labs(title = "Urban Population Share vs Agricultural Employment Share",
       x = "Share of Population Living in Urban Areas (%)",
       y = "Share of Employment in Agricultural Sector (%)",
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")

final_plot <- p1 + p2 + plot_layout(guides = "collect")

print(final_plot)

