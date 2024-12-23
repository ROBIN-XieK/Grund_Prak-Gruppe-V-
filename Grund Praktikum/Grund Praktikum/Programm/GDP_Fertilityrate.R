library(ggplot2)
library(dplyr)

#图1
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/children-per-woman-fertility-rate-vs-level-of-prosperity.csv?v=1&csvType=filtered&useColumnShortNames=true"
data <- read.csv(url)
# 绘制散点图
print(ggplot(data, aes(x = rgdpo_pc, 
                 y = fertility_rate__sex_all__age_all__variant_estimates , color = owid_region)) +
  geom_point(alpha = 1 , size = 3) +  # 散点图
  scale_x_log10(
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),  # 设置刻度点
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")  # 设置刻度标签
  ) +
  labs(title = "GDP per Capita (Log Scale) vs Births per Woman",
       x = "GDP per Capita (Log Scale)",
       y = "Births per Woman",
       color = "Region") +
  theme_minimal() +
  theme(legend.position = "right"))
