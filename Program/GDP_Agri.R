library(ggplot2)
library(dplyr)

#图5
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/gdp-vs-agriculture-employment.csv?v=1&csvType=filtered&useColumnShortNames=true"
data <- read.csv(url)
# 数据清洗：去除缺失值，并确保GDP和农业就业率数据有效
data_clean <- data %>%
  filter(!is.na(gdp_per_capita), 
         !is.na(share_employed_agri),
         gdp_per_capita > 0, 
         share_employed_agri > 0)

correlation <- cor(data_clean$gdp_per_capita, 
                   data_clean$share_employed_agri, 
                   use = "complete.obs")
# 绘制散点图
print(ggplot(data_clean, aes(x = gdp_per_capita, y = share_employed_agri , color = owid_region)) +
  geom_point(alpha = 1 , size = 2) +  # 绘制散点图
  scale_x_log10(  # 设置X轴为对数刻度
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")
  ) +
  labs(title = "BIP pro Kopf vs Beschäftigte in der Landwirtschaft (2019)",
       x = "BIP pro Kopf (Log Skala)",
       y = "Anteil der Beschäftigten in der Landwirtschaft (%)",
       color = "Region") +
  
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right")+
    annotate("text", 
             x = max(data_clean$gdp_per_capita, na.rm = TRUE) , 
             y = max(data_clean$share_employed_agri, na.rm = TRUE), 
             label = paste0("Korrelation: ", round(correlation, 2)), 
             size = 5, 
             hjust = 1))

ggsave("GDP_Agri.png", path = "Results/", width = 9, height = 5,
       device='png', dpi=300, bg = "white")