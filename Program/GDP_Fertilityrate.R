library(ggplot2)
library(dplyr)

#图1
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/children-per-woman-fertility-rate-vs-level-of-prosperity.csv?v=1&csvType=filtered&useColumnShortNames=true"
data <- read.csv(url)

correlation <- cor(data$rgdpo_pc, 
                   data$fertility_rate__sex_all__age_all__variant_estimates, 
                   use = "complete.obs")
# 绘制散点图
print(ggplot(data, aes(x = rgdpo_pc, 
                 y = fertility_rate__sex_all__age_all__variant_estimates , color = owid_region)) +
  geom_point(alpha = 1 , size = 2) +  # 散点图
  scale_x_log10(
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),  # 设置刻度点
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")  # 设置刻度标签
  ) +
  labs(title = "BIP pro Kopf vs Geburten pro Frau (2019)",
       x = "BIP pro Kopf (Log Skala)",
       y = "Geburten pro Frau",
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
             x = max(data$rgdpo_pc, na.rm = TRUE) , 
             y = max(data$fertility_rate__sex_all__age_all__variant_estimates, na.rm = TRUE), 
             label = paste0("Korrelation: ", round(correlation, 2)), 
             size = 5, 
             hjust = 1))

ggsave("GDP_Fertilityrate.png", path = "Results/", width = 9, height = 5,
       device='png', dpi=300, bg = "white")