library(ggplot2)
library(dplyr)

#图2
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/child-mortality-gdp-per-capita.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
data <- read.csv(url)
# 数据清洗：筛选有效数据，去除缺失值，并确保GDP > 0 且 Child Mortality > 0
data_clean <- data %>%
  filter(!is.na(gdp_per_capita), 
         !is.na(under_five_mortality_selected), 
         gdp_per_capita > 0,
         under_five_mortality_selected > 0)
# 更改列名
names(data_clean)[7]<- "population"

correlation_data <- data_clean %>%
  group_by(owid_region) %>%
  summarize(correlation = cor(gdp_per_capita, under_five_mortality_selected, use = "complete.obs")) %>%
  arrange(desc(correlation))

correlation_text <- correlation_data %>%
  mutate(label = paste0(owid_region, ": r = ", round(correlation, 2))) %>%
  pull(label) %>%
  paste(collapse = "\n")

# 绘制散点图，X轴和Y轴都使用对数刻度
 print(ggplot(data_clean, aes(x = gdp_per_capita, 
                                  y = under_five_mortality_selected, color = owid_region)) +
  geom_point(alpha = 0.8,size=2) +  # 散点图
    scale_x_log10(
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),  # X轴对数刻度
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")
  ) +
  scale_y_log10(  # Y轴对数刻度
    breaks = c(0.2, 0.5, 1, 2, 5, 10),  # 自定义Y轴刻度
    labels = c("0.2%","0.5%", "1%", "2%", "5%", "10%")
  ) +
  labs(title = "BIP pro Person vs. Kindersterblichkeit(2019)",
       x ="BIP pro Person(Log Skala)",
       y = "Kindersterblichkeit(Log Skala)",
       color = "Region") +
  theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "right") +
   guides(size = "none")+
    annotate(
      "text", 
      x = max(data_clean$gdp_per_capita, na.rm = TRUE) * 0.85, 
      y = max(data_clean$under_five_mortality_selected, na.rm = TRUE) * 0.65, 
      label = correlation_text, 
      hjust = 1,
      size = 3, color = "black"
    ))

 ggsave("GDP_Mortality.png", path = "Results/", width = 9, height = 5,
        device='png', dpi=300, bg = "white")