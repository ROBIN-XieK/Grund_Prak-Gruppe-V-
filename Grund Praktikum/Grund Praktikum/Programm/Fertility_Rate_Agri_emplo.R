library(ggplot2)
library(dplyr)
library(patchwork)

#图6
# 读取CSV文件从URL
# 定义URL
url_birth_rate <- "https://ourworldindata.org/grapher/crude-birth-rate.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
url_agriculture_labor <- "https://ourworldindata.org/grapher/share-of-the-labor-force-employed-in-agriculture.csv?v=1&csvType=filtered&useColumnShortNames=true"

# 从URL读取数据
birth_rate <- read.csv(url_birth_rate)
agriculture_labor <- read.csv(url_agriculture_labor)

# 数据列名调整
colnames(birth_rate)[colnames(birth_rate) == "birth_rate__sex_all__age_all__variant_estimates"] <- "Birth_Rate"
colnames(agriculture_labor)[colnames(agriculture_labor) == "share_employed_agri"] <- "Agriculture_Labor_Share"

# 数据合并
merged_data <- merge(
  birth_rate %>% select(Entity, Year, Birth_Rate),
  agriculture_labor %>% select(Entity, Year, Agriculture_Labor_Share),
  by = c("Entity", "Year")
)

# 筛选特定年份数据
filtered_data <- merged_data %>%
  filter(Year == 2019)

p1 <- ggplot(filtered_data, aes(x = Birth_Rate, y = Agriculture_Labor_Share, color = Entity)) +
  geom_point(alpha = 1 , size = 3 ) +  # 散点图
  labs(
    title = "Birth Rate vs Agriculture Labor Share (Year: 2019)",
    x = "Birth Rate(%)",
    y = "Share of Labor Force in Agriculture(%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")


#图7
# 读取CSV文件从URL
# 定义URL
url_agriculture_labor <- "https://ourworldindata.org/grapher/share-of-the-labor-force-employed-in-agriculture.csv?v=1&csvType=filtered&useColumnShortNames=true"
url_farm_machinery <- "https://ourworldindata.org/grapher/machinery-per-agricultural-land.csv?v=1&csvType=filtered&useColumnShortNames=true"

# 从URL读取数据
agriculture_labor <- read.csv(url_agriculture_labor)
farm_machinery <- read.csv(url_farm_machinery)

# 数据列名调整
colnames(agriculture_labor)[colnames(agriculture_labor) == "share_employed_agri"] <- "Agriculture_Labor_Share"
colnames(farm_machinery)[colnames(farm_machinery) == "machinery_per_ag_land"] <- "Farm_Machinery_Per_Unit_Land"

# 数据合并
merged_data <- merge(
  agriculture_labor %>% select(Entity, Year, Agriculture_Labor_Share),
  farm_machinery %>% select(Entity, Year, Farm_Machinery_Per_Unit_Land),
  by = c("Entity", "Year")
)

# 筛选特定年份数据
filtered_data <- merged_data %>%
  filter(Year == 2019)


filtered_data <- filtered_data %>%
  filter(Farm_Machinery_Per_Unit_Land > 0, Agriculture_Labor_Share > 0)

p2 <- ggplot(filtered_data, aes(x = Farm_Machinery_Per_Unit_Land, y = Agriculture_Labor_Share, color = Entity)) +
  geom_point(alpha = 1 , size = 3) +  # 散点图
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000),  # X轴对数刻度，包含小于1的刻度
                labels = c("0.01", "0.1", "1", "10", "100", "1000")) +  # 自定义刻度标签
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100),  # Y轴对数刻度
                labels = c("0.01%", "0.1%", "1%", "10%", "100%")) +  # 自定义刻度标签，添加百分号
  labs(
    title = "Farm Machinery vs Agriculture Labor Share (Log Scale, Year: 2019)",
    x = "Farm Machinery Per Unit of Land (Log Scale, in Horsepower per 1000 Hectares)",
    y = "Share of Labor Force in Agriculture (Log Scale, %)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.position = "none")

final_plot <- p1 + p2 + plot_layout(guides = "collect")

print(final_plot)