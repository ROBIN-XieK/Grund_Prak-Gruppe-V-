# 加载必要的库
library(ggplot2)
library(jsonlite)
library(dplyr)

# 下载数据
df <- read.csv("https://ourworldindata.org/grapher/children-per-woman-un.csv?v=1&csvType=full&useColumnShortNames=true")

# 下载元数据
metadata <- fromJSON("https://ourworldindata.org/grapher/children-per-woman-un.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df)[colnames(df) == "fertility_rate__sex_all__age_all__variant_estimates"] <- "Children.per.Woman"

# 筛选国家
countries_to_plot <- c("World","Asia (UN)", "Africa (UN)" ,"Europe (UN)" ,"Latin America and the Caribbean (UN)" ,"Northern America (UN)")
filtered_df <- df %>%
  filter(Entity %in% countries_to_plot)

# 创建折线图
print(ggplot(filtered_df, aes(x = Year, y = Children.per.Woman, color = Entity)) +
  geom_line(size = 0.5) +                 # 折线
  geom_point(size = 1, shape = 16) +   # 数据点
  labs(
    title = "Fertility Rate Trends Over Time",
    subtitle = "Number of children per woman (selected countries)",
    x = "Year",
    y = "Children per Woman",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  ))


