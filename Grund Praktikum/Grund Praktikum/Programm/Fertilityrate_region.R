# 加载必要的库
library(ggplot2)
library(jsonlite)
library(dplyr)

# 下载数据
df1 <- read.csv("https://ourworldindata.org/grapher/children-per-woman-un.csv?v=1&csvType=full&useColumnShortNames=true")

# 下载元数据
metadata1 <- fromJSON("https://ourworldindata.org/grapher/children-per-woman-un.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df1)[colnames(df1) == "fertility_rate__sex_all__age_all__variant_estimates"] <- "Children.per.Woman"

# 筛选国家
countries_to_plot2 <- c("Asia (UN)", "Africa (UN)" ,"Europe (UN)" ,"Latin America and the Caribbean (UN)" ,"Northern America (UN)")
filtered_df2 <- df1 %>%
  filter(Entity %in% countries_to_plot2)
# 筛选固定年份的数据
years_to_plot <- c(1960, 1980, 2000, 2020)
filtered_years_df <- filtered_df2 %>%
  filter(Year %in% years_to_plot)

# 绘制柱状图
print(ggplot(filtered_years_df, aes(x = as.factor(Year), y = Children.per.Woman, fill = Entity)) +
  geom_col(position = "dodge", width = 0.7) +  # 柱状图，并列排列
  labs(
    title = "Fertility Rate Comparison in Selected Years",
    subtitle = "Number of children per woman (selected countries)",
    x = "Year",
    y = "Children per Woman",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 7),
    legend.position = "bottom"
  ))
