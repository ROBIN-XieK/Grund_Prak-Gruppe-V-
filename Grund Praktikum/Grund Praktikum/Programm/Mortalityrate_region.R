# 加载必要的库
library(ggplot2)
library(jsonlite)
library(dplyr)

# 下载数据
df2 <- read.csv("https://ourworldindata.org/grapher/child-mortality-igme.csv?v=1&csvType=full&useColumnShortNames=true")

# 下载元数据
metadata2 <- fromJSON("https://ourworldindata.org/grapher/child-mortality-igme.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df2)[colnames(df2) == "obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births"] <- "Under-5.mortality.rate"

# 筛选国家
countries_to_plot2 <- c("Asia", "Africa" ,"Europe" ,"Latin America and the Caribbean (SDG)" ,"North America")
filtered_df2 <- df2 %>%
  filter(Entity %in% countries_to_plot2)

# 筛选固定年份的数据
years_to_plot <- c(1990, 2000, 2010, 2020)
filtered_years_df <- filtered_df2 %>%
  filter(Year %in% years_to_plot)

# 绘制柱状图
print(ggplot(filtered_years_df, aes(x = as.factor(Year), y = `Under-5.mortality.rate`, fill = Entity)) +
  geom_col(position = "dodge", width = 0.7) +  # 柱状图，并列排列
  labs(
    title = "Child mortality rate",
    subtitle = "The estimated share of newborns who die before reaching the age of five.",
    x = "Year",
    y = "Under-5.mortality.rate",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 9),
    legend.position = "bottom"
  ))
