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
countries_to_plot2 <- c("World","Asia", "Africa" ,"Europe" ,"Latin America and the Caribbean (SDG)" ,"North America")
filtered_df2 <- df2 %>%
  filter(Entity %in% countries_to_plot2 & Year >= 1990)

# 创建折线图
print(ggplot(filtered_df2, aes(x = Year, y = `Under-5.mortality.rate`, color = Entity)) +
  geom_line(size = 0.5) +                 # 折线
  geom_point(size = 1, shape = 16) +   # 数据点
  labs(
    title = "Kindersterblichkeitsrate im Zeitverlauf",
    subtitle = "Der geschätzte Anteil der Neugeborenen, die vor Erreichen des fünften Lebensjahres sterben.",
    x = "Jahr",
    y = "Sterblichkeitsrate unter 5 Jahren",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )
)

ggsave("Mortalityrate.png", path = "Results/", width = 9, height = 5,
       device='png', dpi=300, bg = "white")