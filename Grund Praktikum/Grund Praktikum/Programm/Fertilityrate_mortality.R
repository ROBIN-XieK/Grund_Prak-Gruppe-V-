library(dplyr)
library(jsonlite)
library(ggplot2)          

# 下载数据
df1 <- read.csv("https://ourworldindata.org/grapher/children-per-woman-un.csv?v=1&csvType=full&useColumnShortNames=true")

# 下载元数据
metadata1 <- fromJSON("https://ourworldindata.org/grapher/children-per-woman-un.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 下载数据
df2 <- read.csv("https://ourworldindata.org/grapher/child-mortality-igme.csv?v=1&csvType=full&useColumnShortNames=true")

# 下载元数据
metadata2 <- fromJSON("https://ourworldindata.org/grapher/child-mortality-igme.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df1)[colnames(df1) == "fertility_rate__sex_all__age_all__variant_estimates"] <- "Children.per.Woman"

# 修改列名
colnames(df2)[colnames(df2) == "obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births"] <- "Under_5.mortality.rate"





# 筛选固定年份的数据
years_to_plot <- c(2020)
filtered_years_df1 <- df1 %>% filter(Year %in% years_to_plot)










# 筛选固定年份的数据
years_to_plot <- c(2020)
filtered_years_df2 <- df2 %>% filter(Year %in% years_to_plot)

df_combined<-merge.data.frame(filtered_years_df1,filtered_years_df2,by="Entity")




scatter_plot <- ggplot(df_combined, 
                       aes(x = Children.per.Woman, 
                           y = Under_5.mortality.rate,
                           color = Entity)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(
    title = "Fertility Rate vs. Sterblichkeit",
    x = "Children.per.Woman",
    y = "Under_5.mortality.rate",
    color = "Entity"
  ) +
  theme(legend.position = "none")  # 去掉图例

# 显示图形
print(scatter_plot)

