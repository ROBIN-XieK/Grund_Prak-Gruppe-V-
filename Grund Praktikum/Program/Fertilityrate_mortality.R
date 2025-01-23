library(dplyr)
library(jsonlite)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)

# 下载数据
df1 <- read.csv("https://ourworldindata.org/grapher/children-per-woman-un.csv?v=1&csvType=full&useColumnShortNames=true")
df2 <- read.csv("https://ourworldindata.org/grapher/child-mortality-igme.csv?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df1)[colnames(df1) == "fertility_rate__sex_all__age_all__variant_estimates"] <- "Kinder.pro.Frau"
colnames(df2)[colnames(df2) == "obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births"] <- "Sterblichkeitsrate_unter_5_Jahren"

# 筛选固定年份的数据
years_to_plot <- c(2020)
filtered_years_df1 <- df1 %>% filter(Year %in% years_to_plot)
filtered_years_df2 <- df2 %>% filter(Year %in% years_to_plot)

# 合并数据
df_combined <- merge(filtered_years_df1, filtered_years_df2, by = "Entity")

# 获取国家和对应的大洲信息
world <- ne_countries(scale = "medium", returnclass = "sf")
continent_mapping <- world %>% 
  st_drop_geometry() %>%  # 删除几何信息
  select(name, continent) %>%  # 选择国家名和大洲
  rename(Entity = name)  # 将列名改为与 df_combined 对应

# 合并国家对应的大洲信息
df_combined <- df_combined %>% 
  left_join(continent_mapping, by = "Entity")  # 左连接将大洲信息合并到 df_combined

df_combined_cleaned <- df_combined %>%
  filter(!is.na(continent) & continent != "Seven seas (open ocean)")

# 计算相关系数
correlation <- cor(df_combined_cleaned$Kinder.pro.Frau, 
                   df_combined_cleaned$Sterblichkeitsrate_unter_5_Jahren, 
                   use = "complete.obs")  # 确保忽略 NA 值

# 绘图
scatter_plot <- ggplot(df_combined_cleaned, 
                       aes(x = Kinder.pro.Frau, 
                           y = Sterblichkeitsrate_unter_5_Jahren,
                           color = continent)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(
    title = "Fruchtbarkeitsrate vs. Kinderterblichkeitsrate(2020)",
    x = "Kinder pro Frau",
    y = "Sterblichkeitsrate unter 5 Jahren",
    color = "Region"
  ) +
  theme_minimal() +
  # 添加相关系数的注释
  annotate("text", 
           x = max(df_combined_cleaned$Kinder.pro.Frau, na.rm = TRUE) , 
           y = max(df_combined_cleaned$Sterblichkeitsrate_unter_5_Jahren, na.rm = TRUE) - 10, 
           label = paste0("Korrelation: ", round(correlation, 2)), 
           size = 5, 
           hjust = 1) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right")

# 显示图形
print(scatter_plot)

ggsave("Fertilityrate_mortality.png", path = "Results/", width = 9, height = 5,
       device='png', dpi=300, bg = "white")