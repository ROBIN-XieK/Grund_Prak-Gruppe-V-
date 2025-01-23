# 加载必要的库
library(ggplot2)
library(jsonlite)
library(dplyr)

# 下载数据
df <- read.csv("https://ourworldindata.org/grapher/children-per-woman-un.csv?v=1&csvType=full&useColumnShortNames=true")

# 修改列名
colnames(df)[colnames(df) == "fertility_rate__sex_all__age_all__variant_estimates"] <- "Children.per.Woman"

# 筛选国家
countries_to_plot <- c("China", "India", "Iran", "South Africa", "Colombia", 
                       "Germany", "Japan", "Singapore", "Finland", "Canada")
filtered_df <- df %>%
  filter(Entity %in% countries_to_plot, Year >= 1979, Year <= 2015)

# 自定义颜色
warm_colors <- c(
  "China" = "#D73027",        # 深红色
  "India" = "#FF6F61",        # 珊瑚粉
  "Iran" = "#FEE08B",         # 金黄色
  "South Africa" = "#FFC0CB", # 浅粉红色
  "Colombia" = "#FDAE61"      # 淡橙色
)

cool_colors <- c(
  "Germany" = "#2B83BA",      # 深蓝色
  "Japan" = "#6A51A3",        # 紫色
  "Singapore" = "#1A9850",    # 绿色
  "Finland" = "#B3DE69",      # 青色
  "Canada" = "#41B6C4"        # 深青蓝色
)

# 合并颜色并按照指定顺序排列
all_colors <- c(warm_colors, cool_colors)
ordered_countries <- c("China", "India", "Iran", "South Africa", "Colombia", 
                       "Germany", "Japan", "Singapore", "Finland", "Canada")

# 创建折线图
print(ggplot(filtered_df, aes(x = Year, y = Children.per.Woman, color = factor(Entity, levels = ordered_countries))) +
  geom_line(aes(size = ifelse(Entity %in% c("Germany", "China"), 1.3, 0.8))) +  # 控制线条宽度
  geom_point(size = 1.5, shape = 16) +
  scale_color_manual(values = all_colors) + # 自定义颜色
  scale_size_identity() +                   # 确保 size 是直接使用的值
  labs(
    title = "Fruchtbarkeitsrate im Zeitverlauf",
    subtitle = "Anzahl Kinder pro Frau",
    x = "Jahr",
    y = "Kinder pro Frau",
    color = "Staat"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 3),  # 图例中点大小
    nrow =  2,                      # 图例分两行
    byrow = TRUE                   # 图例按行排列
  )))

ggsave("Fertilityrate_politik.png", path = "Results/", width = 9, height = 5,
       device='png', dpi=300, bg = "white")