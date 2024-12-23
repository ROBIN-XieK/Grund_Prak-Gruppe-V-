library(jsonlite)


# Fetch the data
df1 <- read.csv("https://ourworldindata.org/grapher/child-mortality-vs-wasting.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/child-mortality-vs-wasting.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# 数据列名调整
names(df1)[4]<- "Child.mortality"
names(df1)[5]<- "child_wasting"

# 数据清洗：去除缺失

df1_clean <- df1 %>% 
  filter(!is.na(Child.mortality), 
         !is.na(child_wasting)
  )


# 筛选特定年份数据
filtered_df <- df1_clean%>%
  filter(Year == 2018)



print(ggplot(filtered_df, aes(y = Child.mortality, x = child_wasting, color = Entity)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "child mortality vs child wasting",
    y = "child.mortality",
    x = "Prevalence of child wasting" ,
    color = "Entity"
    ) +
  theme_minimal() +
  theme(legend.position = "none"))
  
