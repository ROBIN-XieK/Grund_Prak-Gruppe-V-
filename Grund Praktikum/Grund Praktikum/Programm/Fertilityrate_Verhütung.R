library(jsonlite)

# Fetch the data
df1 <- read.csv("https://ourworldindata.org/grapher/fertility-vs-contraception.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/fertility-vs-contraception.metadata.json?v=1&csvType=full&useColumnShortNames=true")

colnames(df1) <- c("Entity", "Code", "Year", "FertilityRate", "Nutzungsrate von Verhütungsmitteln")


library(dplyr)










df1_clean <- na.omit(df1)

# 将所有空列名或NA列名替换为默认名称
colnames(df1_clean) <- make.names(colnames(df1_clean), unique = TRUE)



# 筛选固定年份的数据
years_to_plot <- c(2018)

filtered_years_df <- df1_clean %>%
  filter(Year %in% years_to_plot)

# 查看筛选后的数据

library(ggplot2)             # 加载ggplot2包

# 检查列名
colnames(filtered_years_df)

# 替换列名中的空格
colnames(filtered_years_df) <- gsub(" ", "_", colnames(filtered_years_df))

# 再次检查列名
colnames(filtered_years_df)




# 重命名列名，替换空格
colnames(filtered_years_df) <- gsub(" ", "_", colnames(filtered_years_df))








scatter_plot <- ggplot(filtered_years_df, 
                       aes(x = Nutzungsrate.von.Verhütungsmitteln, 
                           y = FertilityRate, 
                           color = Entity)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(
    title = "Fertility Rate vs. Nutzungsrate von Verhütungsmitteln",
    x = "Nutzungsrate von Verhütungsmitteln",
    y = "Fertility Rate",
    color = "Entity"
  ) +
  theme(legend.position = "none")

# 显示图形
print(scatter_plot)

    
