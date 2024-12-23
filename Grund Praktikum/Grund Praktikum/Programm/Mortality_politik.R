library(jsonlite)

# Fetch the data
df2 <- read.csv("https://ourworldindata.org/grapher/child-mortality-vs-electoral-democracy.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata2 <- fromJSON("https://ourworldindata.org/grapher/child-mortality-vs-electoral-democracy.metadata.json?v=1&csvType=full&useColumnShortNames=true")

names(df2)[4]<- "Child.mortality"
names(df2)[5]<- "Electoral.democracy"

df2_clean <- df2 %>% 
  filter(!is.na(Child.mortality), 
         !is.na(Electoral.democracy)
  )

filtered_df2 <- df2_clean%>%
  filter(Year == 2022)

print(ggplot(filtered_df2, aes(x = Electoral.democracy, y = Child.mortality, color = Entity)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Child mortality rate vs. electoral democracy",
    subtitle = "Electoral democracy is based on the expert estimates and index by V-Dem, ranging from 0 to 1 (most democratic).",
    x = "Electoral.democracy",
    y = "Child.mortality" ,
    color = "Entity"
  ) +
  theme_minimal() +
  theme(legend.position = "none"))
