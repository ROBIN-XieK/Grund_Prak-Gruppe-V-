library(ggplot2)
library(dplyr)
library(patchwork)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)

# 图6 - 按大洲分类
url_birth_rate <- "https://ourworldindata.org/grapher/crude-birth-rate.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
url_agriculture_labor <- "https://ourworldindata.org/grapher/share-of-the-labor-force-employed-in-agriculture.csv?v=1&csvType=filtered&useColumnShortNames=true"

birth_rate <- read.csv(url_birth_rate)
agriculture_labor <- read.csv(url_agriculture_labor)

colnames(birth_rate)[colnames(birth_rate) == "birth_rate__sex_all__age_all__variant_estimates"] <- "Birth_Rate"
colnames(agriculture_labor)[colnames(agriculture_labor) == "share_employed_agri"] <- "Agriculture_Labor_Share"

merged_data <- merge(
  birth_rate %>% select(Entity, Year, Birth_Rate),
  agriculture_labor %>% select(Entity, Year, Agriculture_Labor_Share),
  by = c("Entity", "Year")
)

filtered_data <- merged_data %>%
  filter(Year == 2019)

# 获取国家和大洲信息
world <- ne_countries(scale = "medium", returnclass = "sf")
continent_mapping <- world %>%
  st_drop_geometry() %>%
  select(name, continent) %>%
  rename(Entity = name)

# 合并大洲信息
filtered_data <- filtered_data %>%
  left_join(continent_mapping, by = "Entity") %>%
  filter(!is.na(continent) & continent != "Seven seas (open ocean)")

correlation1 <- cor(filtered_data$Birth_Rate, 
                    filtered_data$Agriculture_Labor_Share, 
                   use = "complete.obs")
# 图6绘制
p1 <- ggplot(filtered_data, aes(x = Birth_Rate, y = Agriculture_Labor_Share, color = continent)) +
  geom_point(alpha = 1, size = 5) +
  labs(
    title = "Geburtenrate vs Landwirtschaftliche Arbeitskräfte",
    x = "Geburtenrate (%)",
    y = "Anteil der landwirtschaftlichen Arbeitskräfte (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24)
  )+
  annotate("text", 
           x = min(filtered_data$Birth_Rate, na.rm = TRUE) + 10 , 
           y = max(filtered_data$Agriculture_Labor_Share, na.rm = TRUE) - 5, 
           label = paste0("Korrelation: ", round(correlation1, 2)), 
           size = 8, 
           hjust = 1)

# 图7 - 按大洲分类
url_farm_machinery <- "https://ourworldindata.org/grapher/machinery-per-agricultural-land.csv?v=1&csvType=filtered&useColumnShortNames=true"

farm_machinery <- read.csv(url_farm_machinery)

colnames(farm_machinery)[colnames(farm_machinery) == "machinery_per_ag_land"] <- "Farm_Machinery_Per_Unit_Land"

merged_data2 <- merge(
  agriculture_labor %>% select(Entity, Year, Agriculture_Labor_Share),
  farm_machinery %>% select(Entity, Year, Farm_Machinery_Per_Unit_Land),
  by = c("Entity", "Year")
)

filtered_data2 <- merged_data2 %>%
  filter(Year == 2019, Farm_Machinery_Per_Unit_Land > 0, Agriculture_Labor_Share > 0) %>%
  left_join(continent_mapping, by = "Entity") %>%
  filter(!is.na(continent) & continent != "Seven seas (open ocean)")

# 图7绘制
p2 <- ggplot(filtered_data2, aes(x = Farm_Machinery_Per_Unit_Land, y = Agriculture_Labor_Share, color = continent)) +
  geom_point(alpha = 1, size = 5) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) +
  labs(
    title = "Landmaschinen vs Landwirtschaftliche Arbeitskräfte (2019)",
    x = "Landmaschinen pro Flächeneinheit (PS/kH, Log Skala)",
    y = "Anteil der landwirtschaftlichen Arbeitskräfte (%, Log Skala)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24)
  )

# 合并图形
final_plot <- p1 + p2 + plot_layout(guides = "collect")
print(final_plot)

ggsave("Fertility_Rate_Agri_emplo.png", path = "Results/", width = 25, height = 14,
       device='png', dpi=300, bg = "white")