library(ggplot2)
library(dplyr)
library(patchwork)

#图8
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/healthcare-expenditure-vs-gdp.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
data <- read.csv(url)
# 数据清洗：去除缺失值，并确保数据大于0
data_clean <- data %>%
  filter(!is.na(ny_gdp_pcap_pp_kd), 
         !is.na(sh_xpd_chex_pp_cd),
         ny_gdp_pcap_pp_kd > 0,
         sh_xpd_chex_pp_cd > 0)
# 绘制散点图：双对数坐标
p1 <- ggplot(data_clean, aes(x = ny_gdp_pcap_pp_kd, 
                       y = sh_xpd_chex_pp_cd, color = owid_region)) +
  geom_point( alpha = 0.8, size = 3) +  # 散点图
  scale_x_log10(  # X轴对数刻度
    breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),
    labels = c("$1,000", "$2,000", "$5,000", "$10,000", "$20,000", "$50,000", "$100,000")
  ) +
  scale_y_log10(  # Y轴对数刻度
    breaks = c(100, 200, 500, 1000, 2000, 5000, 10000),
    labels = c("$100", "$200", "$500", "$1,000", "$2,000", "$5,000", "$10,000")
  ) +
  labs(title = "GDP vs Health Expenditure",
       x = "GDP per Capita (Log Scale, PPP International $)",
       y = "Health Expenditure per Capita (Log Scale, PPP International $)",
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")


#图9
# 读取CSV文件从URL
url <- "https://ourworldindata.org/grapher/child-mortality-vs-health-expenditure.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019"
data <- read.csv(url)
colnames(data)
# 数据清洗：去除缺失值，并确保数据大于0
data_clean <- data %>%
  filter(!is.na(sh_xpd_chex_pp_cd), 
         !is.na(obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births),
         sh_xpd_chex_pp_cd > 0,
         obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births > 0)
# 绘制散点图：双对数坐标
p2 <- ggplot(data_clean, aes(x = sh_xpd_chex_pp_cd, 
                       y = obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births,
                       color = owid_region)) +
  geom_point(alpha = 0.8, size = 3) +  # 绘制散点图
  scale_x_log10(  # X轴对数刻度
    breaks = c(10, 50, 100, 200, 500, 1000, 2000, 5000, 10000),
    labels = c("$10", "$50", "$100", "$200", "$500", "$1,000", "$2,000", "$5,000", "$10,000")
  ) +
  scale_y_log10(  # Y轴对数刻度
    breaks = c(1, 5, 10, 20, 50, 100, 200, 500),
    labels = c("1%", "5%", "10%", "20", "50", "100", "200", "500")
  ) +
  labs(title = "Health Expenditure vs Child Mortality",
       x = "Current Health Expenditure per Capita (Log Scale, PPP $)",
       y = "Child Mortality Rate (Log Scale, per 1,000 live births)",
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "right")

final_plot <- p1 + p2 + plot_layout(guides = "collect")

print(final_plot)

