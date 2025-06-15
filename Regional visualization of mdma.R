library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyverse)

score_mdma_2011_2017 <- readr::read_delim(
  "C:/Users/Thijmen/Downloads/score_mdma_wastewater_loads_2011_2017.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
drugs_NL <- score_mdma_2011_2017[108:112,]
df <- drugs_NL
colnames(df)[colnames(df) == "...1"] <- "city"
netherlands <- ne_countries(scale = "medium", country = "Netherlands", returnclass = "sf")
df$longitude  = c(4.9041, 5.4778, 5.0803, 4.8681, 5.1214) 
df$latitude  = c(52.3676, 51.4416, 52.0306, 52.0282, 52.0907)
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
ggplot() +
  geom_sf(data = netherlands, fill = "gray", color = "black") +
  geom_sf(data = df_sf, aes(color = `2017 mean`), size = 5) +
  scale_color_gradientn(
    colors = c("green", "orange", "red"),
    name = "MDMA (mg/ 1000 people/day)"
  ) +
  geom_text(
    data = df,
    aes(x = longitude, y = latitude, label = city),
    vjust = ifelse(df$city == "Oudewater", 1.5, -1.5),
  ) +
  coord_sf(xlim = c(4, 7), ylim = c(51, 53)) +
  labs(title = "Mean levels of MDMA in wastewater in 2017 (mg/1000 people/day") +
  theme_minimal()

