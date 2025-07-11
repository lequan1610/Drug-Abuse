drugs_NL <- scoremdma20112017[108:112,]
library(tidyverse)
library(ggplot2)

df <- drugs_NL
colnames(df)[1:2] <- c("id", "city")

cols_to_use <- names(df)
cols_to_use <- cols_to_use[11:17]

df_long_means <- df %>%
  pivot_longer(
    cols = all_of(cols_to_use),
    names_to = "year",
    values_to = "mean"
  ) %>%
  mutate(
    year = str_extract(year, "\\d{4}"), 
    year = as.integer(year)
  )

cols_to_use <- names(df)
cols_to_use <- cols_to_use[4:10]

df_long_pop <- df %>%
  pivot_longer(
    cols = all_of(cols_to_use),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = str_extract(year, "\\d{4}"), 
    year = as.integer(year)
  )

head(df_long)

mean_vector <- numeric(7)
mean_vector <- drugs_NL[1,4:10]

#redoing

df_long <- df %>%
  pivot_longer(
    cols = -c("id", "city","country"),
    names_to = c("year", ".value"),
    names_pattern = "(\\d{4}) (.*)"
    )

library(ggplot2)

df_plot_utr <- df_long %>% filter(city == "Utrecht")
df_plot_eid <- df_long %>% filter(city == "Eindhoven")
ggplot(data = df_plot_eid, aes(x = year, y = mean))+
  geom_line(data = df_plot_eid, aes(x = year, y = mean, colour = "green", group=1))


ggplot(df_plot, aes(x = year, y = mean)) + 
  geom_point() +
  geom_smooth()

df_plot_ams <- df_long %>% filter( city == "Amsterdam")
ggplot(data = df_plot_utr, aes(x = year, y = mean)) +
  geom_line(data = df_plot_utr, aes(x = year, y = mean, colour = "red", group=1)) +
  geom_point(data = df_plot_ams, aes(x = year, y = mean, colour = "blue",)) +
  geom_line(data = df_plot_eid, aes(x = year, y = mean, colour = "green", group=1)) +
  scale_color_identity(name="",
                       breaks=c("red","blue","green"
                                ),
                       labels=c("Utrecht", "Amsterdam","Eindhoven"),
                       guide="legend"
                       ) +
  coord_cartesian(ylim = c(0,900))+
  labs(y="mean", x="year") + 
  theme_minimal()

#Population-Weighted Load Score Calculation

#####AMS#####

df_plot_ams <- df_plot_ams %>%
  mutate(PopWeightedLoad = (mean/1000) * population)
print(df_plot_ams[,c("year", "mean", "population", "PopWeightedLoad")])
df_plot_ams <- df_plot_ams %>%
  filter(!is.na(mean))

#####UTR#####

df_plot_utr <- df_plot_utr %>%
  mutate(PopWeightedLoad = (mean/1000) * population)
print(df_plot_utr[,c("year", "mean", "population", "PopWeightedLoad")])

#####EID#####

df_plot_eid <- df_plot_eid %>%
  mutate(PopWeightedLoad = (mean/1000) * population)
print(df_plot_eid[,c("year", "mean", "population", "PopWeightedLoad")])

#Plotting PopWeightedLoad

df_plot_ams <- df_plot_ams%>% filter(!is.na(PopWeightedLoad))

df_combined <- bind_rows(
  df_plot_ams %>% mutate(city = "Amsterdam"),
  df_plot_utr %>% mutate(city = "Utrecht"),
  df_plot_eid %>% mutate(city = "Eindhoven")
) %>%
  filter(!is.na(PopWeightedLoad))

#df_combined[city=="Utrecht"]


ggplot(df_combined, aes(x = year, y = PopWeightedLoad, group = city, colour = city)) +
  geom_line() +
  labs(
    title = "Population-Weighted MDMA Load (2011-2017)",
    x = "Year",
    y = "Population-Weighted Load (mg/day)"
  ) +
  coord_cartesian(ylim = c(0,1000000))
# + 
#  theme_minimal() +
#  theme(legend.position = "right")  

ggplot(df_plot_ams, aes(x=year, y=PopWeightedLoad))+
  geom_line(data = df_plot_ams, colour = "blue",group=1) +
  geom_line(data = df_plot_utr, colour = "red", group=1) +
  geom_line(data = df_plot_eid, colour = "green", group=1) +
  scale_color_identity(breaks=c("red","blue","green"),
                       labels=c("Utrecht", "Amsterdam","Eindhoven")) +
  labs(
    title = "Population-Weighted MDMA Load in Amsterdam (2011-2017)",
    x = "Year",
    y = "Population-Weighted Load (mg/day)",
    colour = "City") + 
  theme(legend.position = "bottom")
