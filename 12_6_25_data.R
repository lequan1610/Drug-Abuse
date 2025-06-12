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
#xValue <- df_long[1:7,4]
#yValue <- df_long[1:7,6]
#data <- data.frame(xValue, yValue)

ggplot(df_plot, aes(x = year, y = mean)) + 
  geom_point() +
  geom_smooth()
#+
#  geom_smooth(method= "lm", se = TRUE, col = "purple")

df_plot_ams <- df_long %>% filter(city == "Amsterdam")
ggplot(data = df_plot_utr, aes(x = year, y = mean)) +
  geom_line(data = df_plot_utr, aes(x = year, y = mean, colour = "red", group=1)) +
  geom_point(data = df_plot_ams, aes(x = year, y = mean, colour = "blue",)) +
  geom_line(data = df_plot_eid, aes(x = year, y = mean, colour = "green", group=1)) +
  scale_color_identity(name="",
                       breaks=c("red","blue","green"
                                ),
                       labels=c("Utrecht", "Amsterdam","Eindhoven"),
                       guide="legend")


