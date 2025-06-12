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