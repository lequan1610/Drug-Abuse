score_mdma_2011_2017 <- readr::read_delim(
  "C:/Users/Thijmen/Downloads/score_mdma_wastewater_loads_2011_2017.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
drugs_NL <- score_mdma_2011_2017[108:112,]
df <- drugs_NL

df <- df %>%
       mutate(
             pct_2011_2012 = round(((`2012 mean` - `2011 mean`) / `2011 mean`) * 100, 1),
             pct_2012_2013 = round(((`2013 mean` - `2012 mean`) / `2012 mean`) * 100, 1),
             pct_2013_2014 = round(((`2014 mean` - `2013 mean`) / `2013 mean`) * 100, 1),
             pct_2014_2015 = round(((`2015 mean` - `2014 mean`) / `2014 mean`) * 100, 1),
             pct_2015_2016 = round(((`2016 mean` - `2015 mean`) / `2015 mean`) * 100, 1),
             pct_2016_2017 = round(https://student.vu.nl/tas/public/ssp/content/serviceflow?unid=6dfe0f5911e84cf9b8eef68ccf70aa4e
                   ifelse(
                         city == "Amsterdam",
                         ((`2017 mean` / `2015 mean`)^(1/2) - 1) * 100,  
                         ((`2017 mean` - ifelse(is.na(`2016 mean`), 1, `2016 mean`)) /
                             +                      ifelse(is.na(`2016 mean`), 1, `2016 mean`)) * 100
                     ),
                   1
               )
         )
library(ggplot2)


signed_log <- function(x) {
  sign(x) * log10(abs(x) + 1)
}


selected_cities <- c("Amsterdam", "Utrecht", "Eindhoven")


drugs_long_filtered <- drugs_long %>%
  filter(city %in% selected_cities)


ggplot(drugs_long_filtered, aes(x = year_start, y = signed_log(pct_change), color = city)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2012:2017) +
  labs(
    title = "Yearly Percentage Change of MDMA Found in Sewage Water",
    x = "Year",
    y = "Signed log10 of % Change",
    color = "City"
  ) +
  theme_minimal()
