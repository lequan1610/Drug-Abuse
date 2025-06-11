library(readr)
score_mdma_2011_2017 <- read_delim("C:/Users/lequa/Downloads/score_mdma_wastewater_loads_2011_2017.csv", 
delim = ";", escape_double = FALSE, trim_ws = TRUE)

write.csv(score_mdma_2011_2017, "data/scoremdma20112017.csv")