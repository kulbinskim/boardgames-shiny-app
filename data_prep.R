library(dplyr)
library(stringr)

bgg_dataset <- read.csv2("bgg_dataset.csv")

bgg_dataset$Domains[bgg_dataset$Domains == ""] <- "Unspecified"

games <- select(bgg_dataset, !Mechanics)

domains <- games$Domains %>%
  str_split(", ") %>%
  unlist() %>%
  unique()