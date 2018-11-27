## Ryan Elmore
## 27 Nov 2018
## Maine's RCV Results 
## EDA

library(tidyverse)
library(janitor)
library(alluvial)

df <- read_csv("data/me02-ranked-choice.csv") 

## Breakdown of ranking
init_aluv <- df %>% 
  filter(!is.na(first)) %>%
  group_by(first, second, third) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(freq = n / sum(n),
         second = if_else(is.na(second), "NA", second),
         third = if_else(is.na(third), "NA", third))

pdf("fig/ranking.pdf", height = 8.5, width = 11)
alluvial(init_aluv[,1:3], 
         freq = init_aluv$n,
         col = c("#7fc97f", "#beaed4", "#fdc086"),
         border = c("#7fc97f", "#beaed4", "#fdc086"),
         cex = 0.7)
dev.off()
