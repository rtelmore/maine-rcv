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

## First reallocation of votes

df %>% 
  filter(!is.na(first)) %>%
  group_by(first) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

## Move Hoar to second choice

df_first <- filter(df, !is.na(first)) %>%
  select(first, second) %>%
  mutate(second = if_else(first == "Hoar", second, first)) %>%
  group_by(first, second) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(second = if_else(is.na(second), "NA", second))

pdf("fig/first-realloc.pdf", height = 8.5, width = 11)
alluvial(df_first[,1:2], 
         freq = df_first$n,
         col = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0",
                 "#f0027f", "#bf5b17"),
         border = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0",
                    "#f0027f", "#bf5b17"),
         cex = 0.7)
dev.off()

filter(df, !is.na(first)) %>%
  select(first, second) %>%
  mutate(second = if_else(first == "Hoar", second, first)) %>%
  group_by(second) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

## Reallocate Bond to second

df_second <- filter(df, !is.na(first)) %>%
  select(first, second, third, fourth) %>%
  mutate(second = if_else(first %in% c("Hoar"), second, first),
         second = if_else(is.na(second), "NA", second)) %>%
  mutate(third = if_else(second == "Bond", third, second),
         third = if_else(is.na(third), "NA", third)) %>%
  mutate(fourth = if_else(third == "Hoar", fourth, third),
         fourth = if_else(is.na(fourth), "NA", fourth)) %>%
  ungroup() %>%
  select(first, second, fourth) %>%
  rename(third = fourth) %>%
  group_by(first, second, third) %>%
  summarize(n = n()) %>%
  ungroup()

pdf("fig/second-realloc.pdf", height = 8.5, width = 11)
alluvial(df_second[,1:3], 
         freq = df_second$n,
         col = c("#ffff99", "#7fc97f", "#beaed4", "#fdc086", "#ffff99", 
                 "#386cb0", "#f0027f", "#bf5b17", "#ff7f00", "#cab2d6", 
                 "#6a3d9a"),
         border = c("#ffff99", "#7fc97f", "#beaed4", "#fdc086", "#ffff99", 
                    "#386cb0", "#f0027f", "#bf5b17", "#ff7f00", "#cab2d6", 
                    "#6a3d9a"),
         cex = 0.7)
dev.off()
