library(dplyr)
library(ggplot2)

setwd("~/git/tidytuesday/data/2020/2020-05-19")
rm(list = ls())

df = read.csv("vb_matches.csv")

plot.df = df %>%
  group_by(year, )