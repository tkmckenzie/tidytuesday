library(dplyr)
library(ggplot2)

setwd("~/git/tidytuesday/data/2020/2020-05-12")
rm(list = ls())

eruption.df = read.csv("eruptions.csv")
event.df = read.csv("events.csv")
sulfur.df = read.csv("sulfur.csv")
tree.ring.df = read.csv("tree_rings.csv")
volcano.df = read.csv("volcano.csv")

# Time span for each dataset:
#   * eruption: 11,345 BCE to 2020 CE
#   * event: 9650 BCE to 2020 CE
#   * sulfur: 500 CE to 706 CE
#   * tree ring: 1 CE to 2000 CE
#   * volcano: NA

# Ideas:
#   (1) # of actively erupting volcanoes vs. sulfur/tree ring
#   (2) sulfur vs. tree ring

# (2) sulfur vs. tree ring
sulfur.df$year.int = floor(sulfur.df$year)
sulfur.df.annual = sulfur.df %>%
  group_by(year.int) %>%
  summarize(neem = mean(neem, na.rm = TRUE), wdc = mean(wdc, na.rm = TRUE)) %>%
  rename(year = year.int)

plot.df = sulfur.df.annual %>%
  inner_join(tree.ring.df, by = "year")

ggplot(plot.df, aes(wdc, n_tree)) +
  geom_point()
