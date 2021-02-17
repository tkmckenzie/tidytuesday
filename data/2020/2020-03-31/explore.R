library(dplyr)
library(gganimate)
library(ggplot2)
library(tidyr)

setwd("~/git/tidytuesday/data/2020/2020-03-31")
rm(list = ls())

states.df = read.csv("beer_states.csv")
tax.df = read.csv("beer_taxed.csv")
size.df = read.csv("brewer_size.csv")
materials.df = read.csv("brewing_materials.csv")
products.df = read.csv("brewing_products.csv")

# Ideas:
#   (1) Different ratios of malts/syrups per barrel over time
#   (2) On premises vs. bottle/cans vs. keg/barrels sales over time (can plot each state as a point)

# (1)
materials.summary.df = materials.df %>%
  filter(type %in% c("Malt and malt products", "Corn and corn products", "Rice and rice products", "Total Grain products")) %>%
  mutate(type = sapply(type, function(type.single) switch(type.single,
                                                          "Malt and malt products" = "malt",
                                                          "Corn and corn products" = "corn.rice",
                                                          "Rice and rice products" = "corn.rice",
                                                          "Total Grain products" = "total"))) %>%
  group_by(year, month, type) %>%
  summarize(month_current = sum(month_current)) %>%
  pivot_wider(names_from = type, values_from = month_current) %>%
  mutate(corn.rice.percent = corn.rice / total,
         malt.percent = malt / total) %>%
  arrange(year, month) %>%
  ungroup() %>%
  mutate(index = 1:n())

ggplot(materials.summary.df, aes(corn.rice.percent, malt.percent)) +
  geom_path(aes(color = index))

# (2)
sales.location.df = states.df %>%
  pivot_wider(names_from = type, values_from = barrels) %>%
  rename(on.premises = "On Premises", bottles.cans = "Bottles and Cans", kegs.barrels = "Kegs and Barrels") %>%
  mutate(off.premises = bottles.cans + kegs.barrels, total = on.premises + off.premises,
         percent.on.premises = on.premises / total, percent.off.premises = off.premises / total)

ggplot(sales.location.df, aes(off.premises, on.premises)) +
  geom_point() +
  transition_time(year) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Year: {frame_time}")
ggplot(sales.location.df, aes(year, percent.on.premises)) +
  geom_line(aes(path = state), alpha = 0.5) +
  theme_bw()
