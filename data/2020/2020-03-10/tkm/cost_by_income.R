library(dplyr)
library(ggplot2)

setwd("~/git/tidytuesday/data/2020/2020-03-10/tkm")

df = read.csv("../tuition_income.csv", stringsAsFactors = FALSE)
df$income_lvl = gsub("_", ",", df$income_lvl)
df$income_lvl = gsub("[ ]{1}(?=[0-9]{1})", " $", df$income_lvl, perl = TRUE)
df$income_lvl = gsub("^(?=[0-9]{1})", "$", df$income_lvl, perl = TRUE)

df$income_lvl_rank = sapply(df$income_lvl, function(income.string) switch(income.string,
                            "$0 to $30,000" = 1,
                            "$30,001 to $48,000" = 2,
                            "$48,001 to $75,000" = 3,
                            "$75,001 to $110,000" = 4,
                            "Over $110,000" = 5))

df = df %>%
  filter(year == 2018)

# On average, net costs increase with income, as expected
plot.df = df %>%
  group_by(campus, income_lvl) %>%
  summarize(cost = mean(net_cost))
ggplot(plot.df, aes(income_lvl, cost)) +
  geom_point() +
  facet_grid(campus ~ ., scales = "free_y") +
  xlab("Income level") +
  ylab("Average net cost ($ / year)") +
  theme_bw()

# However, ~ half of all colleges do not have increasing costs with income
# About 25% of colleges have higher costs for income below $30,000 than between $30,001 and $48,000
df %>%
  group_by(name, campus) %>%
  arrange(income_lvl_rank) %>%
  summarize(increasing.cost = all(diff(net_cost) >= 0)) %>%
  group_by(campus) %>%
  summarize(proportion.increasing.cost = mean(increasing.cost))
df %>%
  group_by(name, campus) %>%
  arrange(income_lvl_rank) %>%
  summarize(increasing.cost = net_cost[1] > net_cost[2]) %>%
  group_by(campus) %>%
  summarize(proportion.nonincreasing.cost = mean(increasing.cost))

# Proportion of colleges with non-increasing costs vary by state
plot.df = df %>%
  group_by(name, campus, state) %>%
  arrange(income_lvl_rank) %>%
  summarize(increasing.cost = all(diff(net_cost) >= 0)) %>%
  group_by(state, campus) %>%
  summarize(proportion.nonincreasing.cost = 1 - mean(increasing.cost)) %>%
  filter(campus == "On Campus") %>%
  filter(state != "DC")
plot.df$region = tolower(state.name[sapply(plot.df$state, function(state) which(state.abb == state))])

states.map = map_data("state")
map.df = left_join(states.map, plot.df, by = "region")

ggplot(map.df, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = proportion.nonincreasing.cost)) +
  scale_fill_viridis_c(option = "C") +
  theme_bw() +
  theme(legend.position = "top")
