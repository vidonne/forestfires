# Load needed packages
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

# Import and load raw forest fires data and reordered the months and day
forest.fires <- read_csv("data/forestfires.csv") %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) %>%
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

# Fires grouped by months
forest.fires.months <- forest.fires %>%
  group_by(month) %>%
  summarise(numFiresPerMonths = n())

# Fires grouped by day
forest.fires.day <- forest.fires %>%
  group_by(day) %>%
  summarise(numFiresPerDay = n())

# Number of fires per months Bar graph
ggplot(data = forest.fires.months) +
  aes(x = month, y = numFiresPerMonths) +
  geom_bar(stat = "identity") +
  labs(title = "Total number of fires by month", x = "Month", y = "Number of fires")

# Number of fires per day Bar graph
ggplot(data = forest.fires.day) +
  aes(x = day, y = numFiresPerDay) +
  geom_bar(stat = "identity")

# Create boxplot function for months and day for many variables
CreateBoxplot <- function(x, y) {
  ggplot(data = forest.fires) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}

# assign value to x and y
x.var.month <- names(forest.fires)[3]
x.var.day <- names(forest.fires)[4]
y.var <- names(forest.fires)[5:12]

# plot for month and day using purrr
map2(x.var.month, y.var, CreateBoxplot)
map2(x.var.day, y.var, CreateBoxplot)
map2


