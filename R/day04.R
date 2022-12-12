# AoC2022: Day04
# The answers are 459 and 779.

library(tidyverse)

# Read in example or full data
# Note: modified to replace hyphens with commas!!
# data_day04_01 <- read_csv("Data/data_day04_01_example2", col_names = c("x1","x2","y1","y2"))
data_day04_01 <- read_csv("Data/data_day04_01_full2", col_names = c("x1","x2","y1","y2"))

# ----- Part1 -----
#

df <- data_day04_01 %>%
  mutate(enclosed=ifelse((x1 == y1 | x2==y2 | x1==y2|x2==y1) |
                         (x1 < y1 & x2 >= y1) | (y1 < x1 & y2 >= x1), 1, 0)) %>%
  summarise(total_enclosed = sum(enclosed, na.rm = TRUE))

answer<-df %>%
  pull(total_enclosed)

answer


# ----- Part 2 -----
#

df <- data_day04_01 %>%
  mutate(enclosed=ifelse((x1 ) | (x1 <= y1 & x2 >= y2), 1, 0)) %>%
  summarise(total_enclosed = sum(enclosed, na.rm = TRUE))

answer<-df %>%
  pull(total_enclosed)

answer
# ----- Print results -----
