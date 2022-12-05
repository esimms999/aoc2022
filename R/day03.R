# AoC2022: Day03
# The answers are 7581 and y.

library(tidyverse)

# Read in example or full data
# data_day03_01 <- read_table("Data/data_day03_01_example", col_names = "fullString")
data_day03_01 <- read_table("Data/data_day03_01_full", col_names = "fullString")

# ----- Part1 -----
#
lowerUpper <- str_c(
  paste(letters, collapse = ""),
  paste(LETTERS, collapse = "")
)

df <- data_day03_01 %>%
  mutate(
    fullStringLength = str_length(fullString),
    halfStringLength = fullStringLength / 2,
    firstHalf = substr(fullString, 1, halfStringLength),
    secondHalf = substr(fullString, halfStringLength + 1, fullStringLength)
  ) %>%
  mutate(dupLetter=str_extract(firstHalf,
                               paste0("[", secondHalf, "]")))

# Terrible time trying to find letter's position ... due to
# vectorization(?). Forced with this mess, but there must
# be a better way!
dupLetter_vector<-df$dupLetter
dupLetter_position<-str_locate(lowerUpper,dupLetter_vector)
total_score<-sum(dupLetter_position[,1])
cat("Part 1: Total score: ", total_score, "\n")

# ----- Part 2 -----
#

# Structure data:

# ----- Print results -----
