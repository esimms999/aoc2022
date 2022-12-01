# AoC2022: Day01
# The answers are 69883 and 207576.

library(tidyverse)

# Read in example or full data
# data_day01_01 <- read_table("Data/data_day01_01_example", col_names = "calories", skip_empty_rows = FALSE)
data_day01_01 <- read_table("Data/data_day01_01_full", col_names = "calories", skip_empty_rows = FALSE)

# Structure data: group calories per elf, get total per elf and put in descending order.
df <- data_day01_01 %>%
  mutate(elf_number = 1 + cumsum(is.na(calories))) %>%
  group_by(elf_number) %>%
  summarise(total_calories = sum(calories, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-elf_number) %>%
  arrange(desc(total_calories))

# Get total calories for top elf.
top1 <- df %>%
  slice_head(n = 1) %>%
  pull(total_calories)

# Get combined total calories for top 3 elves.
top3 <- df %>%
  slice_head(n = 3) %>%
  summarize(total_calories = sum(total_calories)) %>%
  pull(total_calories)

cat("Total calories for top elf: ", top1, "\n")
cat("Total calories for top three elves: ", top3, "\n")
