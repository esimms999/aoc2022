# AoC2022: Day02
# The answers are 15691 and 12989.

library(tidyverse)

# Read in example or full data
# data_day02_01 <- read_table("Data/data_day02_01_example", col_names = c("elf_choice", "my_choice"),
#                             skip_empty_rows = FALSE)
data_day02_01 <- read_table("Data/data_day02_01_full", col_names = c("elf_choice", "my_choice"),
                            skip_empty_rows = FALSE)

# ----- Part1 -----
# A/X for Rock, B/Y for Paper, and C/Z for Scissors.
# selection_score: X (Rock): 1, Y (Paper): 2, Z (Scissors): 3
# 0 if you lose, 3 for a draw, and 6 if you win

df <- data_day02_01 %>%
  mutate(elf_choice = case_when(
    elf_choice=="A" ~ "X",
    elf_choice=="B" ~ "Y",
    elf_choice=="C" ~ "Z"
  )) %>%
  mutate(
    selection_score = case_when(
      my_choice == "X"  ~ 1,
      my_choice == "Y"  ~ 2,
      my_choice == "Z"  ~ 3
    )
  ) %>%
  mutate(
    outcome_score = case_when(
      my_choice == elf_choice ~ 3,
      my_choice == "X" & elf_choice == "Y"  ~ 0,
      my_choice == "X" & elf_choice == "Z"  ~ 6,
      my_choice == "Y" & elf_choice == "X"  ~ 6,
      my_choice == "Y" & elf_choice == "Z"  ~ 0,
      my_choice == "Z" & elf_choice == "X"  ~ 0,
      my_choice == "Z" & elf_choice == "Y"  ~ 6
    )
  )   %>%
  mutate(game_score = selection_score + outcome_score) %>%
  summarise(total_score = sum(game_score))

total_score1 <- df %>%
  slice_head(n = 1) %>%
  pull(total_score)


# ----- Part 2 -----
# A/X for Rock, B/Y for Paper, and C/Z for Scissors.
# X = lose, Y = draw, Z = win
# selection_score: X (Rock): 1, Y (Paper): 2, Z (Scissors): 3
# 0 if you lose, 3 for a draw, and 6 if you win

# Structure data:
df <- data_day02_01 %>%
  rename(my_strategy = my_choice) %>%
  mutate(elf_choice = case_when(
    elf_choice=="A" ~ "X",
    elf_choice=="B" ~ "Y",
    elf_choice=="C" ~ "Z"
  )) %>%
  mutate(
    outcome_score = case_when(
      my_strategy == "X"  ~ 0,
      my_strategy == "Y"  ~ 3,
      my_strategy == "Z"  ~ 6
    )
  ) %>%
  mutate(
    selection_score = case_when(
      my_strategy == "X" & elf_choice == "X" ~ 3,
      my_strategy == "X" & elf_choice == "Y" ~ 1,
      my_strategy == "X" & elf_choice == "Z" ~ 2,
      my_strategy == "Y" & elf_choice == "X" ~ 1,
      my_strategy == "Y" & elf_choice == "Y" ~ 2,
      my_strategy == "Y" & elf_choice == "Z" ~ 3,
      my_strategy == "Z" & elf_choice == "X" ~ 2,
      my_strategy == "Z" & elf_choice == "Y" ~ 3,
      my_strategy == "Z" & elf_choice == "Z" ~ 1
    )
  )   %>%
  mutate(game_score = selection_score + outcome_score) %>%
  summarise(total_score = sum(game_score))

total_score2 <- df %>%
  slice_head(n = 1) %>%
  pull(total_score)

# ----- Print results -----
cat("Part 1: Total score: ", total_score1, "\n")
cat("Part 2: Total score: ", total_score2, "\n")
