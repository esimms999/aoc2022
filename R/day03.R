# AoC2022: Day03
# The answers are x and y.

library(tidyverse)

# Read in example or full data
 data_day03_01 <- read_table("Data/data_day03_01_example", col_names = "fullString")
#data_day02_01 <- read_table("Data/data_day02_01_full",
#  col_names = c("elf_choice", "my_choice"),
#  skip_empty_rows = FALSE
#)

# ----- Part1 -----
#
df <- data_day03_01 %>%
   mutate(fullStringLength=str_length(fullString),
          halfStringLength=fullStringLength/2,
          firstHalf=substr(fullString,1,halfStringLength),
          secondHalf=substr(fullString, halfStringLength+1,fullStringLength))

df2 <-df %>%
  mutate(dupLetter=str_extract(firstHalf,paste0("[",secondHalf,"]")))%>%
  mutate(score=str_locate(lowerUpper,dupLetter)[1])


lowerUpper <- str_c(paste(letters,collapse=''),
paste(LETTERS, collapse='') )
lowerUpper

# ----- Part 2 -----
#

# Structure data:

# ----- Print results -----

