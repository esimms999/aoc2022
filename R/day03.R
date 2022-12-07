# AoC2022: Day03
# The answers are 7581 and 2525.

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
df2<-data_day03_01 %>%
 mutate(fS=sapply(str_split(fullString, ""), unique))

num_rows<-nrow(data_day03_01)/3
elf_group<-as_tibble_col(paste0("group",rep(1:num_rows,each=3)),
                         column_name = "group")
df3<-bind_cols(df2,elf_group)
col_names<-as_tibble_col(paste0("fS", rep(1:3,times=num_rows)),
                         column_name = "items")
df4<-bind_cols(df3,col_names)

df5<-df4 %>%
  pivot_wider(id_col=group, names_from = items, values_from = fS)

ddd<-list("inter"="")
for (x in 1:num_rows)
{
  d1<-as_vector(df5$fS1[x])
  d2<-as_vector(df5$fS2[x])
  d3<-as_vector(df5$fS3[x])
  ddd$inter<-str_c(ddd$inter,intersect(intersect(d1,d2),d3))
}

dupLetter_vector<-as_vector(str_split(ddd,""))
dupLetter_position<-str_locate(lowerUpper,dupLetter_vector)
total_score<-sum(dupLetter_position[,1])
cat("Part 2: Total score: ", total_score, "\n")

# ----- Print results -----
