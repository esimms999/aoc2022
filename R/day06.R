# AoC2022: Day06
# The answers are 1262 and 3444.

library(tidyverse)

# ----- Part1 -----
#

# Read in example or full data
# data_day06_01<-"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
 data_day06_01 <- read_lines("Data/data_day06_01_full")
#data_day06_01<-"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

#ddd<-list("inter"="")
flagger<-0
for (x in seq(1:(str_length(data_day06_01) - 3)))
  #for (x in seq(1:1))
     {
       sub_ddd <- str_sub(data_day06_01, x, x+3)
#     cat("Part 1: ", x, "   FourChar: ", sub_ddd, "\n")
       sub_ddd_split<-as_vector(str_split(sub_ddd,""))
       non_reps<-unique(sub_ddd_split)
       ifelse(length(non_reps)==4L, y<-x+3, y<-0)
       if (flagger==0 & y>0)
       {
         cat("Answer: ", y, "\n")
         flagger<-1
       }
}

# ----- Part 2 -----
#

# Read in example or full data
# data_day06_01<-"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
data_day06_01 <- read_lines("Data/data_day06_01_full")
#data_day06_01<-"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

flagger<-0
for (x in seq(1:(str_length(data_day06_01) - 13)))
{
  sub_ddd <- str_sub(data_day06_01, x, x+13)
  sub_ddd_split<-as_vector(str_split(sub_ddd,""))
  non_reps<-unique(sub_ddd_split)
  ifelse(length(non_reps)==14L, y<-x+13, y<-0)
  if (flagger==0 & y>0)
  {
    cat("Answer: ", y, "\n")
    flagger<-1
  }
}

# ----- Print results -----
