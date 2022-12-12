# AoC2022: Day04
# The answers are x and y.

library(tidyverse)

# ----- Part1 -----
#
data_day04_01 <- read_delim("Data/data_day04_01_example",delim=",",col_names=c("A1A2","B1B2"))
df1 <- data_day04_01 %>%
  mutate(A=str_split(A1A2,"-"))
# Set up example or full data
# Example:
stack1<-"ZN"
stack2<-"MCD"
stack3<-"P"

move_blocks <- function(amount, from_to_stack) {
  for (i in 1:amount) {
    str_sub(from_to_stack[2],str_length(from_to_stack[2])+1) <-
      str_sub(from_to_stack[1],str_length(from_to_stack[1]))
    str_sub(from_stack[1],str_length(from_stack[1])) <- ""
    #cat("to_stack: ", to_stack, "   from_stack: ", from_stack, "\n")
  }
  return(from_to_stack)
}

move_blocks_from <- function(amount, from_stack, to_stack) {
  for (i in 1:amount) {
    str_sub(from_stack,str_length(from_stack)) <- ""
    #cat("to_stack: ", to_stack, "   from_stack: ", from_stack, "\n")
  }
  return(from_stack)
}

cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack1 <- move_blocks(1,c(stack2,stack1))
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack3 <- move_blocks(3,c(stack1,stack3))
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack1 <- move_blocks(2,c(stack2,stack1))
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack2 <- move_blocks(1,c(stack1,stack2))
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

# ----- Part 2 -----
#

# ----- Print results -----
