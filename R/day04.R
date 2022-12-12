# AoC2022: Day05
# The answers are x and y.

library(tidyverse)

# ----- Part1 -----
#

# Set up example or full data
# Example:
stack1<-"ZN"
stack2<-"MCD"
stack3<-"P"

move_blocks_to <- function(amount, from_stack, to_stack) {
  for (i in 1:amount) {
    str_sub(to_stack,str_length(to_stack)+1) <-
      str_sub(from_stack,str_length(from_stack))
    #cat("to_stack: ", to_stack, "   from_stack: ", from_stack, "\n")
  }
  return(to_stack)
}

move_blocks_from <- function(amount, from_stack, to_stack) {
  for (i in 1:amount) {
    str_sub(from_stack,str_length(from_stack)) <- ""
    #cat("to_stack: ", to_stack, "   from_stack: ", from_stack, "\n")
  }
  return(from_stack)
}

cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack1 <- move_blocks_to(1,stack2,stack1)
stack2 <- move_blocks_from(1,stack2,stack1)
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack3 <- move_blocks_to(3,stack1,stack3)
stack1 <- move_blocks_from(3,stack1,stack3)
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack1 <- move_blocks_to(2,stack2,stack1)
stack2 <- move_blocks_from(2,stack2,stack1)
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

stack2 <- move_blocks_to(1,stack1,stack2)
stack1 <- move_blocks_from(1,stack1,stack2)
cat("Stack1: ", stack1, "   Stack2: ", stack2, "   Stack3: ", stack3, "\n")

# ----- Part 2 -----
#

# ----- Print results -----
