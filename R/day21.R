# AoC2022: Day21
# The answers are 152479825094094 and y.

library(tidyverse)

# ----- Functions -----

# Split df into two df's: resolved and unresolved expressions

fn_resolve <- function(df) {
  df_resolve <- df %>%
    filter(str_detect(var_value, "^[:digit:]+$"))
}

fn_unresolve <- function(df) {
  df_unresolve <- df %>%
    filter(!str_detect(var_value, "^[:digit:]+$"))
}

# For resolved, create assignment statements for numeric objects
# The eval of the assignemnt statements will be done in the body
# of the code because I still do not know how to make objects
# created in a function available globally. :(

fn_create_assignment_statements <- function(df) {
  df2 <- df_resolve %>%
    mutate(expr = paste0(var_name, " <- ", var_value, ";")) %>%
    pull(expr)

  statements <- str_c(df2, collapse = " ")
}

fn_flag_both_resolved <- function(df) {
  df2 <- df %>%
    mutate(first_var = str_sub(var_value, 1, 4),
           second_var = str_sub(var_value, 8)) %>%
    mutate(first_exist_flag = sapply(first_var, exists),
           second_exist_flag = sapply(second_var, exists)) %>%
    mutate(both_exist_flag = ifelse(first_exist_flag & second_exist_flag, TRUE, FALSE)) %>%
    select(-first_var, -second_var, -first_exist_flag, -second_exist_flag)
}

fn_new_resolve <- function(df) {
  df2 <- df %>%
    filter(both_exist_flag) %>%
    mutate(var_value = sapply(str2expression(var_value), eval)) %>%
    select(-both_exist_flag)
  df3 <- df %>%
    filter(!both_exist_flag) %>%
    select(-both_exist_flag)

  df4 <- rbind(df2, df3)
}

# Read in example or full data
# data_day21_01 <- read_csv("Data/data_day21_01_example", col_names = "orig_val", show_col_types = FALSE)
data_day21_01 <- read_csv("Data/data_day21_01_full", col_names = "orig_val", show_col_types = FALSE)

# ----- Part 1 -----

df_resolve_unresolve <- data_day21_01 %>%
  separate(orig_val, c("var_name", "var_value"), ": ")

loop_count <- 0

while(!exists("root")) {
  loop_count <- loop_count +1

# Split into resolved (already numbers) and unresolved (expressions).
df_resolve <- fn_resolve(df_resolve_unresolve)
df_unresolve <- fn_unresolve(df_resolve_unresolve)

# For already resolved (i.e. var_value is a number instead of an expression), create the objects.
eval(str2expression(fn_create_assignment_statements(df_resolve)))

if (exists("root")) {
  break
}

# For unresolved (i.e. var_value is an expression), flag if both components are already resolved.
df_both_flagged <- fn_flag_both_resolved(df_unresolve)

# Resolve the values which have both components already resolved.
df_resolve_unresolve <- fn_new_resolve(df_both_flagged)
cat("loop_count:", loop_count, "\n")
}

cat("Part1: ", format(root, scientific = FALSE), "\n")


# ----- Part 2 -----



