# # Generate some random data
# 
# library(data.table)
# library(collapse)
# library(glue)
# library(tidyverse)
# library(lubridate)
# 
# set.seed(1)
# n_row <- 5000000
# n_col <- 100
# 
# n_date_cols <- 0.1 * n_col
# n_single_letter_cols <- 0.15 * n_col
# n_multiple_letters_cols <- 0.15 * n_col
# n_numeric_cols <- 0.25 * n_col
# n_integer_cols <- 0.25 * n_col
# n_boolean_cols <- 0.1 * n_col
# 
# generate_date_col <- function(n_row, n_col, min_date = '1990-01-01', max_date = "2024-12-31"){
# 
#   min_date <- ymd(min_date)
#   max_date <- ymd(max_date)
#   whole_seq <- seq(min_date, max_date, by = "day")
# 
#   min_dates  <- sample(whole_seq, size = n_col, replace = TRUE)
#   max_dates  <- sample(whole_seq, size = n_col, replace = TRUE)
# 
#   dates <- map2(ymd(min_dates), ymd(max_dates), ~sample(seq(min(.x, .y), max(.x, .y), by = "day"), size = n_row, replace = TRUE)) %>%
#     map(as.character) %>%
#     unlist()
# 
#   df <- matrix(dates, nrow = n_row, ncol = n_col) %>%
#     data.table()
# 
#   names(df) <- glue("date_col_{1:n_col}")
# 
#   return(df)
# }
# 
# generate_n_single_letter_col <- function(n_row, n_col){
# 
#   df <-
#   sample(LETTERS, size = n_row * n_col, replace = TRUE) %>%
#   matrix(nrow = n_row, ncol = n_col) %>%
#   data.table()
# 
#   names(df) <- glue("single_ltr_col_{1:n_col}")
#   return(df)
# 
# 
# }
# 
# generate_n_multiple_letter_col <- function(n_row, n_col){
# 
#   one_col <- sample(LETTERS, size = n_row, replace = TRUE)
# 
#   glue_fun <- function(n_times){
# 
#     glue_string <-
#       replicate(n_times, "{sample(one_col, size = n_row, replace = FALSE)}") %>%
#       glue_collapse()
# 
#     result <- glue(glue_string)
#     return(result)
# 
#   }
# 
#   string_length <- sample(c(2, 5, 9), n_col, replace = TRUE, prob = c(0.6, 0.20, 0.20))
# 
#   ltrs <- map(string_length, glue_fun, .progress = TRUE)
# 
#   df <- ltrs %>%
#     unlist() %>%
#     sample(., NROW(.), replace = FALSE) %>%
#     matrix(nrow = n_row, ncol = n_col) %>%
#     data.table()
# 
#   names(df) <- glue("multiple_ltr_col_{1:n_col}")
#   return(df)
# 
# 
# }
# 
# 
# generate_num_col <- function(n_row, n_col){
# 
#   mean_val <- round(rnorm(n_col, mean = 5, sd = 300), 0)
# 
#   df <-
#   map2(mean_val, n_row, ~rnorm(.y, mean = .x, sd = min(.y^3, 3000)), .progress = TRUE) %>%
#     unlist() %>%
#     round(4) %>%
#     matrix(nrow =n_row, ncol = n_col) %>%
#     data.table()
# 
#   names(df) <- glue("numeric_col_{1:n_col}")
#   return(df)
# 
# }
# 
# generate_integer_cols <- function(n_row, n_col){
# 
#   lambda <- runif(n_col, 1, 1000) %>% round(0)
# 
#   df <-
#   map(lambda, ~rpois(n = n_row, .x)) %>%
#     unlist() %>%
#     matrix(nrow = n_row, ncol = n_col) %>%
#     data.table()
# 
#   names(df) <- glue("integer_col_{1:n_col}")
#   return(df)
# 
# }
# 
# generate_boolean_cols <- function(n_row, n_col){
# 
#   proprtion <- runif(n_col)
# 
#   df <-
#     map(proprtion, ~sample(c(TRUE, FALSE), n_row, replace = TRUE, prob = c(.x, 1-.x))) %>%
#     unlist() %>%
#     matrix(nrow = n_row, ncol = n_col) %>%
#     data.table()
# 
#   names(df) <- glue("boolean_col_{1:n_col}")
#   return(df)
# 
# }
# 
# date_cols <- generate_date_col(n_row, n_date_cols)
# single_letter_col <- generate_n_single_letter_col(n_row, n_single_letter_cols)
# multiple_letter_col <- generate_n_multiple_letter_col(n_row, n_multiple_letters_cols)
# numeric_cols <- generate_num_col(n_row, n_numeric_cols)
# integer_cols <- generate_integer_cols(n_row, n_integer_cols)
# boolean_cols <- generate_boolean_cols(n_row, n_boolean_cols)
# 
# df <- cbind(date_cols,
#             single_letter_col,
#             multiple_letter_col,
#             numeric_cols,
#             integer_cols,
#             boolean_cols)
# 
# # Create some month end columns
# 
# month_end <- c("date_col_1", "date_col_2", "date_col_3")
# 
# df[, (month_end) := map(.SD, ~(ceiling_date(ymd(.x), "month"))), .SDcols = month_end]
# 
# # Generate some missing values
# 
# 
# cols_missing <- sample(colnames(df)[4:100], 50)
# missing_pct  <- c(runif(25, 0, 0.05), runif(15, 0.05, 0.15), runif(9, 0.15, 1), 1)
# 
# generate_missing_values <- function(data, pct_missing){
#   
#   rows_missing <- sample(1:NROW(data), round(NROW(data) * pct_missing, 0))
#   data[rows_missing] <- NA
#   
#   return(data)
#   
# }
# 
# df[, (cols_missing) := map2(.SD, missing_pct, generate_missing_values), .SDcols = cols_missing]
# 
# fwrite(df, "C:/Users/DELL/Documents/shiny_work/df.csv")
# 
# # df <- fread("df.csv")
# 
# 
# 
