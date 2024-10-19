# A function that gets column information from a table and returns:
# type
# n_unique values (if less than 30 or snapshotdate, contract id)
# min
# max
# mean
# n_missing


get_column_information <- function(database, table, table_sample, extra_unique_columns = c(), tool_db_ = "tool_db"){
  
  # information about the table columns

  table_info <- get_columns_and_types(database, table)
  
  date_columns    <- table_info %>% filter(type == "DATE") %>% pull(name)    %>% unique()
  text_columns    <- table_info %>% filter(type == "TEXT") %>% pull(name)    %>% unique()
  numeric_columns <- table_info %>% filter(type == "REAL") %>% pull(name)    %>% unique()
  integer_columns <- table_info %>% filter(type == "INTEGER") %>% pull(name) %>% unique()
  
  # Get the sample
  
  # table_sample <- 
  #   dbSendQuery(get(tool_db_), glue("SELECT * FROM {database}_{table}_sample"))  %>%
  #   dbFetch() %>% 
  #   data.table()
  
  all_columns <- names(table_sample)
  
  # Calculate unique values
  
  n_distinct_values <- table_sample %>% fndistinct()
  
  aggregate_on_columns <- 
    n_distinct_values[n_distinct_values < 35] %>% names() %>% 
    c(., extra_unique_columns) %>% 
    unique() # This because if extra_unique_columns overlaps with the ones that has less than 35 values
  
  
  #n_missing
  
  non_text_columns <- c(date_columns, numeric_columns, integer_columns)
  non_text <- glue(", SUM(`{non_text_columns}` IS NULL) AS `{non_text_columns}_SEP_n_missing`\n ") %>% glue_collapse()
  text_    <- glue(", SUM(`{text_columns}` IS NULL OR TRIM(`{text_columns}`) = '') AS `{text_columns}_SEP_n_missing`\n ") %>% glue_collapse()
  
  # min, max, mean
  
  numeric_cols <- c(numeric_columns, integer_columns)
  numeric_info <- glue(", MIN(`{numeric_cols}`)  AS `{numeric_cols}_SEP_minimum_value`\n
                   , MAX(`{numeric_cols}`)       AS `{numeric_cols}_SEP_maximum_value`\n
                   , AVG(`{numeric_cols}`)       AS `{numeric_cols}_SEP_mean_value`\n ") %>% glue_collapse()
  
  date_info <- glue(", MIN(`{date_columns}`)  AS `{date_columns}_SEP_minimum_value`\n
                     , MAX(`{date_columns}`)  AS `{date_columns}_SEP_maximum_value`\n ") %>% glue_collapse()

  # Create queries
    
  unique_values_query <- glue(", GROUP_CONCAT(DISTINCT(`{aggregate_on_columns}`)) AS `{aggregate_on_columns}_SEP_unique_val`\n ") %>% glue_collapse()
  
  # Final query
  
  all_queries <- paste(non_text, text_, unique_values_query, numeric_info, date_info) %>% 
    str_remove("^,") # Remove the starting comma
  
  final_query <- glue("SELECT {all_queries} FROM {table}")

  
  # Get the data
   column_information_df <- 
    dbSendQuery(get(database), final_query) %>% 
    dbFetch() %>% 
    data.table()
  
  # Create the unique values table and numeric table
  
  column_information_df_long <- 
    column_information_df %>% 
    mutate_all(as.character) %>% 
    pivot_longer(everything()) %>% 
    separate_wider_delim(name, delim = "_SEP_", names = c("name", "what")) %>% 
    left_join(table_info %>% select(name, type), .)
  
  unique_values_table <- create_unique_values_table(column_information_df_long)
  
  table_info_numeric_vals <- 
    column_information_df_long %>% 
    filter(what != 'unique_val') %>% 
    arrange(name) %>% 
    mutate(value = ifelse(type == "DATE" & str_detect(what, "maximum|minimum"), num_to_date(value), value)) %>% 
    pivot_wider(names_from = what, values_from = value) %>% 
    mutate(unique_values_available = ifelse(name %in% aggregate_on_columns, TRUE, FALSE))
  
  output <- list(unique_values_table = unique_values_table, table_info_numeric_vals = table_info_numeric_vals)
  
  return(output)
  
}

get_column_information_safe <- safely(get_column_information)

