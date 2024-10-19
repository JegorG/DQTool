# This function will:
# Create a sample
# Mapping of unique values
# upload a new entry
# It is used when uploading new data

create_sample_mapping_log <- function(database, table, extra_columns, overwrite = FALSE){
  
  print("Getting sample")
  
  table_sample <- select_sample_from_sql_safe(database, table, 10000)
  check_if_error_and_message(table_sample$error, "select_sample_from_sql")
  
  print("Sample Done")
  
  req(NROW(table_sample$result) > 0)
  
  # Create a table with information about the columns
  
  print(now())
  
  column_information_df <- get_column_information_safe(database, table, table_sample$result, extra_unique_columns = extra_columns)
  check_if_error_and_message(column_information_df$error, "get_column_information")
  print(now())
  print("Column information done")
  
  # Create a table of name and type

  req(column_information_df$result$unique_values_table)
  
 
  dbWriteTable(tool_db, glue("{database}_{table}_unique_val"), column_information_df$result$unique_values_table, overwrite = overwrite)
  dbWriteTable(tool_db, glue("{database}_{table}_table_info"), column_information_df$result$table_info_numeric_vals, overwrite = overwrite)
  dbWriteTable(tool_db, glue("{database}_{table}_sample"), table_sample$result, overwrite = overwrite)
  
  # Update the log
  
  current_time <- now() %>% as.character() %>% str_sub(1, 19)
  
  # If overwrite, then update the old entry
  if(overwrite){
    query <- glue("
    UPDATE log_table
    SET deleted_date = '{current_time}',
    active = 0
    WHERE
    database = '{database}' AND
    table_name = '{table}' AND
    active = 1
    ")
    
    dbSendQuery(tool_db, query)
    
  }
  
  log_entry <- tibble(database     = database,
                      table_name   = table,
                      upload_date  = current_time,
                      deleted_date = NA,
                      active       = 1)
  
  dbAppendTable(tool_db, "log_table", log_entry)
  
  print(dbSendQuery(tool_db, "SELECT * FROM log_table") %>% dbFetch())
  
  return(column_information_df$result$table_info_numeric_vals)
  
}

create_sample_mapping_log_safe <- safely(create_sample_mapping_log)
