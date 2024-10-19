# Function that is used within get_column_information to generate a table with unique values
# sort of a mapping table


create_unique_values_table <- function(column_information_df_long){
  
  unique_values <- 
    column_information_df_long %>% filter(what == 'unique_val') %>% 
    mutate(row = row_number())
  
  seperated_values <- map2_df(unique_values$value, 1:NROW(unique_values$value), ~data.table(row = .y, value = str_split(.x, pattern = ",")[[1]]))

    unique_values_table <- unique_values %>% 
    select(name, type, what, row) %>% 
    inner_join(seperated_values) %>% 
    select(-row) %>% 
    mutate(value = ifelse(type == "DATE", num_to_date(value), value))
  
  
  return(unique_values_table)
  
}

