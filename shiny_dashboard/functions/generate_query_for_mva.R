generate_query_for_mva <- function(database, table, input_columns, filter_columns, filter_list, table_info, all_group_by, sample_mode = TRUE, n_rows){
  
  
  select_query    <- generate_select_query(input_columns, "missing_values")
  sql_where_query <- generate_sql_query(filter_columns, filter_list, table_info, input_columns, table)
  
  # All groups to group by
  # Only select unique ones
  
  all_group_by <- all_group_by %>% {glue("`{.}`")} %>%  glue_collapse(", ")
  group_by_clause <- NULL
  
  if(NROW(all_group_by) > 0){
    
    group_by_clause <- all_group_by %>% glue_collapse(", ") %>% {glue("GROUP BY {.}")}
    
    select_query <- glue("{all_group_by}") %>% 
      glue_collapse(", ") %>% 
      {glue("{.}, {select_query}")}
    
    
  }
  
  database_sample <- ifelse(sample_mode, glue("{database}_{table}_sample LIMIT {n_rows}"), table)
  
  sql_query <- glue("{select_query} FROM {database}_{table}_sample {sql_where_query$where_clause} {group_by_clause}", .na = "", .null  = "") %>% 
    str_remove("^,") %>% 
    {glue("SELECT {.}")}
  
  return(list(sql_query = sql_query))
  
  
}
