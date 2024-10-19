generate_sql_query <- function(filter_columns, filter_list, table_info, columns, table, limit = 1000){

    where_clause <- ""
    not_null_indices <- map_lgl(filter_list, ~!is.null(.x)) %>% which()
    
    # Only map where there is a selection, if someone opens a filter but does not select anything
    # it will be NULL
  
  if(NROW(filter_columns) > 0 & NROW(not_null_indices) > 0){
    
    where_clause <- map2(filter_columns[not_null_indices], filter_list[not_null_indices], generate_where_clause_safe, table_info) 

    where_clause <- where_clause %>% 
    map_chr(~.x$result) %>% 
    glue_collapse(" AND ") %>% 
    {glue("WHERE {.} ")}
  
  }
  
  select_clause <- columns %>% 
    {glue("`{.}`")} %>% 
    glue_collapse(", ") %>% 
    {glue("SELECT {.} FROM {table} ")}
  
  query <- glue("{select_clause} {where_clause} LIMIT {limit}")
  
  return(list(query = query, select_clause = select_clause, where_clause = where_clause))
  
  return(query)
  
  
}