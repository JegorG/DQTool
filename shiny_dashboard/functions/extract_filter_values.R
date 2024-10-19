# Function to extract the values for filters for those that are open


generate_where_clause <- function(column_name, filter_list, table_info){
  
  table_info <- table_info %>% filter(name == column_name)
  type <- table_info %>% pull(type)
  unique_values_available <- table_info %>% pull(unique_values_available)
  
  values <- filter_list
  
  # Range/between
  
  if(type %in% c("DATE", "INTEGER", "REAL") & !unique_values_available){
    
    quote_sign <- ifelse(type == "DATE", "'", "")
    
    query <- glue("`{column_name}` BETWEEN {quote_sign}{filter_list[1]}{quote_sign} AND {quote_sign}{filter_list[2]}{quote_sign}")
    
    return(query)
    
  }
  
  # In clause
  
  if(type %in% c("DATE", "INTEGER", "REAL", "TEXT")){
    
    # Detect if it is a free text field 
    
    if(type == "TEXT" & !unique_values_available){
      
      values <- str_split(values, ",") %>% unlist() %>% {glue("'{.}'")} %>% glue_collapse(", ")
      
    }
    
    # Convert to numeric to remove any quotes
    
    if(type %in% c("REAL", "INTEGER")){
      
      values <- values %>% as.numeric() %>% glue_collapse("; ")
      
    }
    
    if(type %in% c("TEXT")){
      
      values <- glue("'{values}'") %>% glue_collapse(", ")
      
    }
    
    query <- glue("`{column_name}` IN ({values})")
    
    return(query)
    
    
  }
  
  
}

  