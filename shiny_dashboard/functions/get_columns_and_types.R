# Funcion to get columns and types from a SQL table

get_columns_and_types <- function(database, table){
  
   results <- 
     dbSendQuery(get(database), glue("PRAGMA table_info({table});")) %>% 
     dbFetch()
   
   return(results)
  
}

get_columns_and_types_safe <- safely(get_columns_and_types)

