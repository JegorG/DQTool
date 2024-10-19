# Function that takes a character of a database and returns the tables in that database

get_database_tables <- function(selected_db){
  
  table_choices <- dbListTables(get(selected_db))
  return(table_choices)
  
}

get_database_tables_safe <- safely(get_database_tables)

