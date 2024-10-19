# A function that returns n random rows from a SQL table

select_sample_from_sql <- function(database, table, n_rows){
  
  sample <- 
    dbSendQuery(get(database), glue("SELECT * FROM {table}  ORDER BY RANDOM() LIMIT {n_rows}")) %>%
    dbFetch()
  return(sample)
  
}

select_sample_from_sql_safe <- safely(select_sample_from_sql)

