send_query <- function(database, query){
  
  result <- dbSendQuery(get(database), query) %>% dbFetch()
  return(result)
  
}

send_query_safe <- safely(send_query)