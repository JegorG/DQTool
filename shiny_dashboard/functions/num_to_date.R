num_to_date <- function(numeric_date){
  
  char_date <- 
    numeric_date %>% 
    as.numeric() %>% 
    as.Date() %>% 
    as.character()
  
  return(char_date)
  
}
