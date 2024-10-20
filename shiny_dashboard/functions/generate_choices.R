# Generate choices from table_info and return a named list to use in pickerInput

generate_choices <- function(table_info){
  
  choices <- table_info %>% 
    group_split(type) %>% 
    set_names(map_chr(., ~.x$type[1])) %>% 
    map(~pull(., name %>% sort())) %>% 
    map(as.list)
  
  return(choices)
  
}


