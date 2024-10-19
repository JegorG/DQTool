generate_select_query <- function(columns_in, select_type, missing_values = c()){
  
  if(select_type == "missing_values"){
  
    missing_in_statement <- c("('')")
  
  if(NROW(missing_values) > 0){
    
    missing_in_statement <- glue("'{missing_values}'") %>% 
      glue_collapse(",") %>% 
      {glue("('',{.})")}
    
  }
  
 select_clause <-  glue("SUM(`{columns_in}` IS NULL OR TRIM(`{columns_in}`) IN {missing_in_statement}) AS `{columns_in}_n_missing`") %>% 
    glue_collapse(", ") %>% 
    glue(", COUNT(*) AS n_rows")
 
 return(select_clause)
 
  }
  
}
