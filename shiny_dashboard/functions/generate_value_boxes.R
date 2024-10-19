generate_value_boxes <- function(rag_score, average_missing, column_name, hide_element = FALSE){
  
  id_hide <- glue("{column_name}_value_box")
  
  
  if(hide_element){
  
  card <- 
    
    value_boxes <- tagList(
      hidden(
      div(id = id_hide,
          layout_columns(col_height = c(6, 6), 
                         card_body(
        value_box(glue("RAG Score {rag_score}"), value = NULL, theme = rag_score)
      ),
      card(
        card_body(glue("Average missing: {round(average_missing * 100, 2)}%"), value = NULL, theme = rag_score)
      )
      )
      )
      )
      )
  
  return(card)
  
  }
  
  card <- 
    
    value_boxes <- tagList(
        div(id = id_hide,
            layout_columns(col_height = c(6, 6), 
                           card_body(
                             value_box(glue("RAG Score {rag_score}"), value = NULL, theme = rag_score)
                           ),
                           card_body(
                             value_box(glue("Average missing: {round(average_missing * 100, 2)}%"), value = NULL, theme = rag_score)
                           )
            )
        )
      )
    
  
  return(card)
}