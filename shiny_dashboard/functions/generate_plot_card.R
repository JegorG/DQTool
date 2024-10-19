generate_plot_card <- function(column_name, hide_element = TRUE, id){
  
  id_hide <- glue("{column_name}_plot") 
  
  card <- 
     div(id = id_hide,
          card(
            card_body(plotOutput(NS(id, column_name))), 
            full_screen = TRUE
          )
      )
  
  # Start the element hidden, first one should usually not be hidden
  if(hide_element) card <- hidden(card)
      
  return(card)
  
}