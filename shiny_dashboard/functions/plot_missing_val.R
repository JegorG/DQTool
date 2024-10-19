plot_missing_val <- function(data, 
                             column, 
                             x, 
                             y = "n_missing", 
                             database, 
                             table, 
                             color_by, 
                             facet_row, 
                             facet_column){
  
  plt <- ggplot(data, aes_string(x = x, y = y, color = color_by)) +
    geom_line() +
    labs(title = column,
         caption = glue("Dataset:{database}.{table}"),
         y = "Percentage missing") +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
    facet_grid(rows = vars(facet_row), cols = vars(facet_column))
  
  return(plt)
  
}




