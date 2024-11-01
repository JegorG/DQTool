create_labs_args <- function(labels, plot_input){
  
  
  plot_input[["labs"]] <- map(labels, ~plot_input[[glue("{.x}_text")]]) |> setNames(labels)
  
}