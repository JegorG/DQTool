assign_element_text_input <- function(legend, theme, inputs = c("align", "angle", "color", "family", "face", "hjust", "vjust", "size"), plot_input, input){
  
  plot_input[[theme]] <- map2(inputs, legend, ~input[[glue("{.y}_{.x}")]]) |> setNames(inputs)
  
}