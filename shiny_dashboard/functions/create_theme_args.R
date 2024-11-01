create_theme_args <- function(theme_args = c("plot.title", "plot.caption", "plot.caption", "axis.text.x", "axis.text.y"), plot_input){
  
  theme_args <- map(theme_args, ~call2("element_text", !!!plot_input[[.x]]) |> deparse1()) |> setNames(theme_args)
  plot_input[["theme_args"]] <- theme_args 
  
  return(plot_input)
  
  # plot_input[["theme"]] <- call2("theme", !!!theme_args)
  
}

