create_guides_args <- function(args_list, guide, type, plot_input){
  
  args_list <- args_list |> 
    compact() |> 
    keep(~.x != "")
  
guide_args <-   
  call2("element_text", !!!args_list) |> 
    list(legend.title = _) |>
    (\(x) call2("theme", !!!x))() |> 
    list(theme = _) |> 
    (\(x) call2(guide, !!!x))() |> 
  deparse1()

plot_input$guides[[type]] <- guide_args


  
}
