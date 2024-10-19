extra_plot_settings <- function(){

modalDialog(
  
  ### Labs
  
  h4("Labs"),
  
  layout_columns(
    col_widths = c(6, 6),
  card(
  textInput("plot_title", "Title", placeholder = 'Title'),
  textInput("plot_subtitle", "Subtitle", placeholder = 'Title'),
  textInput("plot_caption", "Caption", placeholder = 'Title'),
  
  textInput("x_lab", "X", placeholder = 'Title'),
  textInput("y_lab", "Y", placeholder = 'Title')
  ),
  card(
    materialSwitch("plot_coord_flip", label = "Flip X & Y", value = FALSE, status = "primary"),
    materialSwitch("plot_add_logo", label = "Add logo", value = FALSE, status = "primary"),
    materialSwitch("plot_mean_line", label = "Add mean line", value = FALSE, status = "primary"),
    materialSwitch("plot_trend_line", label = "Add trend line", value = FALSE, status = "primary"),
    materialSwitch("plot_value_boxes", label = "Hide value boxes", value = FALSE, status = "primary"),
    
    radioGroupButtons("plot_rotate_x", "Rotate X label", choices = c("0", "45", "90", "180"), justified = TRUE,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    ),
    
    radioGroupButtons("plot_rotate_y", "Rotate Y label", choices = c("0", "45", "90", "180"), justified = TRUE,
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"))
    )

  )
  ),
  
  
  
  footer = tagList(
    modalButton("Cancel"),
    actionButton("ok", "OK")
  ),
  size = "l"
)
}
