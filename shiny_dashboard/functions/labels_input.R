
labels_input <- function(what_lab, id = id){
  ids <- c("text", "extra", "face", "family", "size", "align", "color", "angle", "hjust", "vjust")
  id_list <- map(ids, ~glue("{what_lab}_{.x}")) |> set_names(ids)
  
  
  ui <- 
  
  layout_columns(col_widths = c(10, 2),
                 class = "no_gap",
  textInput(NS(id, id_list$text), NULL, placeholder = what_lab |> str_to_title()),
  dropMenu(actionButton(NS(id, id_list$extra), NULL, icon =  icon("plus"), style = "height: 36.02px; text-align: center;"),
           
           radioGroupButtons(NS(id, id_list$face), "Font Face:", choices = c("Plain", "Italic", "Bold", "Bold & Italic")),
           pickerInput(NS(id, id_list$family), "Font:", choices = LETTERS[1:5]),
           numericInputIcon(NS(id, id_list$size), NULL, 12, 4, 32, icon = "Size:"),
           radioGroupButtons(NS(id, id_list$align), "Align:", c("Left", "Center", "Right")),
           colorPickr(
             inputId = NS(id, id_list$color),
             label = "Color:",
             swatches = scales::viridis_pal()(10),
             opacity = TRUE
           ),
  numericInputIcon(NS(id, id_list$angle), NULL, 0, 0, 360, icon = "Angle:"),
  numericInputIcon(NS(id, id_list$vjust), NULL, 0, 0, 1, 0.05, icon = "Vertical Adjustment,"),
  numericInputIcon(NS(id, id_list$hjust), NULL, 0, 0, 1, 0.05, icon = "Horizontal Adjustment:")
  )
  )
  
  return(ui)
  
}




