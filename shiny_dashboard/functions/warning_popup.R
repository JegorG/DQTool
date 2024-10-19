warning_popup <- function(title, text, ok_text, id){
  
  shinyalert(
    title = title,
    text = text,
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = ok_text,
    confirmButtonCol = "#AEDEF4",
    cancelButtonText = "Cancel",
    timer = 0,
    imageUrl = "",
    animation = TRUE,
    inputId = id
  )
  
  
}