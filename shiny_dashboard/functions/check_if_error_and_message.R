check_if_error_and_message <- function(error = NULL, function_name = c()){
  
  if(!is.null(error)){
  
  shinyalert(
    title = "Ohhh noooo",
    text = glue('Error in {function_name}:{error}'),
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "error",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  }
  
}
