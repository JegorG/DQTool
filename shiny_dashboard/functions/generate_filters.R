

generate_filters <- function(data_frame, id){
  
  picker <- NULL
  
  # Remove these 3 comments because it did not work when a column is 100% missing
  
  #  unique_values_available <- FALSE
  # unique_values <- data_frame %>% pull(value)
  # if(any(!is.na(unique_values))) {unique_values_available <- TRUE}
  
  # Replaced with
  
  unique_values_available <- data_frame %>% slice(1) %>% pull(unique_values_available)
  unique_values <- data_frame %>% pull(value)
  
  # Now the unique values are selected and identified
  # For the column that has unique values all information will be duplicated to just select from the first row
  
  
  name <- data_frame %>% slice(1) %>% pull(name)
  type <- data_frame %>% slice(1) %>% pull(type)
  
  minimum_value <- data_frame %>% slice(1) %>% pull(minimum_value)
  maximum_value <- data_frame %>% slice(1) %>% pull(maximum_value)
  
# Date --------------------------------------------------------------------

 # Unique values not available 
  
  if(type == "DATE" & !unique_values_available){

    picker <-   airDatepickerInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      range = TRUE,
      value = c(ymd(minimum_value), ymd(maximum_value))
    )
  }
  
  # Unique values available
  
  if(type == "DATE" & unique_values_available){
    
    picker <-   pickerInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      choices  = unique_values
    )
  }
  

# Integer-----------------------------------------------------------------

 # Unique values not available
  
  if(type == "INTEGER" & !unique_values_available){
    
    picker <-   sliderInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      min = minimum_value %>% as.numeric(),
      max = maximum_value %>% as.numeric(),
      value = c(minimum_value, maximum_value) %>% as.numeric(),
      dragRange = TRUE,
    )
  }
  
 # Unique values available
  
  if(type == "INTEGER" & unique_values_available){
    
    picker <-   pickerInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      choices = unique_values,
      multiple = TRUE
    )
  }
  

# Real/Numeric --------------------------------------------------------------------

  # Unique values not available 
  
  if(type == "REAL" & !unique_values_available){
    
    picker <-   sliderInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      min = minimum_value %>% as.numeric(),
      max = maximum_value %>% as.numeric(),
      value = c(minimum_value, maximum_value) %>% as.numeric()
    )
  }
  
  # Unique values available
  
  if(type == "REAL" & unique_values_available){
    
    picker <-   pickerInput(
      inputId = NS(id, glue("{name}_filter_input")),
      label = glue("{name}"),
      multiple = TRUE,
      choices = unique_values
    )
  }

# Text --------------------------------------------------------------------
  
  # Unique values not available

  if(type == "TEXT" & !unique_values_available){
    
    picker <- 
    textInput(NS(id, glue("{name}_filter_input")),
              label = glue("{name}"),
              placeholder = "Seperate values with ;")
    
  }
  
  # Unique values available
  
  if(type == "TEXT" & unique_values_available){
    
    
    picker <- 
    pickerInput(NS(id, glue("{name}_filter_input")),
              label = glue("{name}"),
              choices = unique_values,
              multiple = TRUE)
    
  }
  

# Generate ConditionalPanel --------------------------------------------------------

  
  
  panel_and_pickers <- 
  
  conditionalPanel(glue("input.filter_columns.includes('{name}')"),
                   
                   picker , ns = NS(id))
  
  return(panel_and_pickers)
    
    
}
  


