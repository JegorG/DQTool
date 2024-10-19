

# UI ----------------------------------------------------------------------


upload_data_ui <- function(id, databases_input) {
  ns <- NS(id)
  
  tagList(
   page_sidebar(
    sidebar = sidebar(
      bslib::accordion(accordion_panel("Dataset", icon = bsicons::bs_icon("database-add"),
      # Select data type (SQL etc) --------------------------------------------
      
      
      pickerInput(
        inputId = ns("select_data_type"),
        label = "Select Data Type",
        choices = c("SQL", "CSV"),
        selected = "SQL",
        options = list(title = "Select Data Type")
      ),
      
      
      # ConditionalPanel for SQL ------------------------------------------------
      

# * Database --------------------------------------------------------------


      
      conditionalPanel(
        "input.select_data_type == 'SQL'",
        
        pickerInput(
          inputId = ns("sql_database_input"),
          label = "Select Database",
          choices = databases_input,
          selected = c(),
          options = list(title = "Database")
        ),
        ns = NS(id)
      ),
      

# * Table -----------------------------------------------------------------

      
      conditionalPanel(
        "input.sql_database_input.length > 0",
        
        pickerInput(
          inputId = ns("sql_table_input"),
          label = "Select Table",
          choices = c("Placeholder"),
          options = list(title = "Table")
        ),
        ns = NS(id)
      ),

# * SnapshotDate ----------------------------------------------------------
      
      conditionalPanel(
        "input.sql_table_input.length > 0",
        
        pickerInput(
          inputId = ns("sql_table_snapshotdate"),
          label = "Snapshot Date",
          choices = c("Placeholder"),
          options = list(title = "Snapshot Date"),
          multiple = TRUE,
        ),
        ns = NS(id)
      ),


# * Contract ID´s ---------------------------------------------------------

conditionalPanel(
  "input.sql_table_input.length > 0",
  
  pickerInput(
    inputId = ns("sql_table_id"),
    label = "Contract ID",
    choices = c("Placeholder"),
    options = list(title = "Contract ID", `live-search` = TRUE),
    multiple = TRUE,
  ),
  ns = NS(id)
),

      

# * Upload Button -----------------------------------------------------------

      conditionalPanel(
        "input.sql_table_input.length > 0",
        div(input_task_button(ns("upload_data_btn"), "Load Data", icon = icon("file-arrow-up"),
                                    label_busy = "Loading Data",
                                    icon_busy = icon("sync", class = "fa-spin"))),
        ns = NS(id)
      )
    ))),


# MainPanel ---------------------------------------------------------------


  tableOutput(NS(id, "table_info"))
  ))
  
}


# Server ------------------------------------------------------------------


# Server to update the Table selections according to the selected Database

upload_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # observeEvent: Database to update SQL tables -----------------------------
    
    
    observeEvent(input$sql_database_input, {
      req(input$sql_database_input)
      choices <- get_database_tables_safe(input$sql_database_input)
      
      check_if_error_and_message(choices$error)
      
      req(choices$result)
      new_choices <- choices$result
      updatePickerInput(session = session,
                        inputId = "sql_table_input",
                        choices = new_choices)
      
      
    })
    
    observeEvent(input$sql_database_input, {
      req(input$sql_database_input)
      choices <- get_database_tables_safe(input$sql_database_input)
      
      check_if_error_and_message(choices$error)
      
      req(choices$result)
      new_choices <- choices$result
      updatePickerInput(session = session,
                        inputId = "sql_table_input",
                        choices = new_choices)
      
      
    })
    
    # observeEvent: Table to update Snapshot Date and Contract ID -----------------------------
    
    observeEvent(input$sql_table_input, {
  
      req(input$sql_table_input)
      
      choices <- get_columns_and_types_safe(input$sql_database_input, input$sql_table_input)
      
      check_if_error_and_message(choices$error, "get_columns_and_types")
    
      req(choices$result)
      
      # Update SnapshotDate
      
      new_choices <- choices$result %>% filter(type == "DATE") %>% pull(name)
      updatePickerInput(session = session,
                        inputId = "sql_table_snapshotdate",
                        choices = new_choices)
      
      # Update Contract ID
      
      new_choices <- choices$result %>% pull(name)
      updatePickerInput(session = session,
                        inputId = "sql_table_id",
                        choices = new_choices)
      
      
    })
    

# observeEvent: Upload data Button ----------------------------------------

observeEvent(input$upload_data_btn, {
  
  req(input$sql_database_input)
  req(input$sql_table_input)
  
  sql_database <- input$sql_database_input
  sql_table    <- input$sql_table_input
  
  # Check if table is active

    is_active <- 
    dbSendQuery(tool_db, glue("SELECT * FROM log_table WHERE database = '{sql_database}' AND table_name = '{sql_table}' AND active = 1")) %>% 
    dbFetch()
  
  # If there is a active table with the same name, print a warning
  
  if(NROW(is_active) > 0) {
    warning_popup("Table already exists", "Do you want to reupload the table?", "Upload", id = "warning_upload_data")
    res <- NULL
  } else {
  res <- create_sample_mapping_log_safe(sql_database, sql_table, input$sql_table_id)
    check_if_error_and_message(res$error, "create_sample_mapping_log")
  }
      
  req(res$result)
  output$table_info <- renderTable(res$result)
 
  
})
    

# observeEvent ShinyAlert Upload -----------------------------------------------
# Warning message to hit if data already exists and user and choose if continue uploading
    observeEvent(input$warning_upload_data, {
      
      req(input$warning_upload_data)
      req(input$sql_database_input)
      req(input$sql_table_input)
      
      sql_database <- input$sql_database_input
      sql_table    <- input$sql_table_input
      
      res <- create_sample_mapping_log_safe(sql_database, sql_table, input$sql_table_id, overwrite = TRUE)
      check_if_error_and_message(res$error, "create_sample_mapping_log")
      
      req(res$result)
      output$table_info <- renderTable(res$result)
      
    })
    
    
    
    
    
    
  })
  
  
  
  
}
