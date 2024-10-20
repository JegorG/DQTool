  view_data_ui <- function(id, databases_input) {
    ns <- NS(id)
    page_fillable(
    tagList(page_sidebar(
      sidebar = sidebar(
        
  
  # refresh databases -------------------------------------------------------
  
  
  input_task_button(ns("refresh_databases"), "Refresh Databases",
                        icon = bs_icon("arrow-repeat"),
                        label_busy = "Refreshing",
                        icon_busy = icon("sync", class = "fa-spin")),
        
        
        # Select database --------------------------------------------
        
        pickerInput(
          inputId = ns("database"),
          label = "Database",
          choices = databases_input,
          options = list(title = "Select Database"),
          selected = "Alfam"
        ),
        
  
  # Select Table (conditionalPanel) -----------------------------------------
  
        
        conditionalPanel(
          "input.database.length > 0",
  
          pickerInput(
            inputId = ns("table"),
            label = "Table",
            choices = c(),
            options = list(title = "Select Table")
          ),
          ns = NS(id)
        ),
  
  
  # Select Columns and Filters (ConditionalPanel) ---------------------------------------
  
  
  conditionalPanel(
    "input.table.length > 0",

    pickerInput(
      inputId = ns("columns"),
      label = "Select Columns",
      options = list(title = "Select Columns"),
      choices = c(),
      multiple = TRUE
    ),

    pickerInput(
      inputId = ns("filter_columns"),
      label = "Filter Columns",
      choices = c(),
      options = list(title = "Filter Columns"),
      multiple = TRUE
    ),

    ns = NS(id)
  ),
  

# Filters -----------------------------------------------------------------

   uiOutput(NS(id, "filters")),
  

# View data ---------------------------------------------------------------

conditionalPanel(
  "input.columns.length > 0",
  actionBttn(inputId = ns("view_data_btn"),
             label = "View Data"),
  ns = NS(id)
),
      ),

# mainPanel ---------------------------------------------------------------

        
navset_card_underline(
  
  nav_panel("Data", DTOutput(NS(id, "view_table"))),
  
  nav_panel("SQL Query", htmlOutput(NS(id, "sql_query")),
  
  
))


)))
    
  }
  
  view_data_server <- function(id, databases = NULL, selected_database = NULL, tables = NULL, selected_table = NULL) {
    moduleServer(id, function(input, output, session) {
      
      table_selected <- reactiveVal(NULL)
      
      observe({
        
        req(databases, selected_database, tables, selected_table)
        
        updatePickerInput(session, "database", choices = databases, selected = selected_database)
        updatePickerInput(session, "table", choices = tables, selected = selected_table)
        table_selected(selected_table)
      })
        
      reactive_values <- reactiveValues()
      
  # observeEvent Database to update tables ----------------------------------
  
      
        observeEvent(input$database, priority = 1, {
          
          req(input$database)

            new_choices <- 
            send_query("tool_db", glue("SELECT table_name FROM log_table WHERE active = 1 AND database = '{input$database}'")) %>% 
            pull(table_name)
          
          updatePickerInput(session = session,
                            inputId = "table",
                            choices = new_choices,
                            selected = table_selected())
          
          if(!is.null(table_selected())) table_selected(NULL)

          
        
      })
      
  # observeEvent: Table to update columns -----------------------------------
  
      observeEvent(input$table, {
        
        req(input$table)
        
        database <- input$database
        table    <- input$table
        
        table_info <- send_query_safe("tool_db", glue("SELECT * FROM {database}_{table}_table_info"))
        check_if_error_and_message(table_info$error, "send_query")
        
        req(NROW(table_info) > 0)
        
        table_info <- table_info$result
        new_choices <- table_info %>% generate_choices()
        # Select columns picker
        
        updatePickerInput(session = session,
                          inputId = "columns",
                          choices = new_choices,
                          options = list(`live-search` = TRUE,
                                         `actions-box` = TRUE))
        
        # Filter columns picker
        
        updatePickerInput(session = session,
                          inputId = "filter_columns",
                          choices = new_choices,
                          options = list(`live-search` = TRUE,
                                         `actions-box` = TRUE))
        
        # Generate all the filter panels 

        unique_value_table <- send_query_safe("tool_db", glue("SELECT * FROM {database}_{table}_unique_val"))
        check_if_error_and_message(unique_value_table$error, "send_query_safe")
        
        req(NROW(unique_value_table$result) > 0)
        
        splitted_info <-
          table_info %>%
          left_join(unique_value_table$result) %>%
          group_split(name)
        
        
        new_filter_inputs <- map(splitted_info, generate_filters, id = id)
        
        output$filters <- renderUI(tagList(
          
          new_filter_inputs
          
        ))
        
        # Update the reactive values
        
        reactive_values$table_info <- table_info
        reactive_values$unique_values_table <- unique_value_table$result
        
        
      })
      
  
  # observeEvent refresh databases ------------------------------------------
  
      
      observeEvent(input$refresh_databases, {
        
        databases_available <- send_query_safe("tool_db", "SELECT DISTINCT(database) AS database FROM log_table WHERE active")
        check_if_error_and_message(databases_available$error, "send_query")
        
        req(databases_available$result)
        
        updatePickerInput(session = session,
                          inputId = "database",
                          choices = databases_available$result %>% pull(database) %>% c(., "tool_db") %>%  sort())
        
        
      })
      

# observeEvent: View Data ---------------------------------------------

      observeEvent(input$view_data_btn, {
        
        filter_columns <- input$filter_columns
        
        filter_list <- map(filter_columns, ~input[[glue("{.x}_filter_input")]], input)
        table_info <- reactive_values$table_info %>% filter(name %in% filter_columns)
        
        
        sql_query <- generate_sql_query(filter_columns, filter_list, table_info, input$columns, input$table)

        result <- send_query_safe(input$database, sql_query$query)
        check_if_error_and_message(result$error, "send_query_safe")
        
        output$view_table <- DT::renderDT(result$result)
        
        
        sql_query <- 
          sql_query$query %>% 
          sql_format(reindent = TRUE) %>% 
          str_replace_all(fixed("\n"), "<br>") %>% 
          str_replace("SELECT", "SELECT<br>") %>% 
          str_replace("FROM", "FROM<br>") %>% 
          str_replace("WHERE", "WHERE<br>")
        
        output$sql_query <- renderUI({HTML(sql_query)})
        
        print("Done")
        
        
      }, ignoreNULL = TRUE)
  
      
      
  ################    
    })}
