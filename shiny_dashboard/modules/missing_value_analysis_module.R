missing_value_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(sidebarPanel(
      bsCollapsePanel(
      "Data",
      
      actionBttn(ns("refresh_databases"), "Refresh Databases", size = "xs"),
      pickerInput(ns("database_input"), "Database", choices = c("Alfam", "Retail"), selected = NULL, multiple = TRUE, options = pickerOptions(maxOptions = 1)),
      
      uiOutput(ns("table_picker"))
      
      
      ),
      bsCollapsePanel(
        "Columns",
        radioButtons("controller2", "Controller", choices = c(1, 2))
      ),
      
      bsCollapsePanel(
        "Filter",
        radioButtons("controller2", "Controller", choices = c(1, 2))
      ),
      
      bsCollapsePanel(
        "Missing value settings",
        radioButtons("controller2", "Controller", choices = c(1, 2))
      ),
      
      bsCollapsePanel(
        "Plot settings",
        radioButtons("controller2", "Controller", choices = c(1, 2))
      ),
      
      bsCollapsePanel(
        "Transform columns",
        radioButtons("controller2", "Controller", choices = c(1, 2))
      ),
      layout_columns(
      radioGroupButtons(
        inputId = ns("mode_input"),
        label = "Dataset",
        choices = c("Sample", 
                    "Full"),
        justified = TRUE
      ),
      
          tooltip(
            bsicons::bs_icon("question-circle", title = "Help"),
            "Mass measured in grams."
           # placement = "right"
          ), col_widths = c(12, 1))
      
    ,width = 3),
    
  mainPanel()
    ))
}

missing_value_analysis_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      

# Generate Table Input ---------------------------------------------------

      
      
      observeEvent(input$database_input, {
        
        print(input$database_input)
        output$table_picker <- NULL
        
        req(input$database_input)
        print(input$database_input)

        picker <- pickerInput(NS(id = id, "test"), choices = letters)
        output$table_picker <- renderUI({picker})
        
        
        
      }, ignoreNULL = FALSE)
      

# Refresh Databases -------------------------------------------------------

      
      observeEvent(input$refresh_databases, {
        
        databases_available <- send_query_safe("tool_db", "SELECT DISTINCT(database) AS database FROM log_table WHERE active")
        check_if_error_and_message(databases_available$error, "send_query")
        
        req(databases_available$result)
        
        updatePickerInput(session = session,
                          inputId = "database_input",
                          choices = databases_available$result %>% pull(database) %>% c(., "tool_db") %>%  sort())
        
        
      })
      
      
      
      
    }
  )
}