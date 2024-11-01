  #rm(ui, server)

  library(shiny)
  library(tidyverse)
  library(shinyWidgets)
  library(DBI)
  library(RSQLite)
  library(shinyalert)
  library(glue)
  library(data.table)
  library(collapse)
  library(knitr)
  library(kableExtra)
  library(bslib)
  library(sqlparseR)
  library(shinydashboardPlus)
  library(shinyBS)
  library(bsicons)
  library(scales)
  library(showtext)
  library(keys)
  library(DT)
  library(shinyjs)
  library(thematic)
  library(tictoc)
  library(esquisse)
  library(rlang)
  
  # font_add_google("Chilanka", "chilanka")
  # showtext_auto()
  # 
  # standard_theme <- theme(panel.background = element_rect(fill = "#f8ecd6",
  #                                                         color = "#f8ecd6"),
  #                         panel.border = element_rect(colour = "black", fill = NA),
  #                         axis.text =  element_text(size = 16),
  #                         strip.text.x = element_text(size = 18),
  #                         strip.background = element_rect(fill = "#f8ecd6"),
  #                         text=element_text(size=16,  family = "chilanka"))

  # plan(multisession)

  Alfam <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
  Retail <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
  tool_db <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)

  df <- fread("C:/Users/DELL/Documents/shiny_work/df.csv", nrows = 1000)
  dbWriteTable(Alfam, "mtcars1", mtcars)


  dbWriteTable(Alfam, "iris1", iris)
  dbWriteTable(Alfam, "df", df)
  rm(df)
  gc()

  dbWriteTable(Retail, "mtcars2", mtcars)
  dbWriteTable(Retail, "iris2", iris)

  dbExecute(tool_db, "DROP TABLE IF EXISTS log_table;")

  dbExecute(tool_db, "CREATE TABLE log_table (
  	database TEXT NOT NULL,
    'table_name'    TEXT NOT NULL,
  	upload_date datetime,
  	deleted_date datetime,
  	active INT
  )")
# 
#   # tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()
#   #reactive_values <- reactiveValues()a
#   #reactive_values$tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()
# 
# 
# # Load Functions ----------------------------------------------------------
#   
  setwd('C:/Users/DELL/Documents/shiny_work')
  map(list.files("C:/Users/DELL/Documents/DQTool/shiny_dashboard/modules", full.names = TRUE), source)
  map(list.files("C:/Users/DELL/Documents/DQTool/shiny_dashboard/functions", full.names = TRUE), source)

# Global objects ----------------------------------------------------------

databases <- c("Alfam", "Retail", "Non-Retail", "Sandbox", "tool_db")

start_theme <- bs_theme()
  
  # bs_add_rules(base_font    = "Nosifer",
  #              code_font    = "Nosifer",
  #              heading_font = "Nosifer") #|> 
  # # accordion
  # bs_add_rules(".accordion-title {
  #    font-family: 'Nosifer', sans-serif;
  #    color: red;
  #    }") |> 
  # bs_add_rules(".irs--shiny, .irs-min, .irs--shiny, .irs-max, .irs-single, .irs-to, .irs-from, .irs-grid-text, .js-irs-0 {
  #     font-family: 'Nosifer', sans-serif;
  #     color: red !important;
  #   }")
  
  app <- function(){
  
  ui <- page_fillable(
    theme = start_theme,
    tags$style(HTML("
       @import url('https://fonts.googleapis.com/css2?family=Henny+Penny&family=Nosifer&display=swap');
       @import url('https://fonts.googleapis.com/css2?family=Nosifer&display=swap');
       .no_gap_boxes {gap: 0rem;}
       .no_gap_boxes > div > div:has(button) {display: flex; flex-grow: 1;}
       button {width: 100%;}
       .sw-input-icon {width: 75px;}
       .no_gap {--bslib-spacer: 0rem;}
       input[value = 'Bold'] + label {font-weight: bold;}
       input[value = 'Italic'] + label {font-style: italic;}
       input[value = 'Bold & Italic'] + label {font-weight: bold; font-style: italic;}
       "
    )),
    page_navbar(
      title = "Data Quality pro",
      collapsible = TRUE,
     # inverse = !IS_LEGACY,
      id = "navbar",
      fillable = "Dashboard",
      
  
  # Data Section ------------------------------------------------------------
  
      
      nav_menu(
        'Data section',
        
  
  # * Upload Data ------------------------------------------------------------
  
        
       nav_panel('Upload Data', upload_data_ui("upload_data", databases_input = databases)),
  
  
  #  * Convert Data Columns -------------------------------------------------
  
  
        
        tabPanel('View data', view_data_ui("view_data", databases)),
        tabPanel('Delete Data'),
        tabPanel('Download Data'),
        
        '----',
        tabPanel('Help')
      ),
      
                 
       # source('completeness_ui.R')$value,
        navbarMenu("Completeness", 
                 tabPanel("Missing value analysis", missing_value_analysis_bs_ui("mva", databases)),
                 tabPanel("Help")),
        source('accuracy_ui.R')$value,
        source('consistency_ui.R')$value,
        source('timeliness_ui.R')$value,
        source('uniqueness_ui.R')$value,
        source('validity_ui.R')$value,
  
  
  nav_menu("Settings",
           nav_panel("Data Options",
                     bslib::accordion(accordion_panel("Set Data Globally",
                                                      
                                                      pickerInput("set_database",
                                                                  choices = databases,
                                                                  multiple = TRUE,
                                                                  options = pickerOptions(maxOptions = 1, title = "Database")),
                                                      
                                                      conditionalPanel("input.set_database.length > 0",{
                                                      
                                                        pickerInput("set_table",
                                                                    choices = NULL,
                                                                    multiple = TRUE,
                                                                    options = pickerOptions(maxOptions = 1, `live-search`=TRUE, title = "Table"))
                                                        
                                                      },
                                                      
                                                      conditionalPanel("input.set_table.length > 0", {
                                                        
                                                       input_task_button("set_button", "Set")
                                                        
                                                        
                                                      })
                                                                       
                                                      )))),
           nav_panel("Theme",
                     bslib::accordion(accordion_panel("Theme Settings"))))
  
  
        
        # tabPanel('Availability'),
        # tabPanel('Traceacbility'),
        # tabPanel('Representativiine ss')
          
          
          
          
  ))
  
  
  server <- function(session, input, output){
    
    reactive_values <- reactiveValues()
    
    upload_data_server("upload_data")
    view_data_server("view_data")
    missing_value_analysis_bs_server("mva")
    
    # session$onSessionEnded(function() {
    #   
    #   # dbDisconnect(Alfam)
    #   # dbDisconnect(Retail)
    #   # dbDisconnect(tool_db)
    # })
    
    observeEvent(input$set_database, {
      
      req(input$set_database)
      
      new_choices <- send_query("tool_db", glue("SELECT table_name FROM log_table WHERE active = 1 AND database = '{input$set_database}'")) %>% pull(table_name)
      
      updatePickerInput(session, "set_table", choices = new_choices)
      
      reactive_values$tables_available <- new_choices
      
    })
    
    observeEvent(input$set_button, {
      
      req(input$set_table)
      
      view_data_server("view_data", databases = databases, 
                       selected_database = input$set_database,
                       tables = reactive_values$tables_available, 
                       selected_table = input$set_table)
      
      missing_value_analysis_bs_server("mva", databases = databases, 
                       selected_database = input$set_database,
                       tables = reactive_values$tables_available, 
                       selected_table = input$set_table)
      
      
    })
    
    output$test_plot <- renderPlot({
      
      ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        facet_wrap(vars(Species))
      
      
    })
    
  }
  
  shinyApp(ui, server)
  
  }
  
  app()

  