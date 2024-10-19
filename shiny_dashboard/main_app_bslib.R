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

  # Alfam <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
  # Retail <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
  # tool_db <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
  # 
  # df <- fread("C:/Users/DELL/Documents/shiny_work/df.csv")
  # dbWriteTable(Alfam, "mtcars1", mtcars)
  # 
  # 
  # dbWriteTable(Alfam, "iris1", iris)
  # dbWriteTable(Alfam, "df", df)
  # rm(df)
  # gc()
  # 
  # dbWriteTable(Retail, "mtcars2", mtcars)
  # dbWriteTable(Retail, "iris2", iris)
  # 
  # dbExecute(tool_db, "DROP TABLE IF EXISTS log_table;")
  # 
  # dbExecute(tool_db, "CREATE TABLE log_table (
  # 	database TEXT NOT NULL,
  #   'table_name'    TEXT NOT NULL,
  # 	upload_date datetime,
  # 	deleted_date datetime,
  # 	active INT
  # )")

  # tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()
  #reactive_values <- reactiveValues()a
  #reactive_values$tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()


# Load Functions ----------------------------------------------------------
  
  setwd('C:/Users/DELL/Documents/shiny_work')
  map(list.files("C:/Users/DELL/Documents/DQTool/shiny_dashboard/modules", full.names = TRUE), source)
  map(list.files("C:/Users/DELL/Documents/DQTool/shiny_dashboard/functions", full.names = TRUE), source)
  

# Global objects ----------------------------------------------------------

databases <- c("Alfam", "Retail", "Non-Retail", "Sandbox", "tool_db")
  
  app <- function(){
  
  ui <- page_fillable(
    page_navbar(
      theme = bs_theme(bg = "black", fg = "white"),
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
  
  
        
        tabPanel('View data', view_data_ui("view_data")),
        tabPanel('Delete Data'),
        tabPanel('Download Data'),
        
        '----',
        tabPanel('Help')
      ),
      
                 
                 
                 
                 
       # source('completeness_ui.R')$value,
        navbarMenu("Completeness", 
                 tabPanel("Missing value analysis", missing_value_analysis_bs_ui("mva")),
                 tabPanel("Help")),
        source('accuracy_ui.R')$value,
        source('consistency_ui.R')$value,
        source('timeliness_ui.R')$value,
        source('uniqueness_ui.R')$value,
        source('validity_ui.R')$value
        
        # tabPanel('Availability'),
        # tabPanel('Traceacbility'),
        # tabPanel('Representativiine ss')
          
          
          
          
  ))
  
  
  server <- function(session, input, output){
    thematic::thematic_shiny() 
    upload_data_server("upload_data")
    view_data_server("view_data")
    missing_value_analysis_bs_server("mva")
    
    # session$onSessionEnded(function() {
    #   
    #   # dbDisconnect(Alfam)
    #   # dbDisconnect(Retail)
    #   # dbDisconnect(tool_db)
    # })
    
  }
  
  shinyApp(ui, server)
  
  }
  
  run_with_themer(app())

  