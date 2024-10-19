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


# plan(multisession) 

Alfam <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
Retail <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)
tool_db <- dbConnect(RSQLite::SQLite(), "", extended_types = TRUE)

df <- fread("C:/Users/DELL/Documents/shiny_work/df.csv", nrows = 1000)
dbWriteTable(Alfam, "mtcars1", mtcars)
dbWriteTable(Alfam, "iris1", iris)
dbWriteTable(Alfam, "df", df)

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


# Tables active


# tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()
#reactive_values <- reactiveValues()
#reactive_values$tables_available <- dbSendQuery(tool_db, "SELECT * FROM log_table WHERE active = 1") %>% dbFetch()

setwd('C:/Users/DELL/Documents/shiny_work')
map(list.files("C:/Users/DELL/Documents/shiny_dashboard/modules", full.names = TRUE), source)
map(list.files("C:/Users/DELL/Documents/shiny_dashboard/functions", full.names = TRUE), source)


app <- function(){

ui <- fluidPage(
  navbarPage(
    'Modelling Data Quality tool',
    

# Data Section ------------------------------------------------------------

    
    navbarMenu(
      'Data section',
      

# * Upload Data ------------------------------------------------------------

      
     tabPanel('Upload Data',upload_data_ui("upload_data")),


#  * Convert Data Columns -------------------------------------------------


      
      tabPanel('View data', view_data_ui("view_data")),
      tabPanel('Delete Data'),
      tabPanel('Download Data'),
      
      '----',
      tabPanel('Help')
    ),
    
               
               
               
               
     # source('completeness_ui.R')$value,
      navbarMenu("Completeness", 
               tabPanel("Missing value analysis", missing_value_analysis_ui("mva")),
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
  
  upload_data_server("upload_data")
  view_data_server("view_data")
  missing_value_analysis_server("mva")
  
  session$onSessionEnded(function() {
    
    # dbDisconnect(Alfam)
    # dbDisconnect(Retail)
    # dbDisconnect(tool_db)
  })
  
}

shinyApp(ui, server)

}

app()
