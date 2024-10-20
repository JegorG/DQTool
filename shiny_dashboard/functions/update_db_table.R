# Function to update db/table between modules 

update_db_table <- function(shared_inputs, session, called_from = ""){

  updateSelectInput(
    session,
    'database_input',
    selected = shared_inputs$database_input()
  )
  
  print(shared_inputs$table_input())
  
  updateSelectInput(
    session,
    'table_input',
    selected = shared_inputs$table_input()
  )
  
}