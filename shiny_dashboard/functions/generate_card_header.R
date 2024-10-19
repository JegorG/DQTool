generate_card_header <- function(choices, selected, id){
    card_header <-       card_header(
      "Missing value plot",
      popover(
        bsicons::bs_icon("gear"),
        pickerInput(
          NS(id, "choose_column_to_view"), "View column", inline = TRUE,
          choices = choices,
          selected = selected
        ),
        materialSwitch(
          NS(id, "order_by_rag_score"), "Order By RAG score", inline = TRUE,
        ),
        materialSwitch(
          NS(id, "only_missing"), "Only missing", inline = TRUE,
        ),
        title = "Select and Sort"
      ),
      class = "d-flex justify-content-between align-items-center"
    )
  
  
  
}
