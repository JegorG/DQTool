# plot_names <- names(plot_data_list)
# 
# all_plots <- map2(plot_data_list, plot_names, plot_missing_val, 
#                   x = "date_col_1",
#                   y = "pct_missing",
#                   database = "Alfam",
#                   table    = "df") 
# 
# map(plot_names,  ~all_plots[.x])
# 
# # Save all plots to the ouput
# 
# values_boxes_list <- pmap(list(missing_over_view$rag_score, missing_over_view$mean_pct_missing, missing_over_view$column))
# plot_cards_list   <- 
# 
# 
# 
# 
# 
# generate_mva_ui <- function(rag_score, average_missing, selected, choices, id){
#   
#   card_header <-       card_header(
#     "Missing value plot",
#     popover(
#       bsicons::bs_icon("gear"),
#       pickerInput(
#         NS(id, "choose_column_to_view"), "View column", inline = TRUE,
#         choices = choices,
#         selected = selected
#       ),
#       materialSwitch(
#         NS(id, "order_by_rag_score"), "Order By RAG score", inline = TRUE,
#       ),
#       # materialSwitch(
#       #   NS(id, "only_missing"), "Only missing", inline = TRUE,
#       # ),
#       title = "Select and Sort"
#     ),
#     class = "d-flex justify-content-between align-items-center"
#   )
#   
#   
#   ### value box card (Thoose that show rag score)
#   
#   ui <- 
#   
#   layout_columns(
#     col_widths = c(6, 6, 12, 12),
#     row_heights = c(1, 6, 3),
#     card(
#       value_box("RAG Score", value = rag_score, theme = rag_score), height = "100px"
#     ),
#     card(
#       value_box("Average missing", glue("{round(average_missing * 100, 2)}%"), theme = rag_score), height = "100px"
#     ),
#     
#     card(
#       card_body(plotOutput(NS(id, "missing_value_plot"))), 
#       full_screen = TRUE
#     ),
#     card(
#       DT::dataTableOutput(NS(id, "table")), full_screen = TRUE
#     )
#     
#   )
#   
#   return(ui)
#   
# }
# 
