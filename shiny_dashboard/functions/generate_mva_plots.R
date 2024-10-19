generate_mva_plots <- function(database, 
                               table, 
                               columns,
                               filter_columns, 
                               filter_list, 
                               table_info,
                               group_by, 
                               facet_row = c(), 
                               facet_column = c(), 
                               color_by = c(),
                               sample_mode = FALSE, 
                               n_rows = c(),
                               plot_type = c(),
                               plot_function = "pct_missing",
                               id
                               ){
  
  # Generate the query that will fetch the data
  
  
  # Remove the selected columns that are in the group by, because you cant group by and select that value
  
  all_group_by <- c(group_by, facet_row, facet_column, color_by) %>% unique()
  input_columns <- columns[columns %in% all_group_by]
  
  ### Probably print a warning here ####
  
  sql_query <- generate_query_for_mva(database,
                                      table, 
                                      columns,
                                      filter_columns,
                                      filter_list,
                                      table_info,
                                      all_group_by,
                                      sample_mode = sample_mode,
                                      n_rows = n_rows
  )
  
  query_res <- send_query_safe("tool_db", sql_query$sql_query)
  check_if_error_and_message(query_res$error, "send_query")
  
  req(query_res$result)
  
  plot_data_list <- query_res$result %>% 
    pivot_longer(contains("_n_missing"), names_to = "column", values_to = "n_missing") %>% 
    mutate(pct_missing = n_missing/n_rows) %>% 
    group_split(column) %>% 
    set_names(map_chr(., ~.x$column[1] %>% str_remove("_n_missing"))) 
  
  plot_names <- names(plot_data_list)
  
  ggplot_statements <- map_chr(glue("plot_data_list${plot_names}"), 
                           generate_ggplot_statement,
                           input = list(
                             Type = plot_type,
                             x_var = group_by,
                             y_var = plot_function,
                             group = color_by,
                             facet_row = facet_row,
                             facet_col = facet_column 
                           ))
  
  all_plots <- map(ggplot_statements, ~eval(parse(text = .x))) %>% set_names(plot_names)
  
  missing_over_view <- 
    map2_df(plot_data_list, plot_names, ~tibble(column = .y, mean_pct_missing = mean(.x$pct_missing))) %>% 
    mutate(rag_score = case_when(mean_pct_missing > 0.15 ~ "red",
                                 mean_pct_missing %between% c(0.05, 0.15) ~ "orange",
                                 mean_pct_missing < 0.05 ~ "green"),
           row_index = row_number())
  
  ### Generate the UI
  
  hide_element <- c(FALSE, rep(TRUE, NROW(plot_names) - 1))
  
  values_boxes_list    <- pmap(list(missing_over_view$rag_score, missing_over_view$mean_pct_missing, missing_over_view$column, hide_element),generate_value_boxes)
  plot_cards_list      <- map2(plot_names, hide_element , generate_plot_card, id = id)
  card_header_settings <- generate_card_header(plot_names, plot_names[[1]], id = id)
  
  ### Update ###
  
  output_list <- list(plot_names              = plot_names, 
                      all_plots               = all_plots,
                      ui_value_boxes          = values_boxes_list,
                      ui_plot_cards_list      = plot_cards_list,
                      ui_card_header_settings = card_header_settings,
                      missing_over_view       = missing_over_view,
                      sql_query               = sql_query,
                      ggplot_statements       = ggplot_statements)
  
  
  return(output_list)

  
}
