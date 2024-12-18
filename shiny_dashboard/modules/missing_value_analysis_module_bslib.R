
style <- "font-size:80%; padding-top: 0; padding-bottom: 0; text-align:left"


missing_value_analysis_bs_ui <- function(id, databases_input){
  ns <- NS(id)
  tagList(useShinyjs(),
  useKeys(),
  keysInput(ns("keys"), c("left", "right")),
  page_sidebar(
    sidebar = sidebar(
      bslib::accordion(
      div(id = "Dataset_panel", 
          accordion_panel("Dataset", icon = bsicons::bs_icon("database-add"), 
                      actionButton(ns("refresh_databases"), "Refresh"), 
                      pickerInput(ns("database_input"), "Database", choices = databases_input, multiple = FALSE,  options = list(`live-search` = TRUE, title = "Pick a dataset"), pickerOptions(maxOptions = 1)),
                      
                      conditionalPanel("input.database_input.length > 0",{
                        
                      pickerInput(ns("table_input"), "Table", choices = c(), multiple = FALSE,  options = list(`live-search` = TRUE, title = "Pick a dataset"), pickerOptions(maxOptions = 1))
                        
                      }, ns = NS(id)),
                      
                      
                      conditionalPanel("input.table_input.length > 0 && input.database_input.length > 0", {
                        
                      uiOutput(NS(id, "column_pickers"))
                        
                        
                      }, ns = NS(id))
                      
                      ), class = "first-accordion"),
      
      
      uiOutput(NS(id, "filter_panel")),
      
      uiOutput(NS(id, "plot_option_panel")),
      
      accordion_panel("Missing value settings", icon = bsicons::bs_icon("gear"), 
                      textInput(ns("extra_missing"), tooltip(
                        trigger = list(
                          "Missing Values",
                          bs_icon("info-circle")
                        ),
                        "NULL and trimmed empty string are counted as missing. Enter additional values seperate by ; For example: 9999-12-31;?"
                      ))),
      
      accordion_panel("Mode", icon = bs_icon("yin-yang"),
                      radioGroupButtons(ns("mode_setting"), tooltip(
                        trigger = list(
                          "Mode",
                          bs_icon("info-circle")
                        ),
                        "Choose sample to try out the layout of the plots and Whole Data when you want to query the whole database"
                      ),
                      choices = c("Sample", "Whole Dataset"),
                      selected = "Sample",
                      direction = "vertical"
                      ),
                      
                      conditionalPanel("input.mode_setting == 'Sample'", {
                        
                       sliderInput(ns("sample_rows"), min = 1000, label = "N rows to sample", max = 10000, value = 10000, step = 1000)
                        
                      }, ns = NS(id))
                      
      )
      , open = FALSE
                      
    ),
    
    conditionalPanel("input.mode_setting != 'Sample'", {
      
      input_task_button(ns("generate_graphs"), "Run", icon = icon("bolt"),
                        label_busy = "Generating Graphs",
                        icon_busy = icon("sync", class = "fa-spin"))
      
    }, ns = NS(id))
    
     
    ),
    
    

# mainPanel ---------------------------------------------------------------

    bslib::accordion(open = FALSE,
      accordion_panel("Plot Options", icon = bs_icon("sliders2-vertical"),

                      dragulaInput(
                        inputId = NS(id, "dad1"),
                        label = NULL,
                        sourceLabel = NULL,
                        targetsLabels = c("X", "Y", "Fill", "Color", "Size", "Facet"),
                        targetsIds = c("x", "y", "fill", "color", "size", "facet"),
                        choices = sample(LETTERS, 1200, T),
                        replace = TRUE,
                        copySource = TRUE,
                        targetsHeight = "40px",
                        width = "100%"
                      )
      )
    ),
plotOutput(NS(id, "test_plot")),
card_footer(
  card_body(
     # tags$head(tags$style(HTML(".no_gap_boxes > div > div:has(button) {display: flex; flex-grow: 1;}   
     #                               button {width: 100%}"))),
    layout_column_wrap(
      width = 1/5, class = "no_gap_boxes",
      dropMenu(actionButton("labels", "Labels", icon = icon("tag"), style = style, placement = "bottom"),
               
                labels_input("title", id),
                labels_input("subtitle", id),
                labels_input("caption", id),
                labels_input("x", id),
                labels_input("y", id),
                labels_input("fill", id),
                labels_input("color", id),
                labels_input("size", id)
               
               ),
      dropMenu(actionButton("axes", "Axes", icon = icon("lines-leaning"), style = style),
               
               # Will need to render this on the server side depending on class
               
                    tags$h5("Axis limits"),
                    numericRangeInput("x_limits", NULL, separator  = "X Axis", c(0, 1), min = 0, max =  1, step = 0.05),
                    numericRangeInput("y_limits", NULL, separator  = "Y Axis", c(0, 1), min = 0, max =  1, step = 0.05),
                    tags$hr(style = "margin-top: -5px; margin-bottom: 10px;"),
                    tags$h5("Number of breaks/labels"),
                    numericInputIcon("x_breaks", NULL, NULL, min = 0, step = 1, icon = "X Axis:"),
                    numericInputIcon("y_breaks", NULL, NULL, min = 0, step = 1, icon = "Y Axis:"), 
                    tags$hr(style = "margin-top: -5px; margin-bottom: 10px;"),
                    tags$h5("Coordiates"),
                    radioGroupButtons("coord_system", NULL, choices = c("Cartesian", "Flip", "Fixed", "Polar"), selected = "Cartesian")
               
               
               
               ),
      dropMenu(actionButton("theme", "Theme", icon = icon("palette"), style = style),
               
               pickerInput("plot_theme", "Theme", choices = c()),
               
               radioGroupButtons(
                 inputId = "legend_position",
                 label = "Legend Position:",
                 choiceNames = list(
                   phosphoricons::ph("arrow-left", title = "Left"),
                   phosphoricons::ph("arrow-up", title = "Top"),
                   phosphoricons::ph("arrow-down", title = "Bottom"),
                   phosphoricons::ph("arrow-right", title = "Right"),
                   phosphoricons::ph("x", title = "None")
                 ),
                 choiceValues = c("left", "top", "bottom", "right", "none"),
                 selected = "right",
                 justified = TRUE,
                 size = "sm"
               ),
               
               pickerInput("family_theme_main", "Font:", choices = c("")), 
               radioGroupButtons("family_font_weight_main", "Font Face:", choices = c("Plain", "Italic", "Bold", "Bold & Italic"), size = "sm"),
               numericInputIcon("family_size_main", NULL, 16, 1, 48, 1, "Font size:"),
               
               colorPickr(
                 inputId = "family_color_main",
                 label = "Font color:",
                 swatches = scales::viridis_pal()(10),
                 opacity = TRUE
               )
               
               
               ),
      dropMenu(actionButton("options", "Options", icon = icon("sliders"), style = style),
               
                        numericInputIcon("plot_width", "Plot Dimensions:",1000, 400, 1200, icon = "Width:"),
                        tags$div(style = "margin-top: -16px;"),
                        numericInputIcon("plot_height", NULL ,1000, 400, 1200, icon = "Height:"),
                        tags$hr(style = "margin-top: -5px; margin-bottom: 10px;"),
                        materialSwitch("use_plotly", "Use Plotly"), 
                        materialSwitch("add_logo", "Add Logo"),
                        materialSwitch("add_rag", "Add RAG lines")
                        
               ),
               
      dropMenu(actionButton("sort", "Select & Sort", icon = icon("sort"), style = style),
               
               pickerInput("select_column", "Select Column", choices = c()),
               materialSwitch("order_by_rag", "Order by RAG score"),
               materialSwitch("exclude_perfect", "Exclude columns without any missing")
               
               
               )
    ))),

    uiOutput(NS(id, "card_header")),
    uiOutput(NS(id, "plot_cards")),
    uiOutput(NS(id, "value_boxes"))
   # div(id = "table_mainpanel", card(DTOutput(NS(id, "table_mainpanel"))))
  
  

# value boxes -------------------------------------------------------------


  )
)


}


missing_value_analysis_bs_server <- function(id, databases = NULL, selected_database = NULL, tables = NULL, selected_table = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      table_selected <- reactiveVal(NULL)
      reactive_values <- reactiveValues()
      plot_input      <- reactiveValues()
      key_index       <- reactiveVal(1)
      
      observe({
        
        legends <- c("caption", "color", "fill", "size", "subtitle", "title", "x", "y")
        theme   <- c("plot.caption", "color", "fill", "size", "plot.subtitle", "plot.title", "axis.text.x", "axis.text.y")
        
        map2(legends, theme, assign_element_text_input, plot_input = plot_input, input = input)
        
        create_labs_args(legends, plot_input = plot_input)
        create_theme_args(plot_input = plot_input)
        pmap(list(map(c("color", "fill", "size"), ~plot_input[[.x]]),
                  c("guide_colorbar", "guide_legend", "guide_legend"),
                  c("color", "fill", "size")),
             create_guides_args,
             plot_input = plot_input
                  )
        
        #
        # labs
        # theme args
        # guides args -- need to adjust ggcall
        # element_text, legend.title[NAME], theme, 
      
      })
      
      observe({
        
        req(databases, selected_database, tables, selected_table)
        
        updatePickerInput(session, "database_input", choices = databases, selected = selected_database)
        updatePickerInput(session, "table_input", choices = tables, selected = selected_table)
        table_selected(selected_table)
        
      })
    
      
      observeEvent(input$keys, {

        req(reactive_values$plot_choices)

        current_plot <- input$choose_column_to_view

        current_index <- which(reactive_values$plot_choices == current_plot)

        new_index <- current_index + ifelse(input$keys == "left", -1, 1)

        # Make sure it is still in range

        new_index <- max(new_index, 1) # If zero, then 1
        new_index <- min(new_index, NROW(reactive_values$plot_choices)) # If above, then last

        new_column <- reactive_values$plot_choices[[new_index]]

        if(new_column != current_plot){

          updatePickerInput(session = session,
                            "choose_column_to_view",
                            selected = new_column)

        }



      })
      

# refresh button ----------------------------------------------------------
observeEvent(input$refresh_databases, {
  
  # input_     <- map(names(input), ~input[[.x]])      |> setNames(names(input))
  # plot_input_ <- map(names(plot_input), ~plot_input[[.x]]) |> setNames(names(plot_input))
  
  output$filter_panel <- NULL

  databases_available <- send_query_safe("tool_db", "SELECT DISTINCT(database) AS database FROM log_table")
  check_if_error_and_message(databases_available$error, "send_query")

  req(databases_available$result)

  updatePickerInput(session = session,
                    inputId = "database_input",
                    selected = c(),
                    choices = databases_available$result %>% pull(database) %>% c(., "tool_db") %>%  sort())

}, ignoreNULL = TRUE)
      

# data base selection to update tables ------------------------------------

      observeEvent(input$database_input, priority = 1, {

        req(input$database_input)

        output$filter_panel <- NULL

        new_choices <-
          send_query("tool_db", glue("SELECT table_name FROM log_table WHERE active = 1 AND database = '{input$database_input}'")) %>%
          pull(table_name)

        req(new_choices)

        updatePickerInput(session = session,
                          inputId = "table_input",
                          choices = new_choices,
                          selected = table_selected())
        
        if(!is.null(table_selected())) table_selected(NULL)

      }, ignoreNULL = TRUE)
      
      # observeEvent: Table to update columns -----------------------------------
      
      observeEvent(input$table_input, {

        req(input$table_input)

        database <- input$database_input
        table    <- input$table_input

        table_info <- send_query_safe("tool_db", glue("SELECT * FROM {database}_{table}_table_info"))
        check_if_error_and_message(table_info$error, "send_query")

        req(NROW(table_info$result) > 0)

        new_choices <- table_info$result %>% generate_choices()

        # Select columns picker

        output$column_pickers <- renderUI({

        tagList(
        pickerInput(
                          inputId = NS(id, "column_input"),
                          label = "Columns",
                          choices = new_choices,
                          selected = c(),
                          options = list(`live-search` = TRUE,
                                         `actions-box` = TRUE,
                                         title = "Select columns"),
                    multiple = TRUE),

        pickerInput(NS(id, "plot_group_by"),
                    label = "Group By:",
                    choices = new_choices,
                    multiple = TRUE,
                    options = list(`live-search` = TRUE, title = "Group by column", pickerOptions(maxOptions = 1)))
        )

        })


        # Generate all the filter panels

        unique_value_table <- send_query_safe("tool_db", glue("SELECT * FROM {database}_{table}_unique_val"))
        check_if_error_and_message(unique_value_table$error, "send_query_safe")

        req(NROW(unique_value_table$result) > 0)

        splitted_info <-
          table_info$result %>%
          left_join(unique_value_table$result) %>%
          group_split(name)

        new_filter_inputs <- map(splitted_info, generate_filters, id = id)

        output$filter_panel <- renderUI(

          accordion_panel("Filters", icon = bsicons::bs_icon("sliders"),
                          tagList(
                            pickerInput(
                              inputId = NS(id, "filter_columns"),
                              label = "Filter columns",
                              choices = new_choices,
                              options = list(`live-search` = TRUE,
                                             `actions-box` = TRUE,
                                             title = "Select columns to filter"),
                              multiple = TRUE)),

          tagList(new_filter_inputs)
          ))
        
        
        ### Update
        
        reactive_values$table_info <- table_info$result


        })

      

# generate plots ----------------------------------------------------------

      # This part will generate the plots on the fly when mode is sample      
      
observeEvent(input$generate_graphs, {
  
  req(input$database_input)
  req(input$table_input)
  req(input$column_input)
  req(input$plot_group_by)

  filter_list <- map(input$filter_columns, ~input[[glue("{.x}_filter_input")]], input)
  mva_info <- generate_mva_plots(
                     database       = input$database_input,
                     table          = input$table_input,
                     columns        = input$column_input,
                     filter_columns = input$filter_columns,
                     filter_list    = filter_list,
                     table_info     = reactive_values$table_info,
                     group_by       = input$plot_group_by,
                     facet_row      = input$facet_row,
                     facet_column   = input$facet_column,
                     color_by       = input$plot_color,
                     sample_mode    = input$mode_setting == "Sample",
                     n_rows         = input$sample_rows,
                     plot_type      = input$choose_graph,
                     plot_function  = input$choose_function,
                     id = id)

   ### Update ###

   lapply(mva_info$plot_names, function(x) output[[x]] <- renderPlot(mva_info$all_plots[[x]]))

   output$value_boxes <- renderUI(tagList(mva_info$ui_value_boxes))
   output$plot_cards  <- renderUI(tagList(mva_info$ui_plot_cards_list))
   output$card_header <- renderUI(mva_info$ui_card_header_settings)

   output$table_mainpanel <- DT::renderDT(

     mva_info$missing_over_view %>% arrange(ifelse(column == mva_info$plot_names[[1]], 0, 1)), options = list(lengthChange = FALSE)

   )


   reactive_values$plot_choices      <- mva_info$plot_names
   reactive_values$missing_over_view <- mva_info$missing_over_view

   all_choices   <-  reactive_values$plot_choices
   value_box_ids <- glue("{all_choices}_value_box")
   plot_ids      <- glue("{all_choices}_plot")

   map(c(value_box_ids, plot_ids), ~shinyjs::hide(id = .x, asis = TRUE))
   map(glue("{input$choose_column_to_view}_{c('value_box', 'plot')}"), ~shinyjs::show(id = .x, asis = TRUE))


})
      


# observeevent Update Plots -----------------------------------------------


      observeEvent(list(input$choose_column_to_view), {

        req(input$choose_column_to_view)
        
         all_choices   <-  reactive_values$plot_choices
         value_box_ids <- glue("{all_choices}_value_box")
         plot_ids      <- glue("{all_choices}_plot")

         map(c(value_box_ids, plot_ids), ~shinyjs::hide(id = .x, asis = TRUE))
         map(glue("{input$choose_column_to_view}_{c('value_box', 'plot')}"), ~shinyjs::show(id = .x, asis = TRUE))

         output$table_mainpanel <- DT::renderDT(

          reactive_values$missing_over_view %>% arrange(ifelse(input$choose_column_to_view == column, 0, 1)), options = list(lengthChange = FALSE)

         )

      })
      

# Plot settings panel -----------------------------------------------------

observeEvent(input$plot_group_by, {
  
  output$plot_option_panel <- NULL
  req(input$plot_group_by)
  
  
  # Dont allow to facet or color by the group by column
  
  reactive_values$facet_choices <- reactive_values$table_info %>% 
    filter(unique_values_available == 1) %>% 
    pull(name) 
  
  disable_vector <- reactive_values$facet_choices %in% input$plot_group_by
  
  panel <- 
    tagList(
    accordion_panel("Plot Options", icon = bsicons::bs_icon("graph-up"),
                    
                    radioGroupButtons(
                      inputId = NS(id, "choose_graph"),
                      label = "Graph type", 
                      choices = c("Bar" = "Histogram", "Line" = "Line"),
                      selected = "Line",
                      justified = TRUE),
                    
                    pickerInput(
                      inputId = NS(id, "choose_function"),
                      label   = "Function",
                      multiple = FALSE,
                      choices = c("Percentage missing" = "pct_missing", "Count missing" = "n_missing"),
                      selected = "pct_missing"
                    ),
                    
                    pickerInput(
                      inputId = NS(id, "facet_row"),
                      label   = "Facet row",
                      choices = reactive_values$facet_choices,
                      options = list("max-options" = 1, `actions-box` = TRUE),
                      multiple = TRUE,
                      selected = c(),
                      choicesOpt = list(disabled = disable_vector)
                    ),
                    
                    pickerInput(
                      inputId = NS(id, "facet_column"),
                      label   = "Facet column",
                      choices = reactive_values$facet_choices,
                      multiple = TRUE,
                      options = list("max-options" = 1, `actions-box` = TRUE),
                      selected = c(),
                      choicesOpt = list(disabled = disable_vector)
                    ),
                    
                    pickerInput(
                      inputId = NS(id, "plot_color"),
                      label   = "Color",
                      multiple = TRUE,
                      options = list("max-options" = 1, `actions-box` = TRUE),
                      choices = reactive_values$facet_choices,
                      selected = c(),
                      choicesOpt = list(disabled = disable_vector)
                    ),
                    
                    actionButton(NS(id, "additional_plot_settings"),
                                 div(bs_icon("three-dots-vertical"), style = "font-size:80%;text-align:left",  "Additional settings")
                    ))
    )
  
  output$plot_option_panel   <- renderUI(panel)
  
})
      

# update facet choices ----------------------------------------------------

observeEvent(input$facet_column,{
  
  # You should not be able to facet rows and columns
  
  req(input$facet_column)
  
    disabled_vector <- reactive_values$facet_choices %in% c(input$plot_group_by, input$facet_column)
    updatePickerInput(session, 
                      "facet_row", 
                      selected = input$facet_row,
                      choices = reactive_values$facet_choices,
                      choicesOpt = list(disabled = disabled_vector))
    
  
})      
      
   
      observeEvent(input$facet_row,{
        
        # You should not be able to facet rows and columns
        
        req(input$facet_row)
        
        disabled_vector <- reactive_values$facet_choices %in% c(input$plot_group_by, input$facet_row)
        updatePickerInput(session, 
                          "facet_column", 
                          selected = input$facet_column,
                          choices = reactive_values$facet_choices,
                          choicesOpt = list(disabled = disabled_vector))
      })      
      
  
      

# Additional_plot_settings ------------------------------------------------

observeEvent(input$additional_plot_settings, {


  showModal(extra_plot_settings())

})
      
output$test_plot <- renderPlot({
  
  ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point() +
    facet_wrap(vars(Species))
  
})
      
      

# OUTPUTS -----------------------------------------------------------------


})}







