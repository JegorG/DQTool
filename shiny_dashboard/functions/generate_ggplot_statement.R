replace_null <- function(list, replacement_string){
  
  ifelse(is.null(list), replacement_string, list)
  
}

# Function to generate the statement. Dataset should be a string and input should be a list of the inputs

generate_ggplot_statement <- function(dataset, input) {
  
  
  input$facet_row <- replace_null(input$facet_row, ".")
  input$facet_col <- replace_null(input$facet_col, ".")
  
  
  
  gg_x_y <- input$Type == "Histogram" || input$Type ==
    "Density"
  gg_fil <- input$Type == "Histogram" || input$Type ==
    "Density" || input$Type == "Dotplot"
  if (gg_fil || input$Type == "Scatter")
    jitt <- FALSE
  else
    jitt <- input$jitter
  p <- paste(
    "ggplot(", dataset, ", aes(",
    if (gg_x_y) {
      "x = input$y_var"
    }
    else {
      "x = input$x_var, y = input$y_var"
    },
    if (NROW(input$group) > 0 && gg_fil) {
      ", fill = input$group"
    }
    else if (NROW(input$group) > 0 && !gg_fil) {
      ", colour = input$group"
    },
    ")) + ",
    if (input$Type == "Histogram")
      paste(
        "geom_histogram(position = 'identity', alpha = input$alpha, ",
        "binwidth = input$binwidth)",
        sep = ""
      ),
    if (input$Type ==
        "Density")
      paste(
        "geom_density(position = 'identity', alpha = input$alpha, ",
        "adjust = input$adj_bw)",
        sep = ""
      ),
    if (input$Type ==
        "Boxplot")
      "geom_boxplot(notch = input$notch)",
    if (input$Type ==
        "Violin")
      "geom_violin(adjust = input$adj_bw)",
    if (input$Type ==
        "Dotplot")
      paste(
        "geom_dotplot(binaxis = 'y', binwidth = input$binwidth, ",
        "stackdir = 'input$dot_dir')",
        sep = ""
      ),
    if (input$Type ==
        "Dot + Error")
      paste(
        "geom_point(stat = 'summary', fun.y = 'mean') +\n  ",
        "geom_errorbar(stat = 'summary', fun.data = 'mean_se', ",
        "\n                width=0, fun.args = list(mult = input$CI))",
        sep = ""
      ),
    if (input$Type == "Scatter")
      "geom_point()",
    if (input$Type == "Scatter" &&
        input$line)
      "+ geom_smooth(se = input$se, method = 'input$smooth')",
    
    if (isTRUE(jitt))
      paste(
        " + geom_jitter(size = input$size_jitter, ",
        "alpha = input$opac_jitter, width = input$width_jitter, ",
        "colour = 'input$col_jitter')",
        sep = ""
      ),
    if (input$Type == "Line")
"geom_line()",
    sep = ""
  )
  facets <- paste(input$facet_row, "~", input$facet_col)
  if (facets != ". ~ .")
    p <- paste(p, "+ facet_grid(", facets, ")")
  if (!is.null(input$lab_x))
    p <- paste(p, glue("+ labs(x = '{input$lab_x}')"))
  if (!is.null(input$lab_y))
    p <- paste(p, glue("+ labs(y = '{input$lab_y}')"))
  if (!is.null(input$caption))
    p <- paste(p, glue("+ labs(caption = '{input$caption}')"))
  if (!is.null(input$title))
    p <- paste(p, glue("+ ggtitle(label = '{'input$title'}')"))
  if (!is.null(input$subtitle))
    p <- paste(p, glue("+ ggtitle(label = waiver(), subtitle = '{input$subtitle}')"))
  if (isTRUE(input$adj_leg == "Change legend"))
    p <- paste(p,
               "+ labs(",
               if (gg_fil)
                 "fill"
               else
                 "colour",
               " = 'input$leg_ttl')",
               sep = "")
  if (isTRUE(input$adj_col))
    p <- paste(p,
               "+ scale_",
               if (gg_fil)
                 "fill"
               else
                 "colour",
               "_brewer(palette = 'input$palet')",
               sep = "")
  if (NROW(input$theme) > 0)
    p <- paste(p, "+", input$theme)
  if (isTRUE(input$adj_fnt_sz) ||
      isTRUE(input$adj_fnt) || isTRUE(input$rot_txt) ||
      isTRUE(input$adj_leg != "Keep legend as it is") ||
      isTRUE(input$adj_grd)) {
    p <-
      paste(
        p,
        paste(
          " + theme(\n    ",
          if (isTRUE(input$adj_fnt_sz))
            "axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
          if (isTRUE(input$adj_fnt_sz))
            "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
          if (isTRUE(input$adj_fnt))
            "text = element_text(family = 'input$font'),\n    ",
          if (isTRUE(input$rot_txt))
            "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
          if (isTRUE(input$adj_leg == "Remove legend"))
            "legend.position = 'none',\n    ",
          if (isTRUE(input$adj_leg ==
                     "Change legend"))
            "legend.position = 'input$pos_leg',\n    ",
          if (isTRUE(input$grd_maj))
            "panel.grid.major = element_blank(),\n    ",
          if (isTRUE(input$grd_min))
            "panel.grid.minor = element_blank(),\n    ",
          ")",
          sep = ""
        ),
        sep = ""
      )
  }
  p <- str_replace_all(p, c(`input\\$y_var` = input$y_var,
                            `input\\$x_var` = input$x_var, `input\\$group` = input$group,
                            `input\\$notch` = as.character(input$notch),
                            `input\\$binwidth` = as.character(input$binwidth),
                            `input\\$adj_bw` = as.character(input$adj_bw),
                            `input\\$dot_dir` = as.character(input$dot_dir),
                            `input\\$alpha` = as.character(input$alpha),
                            `input\\$se` = as.character(input$se), `input\\$smooth` = as.character(input$smooth),
                            `input\\$CI` = as.character(input$CI), `input\\$size_jitter` = as.character(input$size_jitter),
                            `input\\$width_jitter` = as.character(input$width_jitter),
                            `input\\$opac_jitter` = as.character(input$opac_jitter),
                            `input\\$col_jitter` = as.character(input$col_jitter),
                            `input\\$lab_x` = as.character(input$lab_x),
                            `input\\$lab_y` = as.character(input$lab_y),
                            `input\\$title` = as.character(input$title),
                            `input\\$palet` = as.character(input$palet),
                            `input\\$fnt_sz_ttl` = as.character(input$fnt_sz_ttl),
                            `input\\$fnt_sz_ax` = as.character(input$fnt_sz_ax),
                            `input\\$font` = as.character(input$font), `input\\$leg_ttl` = as.character(input$leg_ttl),
                            `input\\$pos_leg` = as.character(input$pos_leg),
                            `input\\$subtitle` = as.character(input$subtitle)
                            ))
  p <- str_replace_all(p, ",\n    \\)", "\n  \\)")
  return(p)
}

# debugonce(generate_ggplot_statement)
# a <- generate_ggplot_statement("iris", input =list(Type = "Line",
#                                               x_var = "Sepal.Length",
#                                               y_var= "Sepal.Width", 
#                                               title = "Iris Title Glue",
#                                               subtitle = "Iris Sub glie",
#                                               caption = "caption",
#                                               lab_x = "haha",
#                                               lab_y = "LOOL"))
# 
# a
# eval(parse(text = a))
# 
# 
# ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
#   geom_point() +
#   ggtitle(label = "Test",subtitle = "Test") +
#   ggtitle(label = waiver(), subtitle = "Test2") +
#   labs(x = "asd") + 
#   labs(y = "af") +
#   scale_y_continuous(labels = percent) 
