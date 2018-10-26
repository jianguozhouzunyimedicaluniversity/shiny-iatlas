immuneinterface_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Clonal Diversity by Sample Group"),
        fluidRow(
            optionsBox(width = 4,
                       
                       # Drop-down selected diversity metrics
                       selectInput(
                           inputId = ns("diversity_metric_choice"),
                           label = "Select Receptor Type(s)",
                           choices = config_yaml$diversity_metric_choices,
                           selected = "Shannon"
                       ),
                       
                       # Checkbox selected receptor type(s)
                       checkboxGroupInput(
                           inputId = ns("receptor_type_choices"),
                           label = "Select Receptor Type(s)",
                           choices = config_yaml$receptor_type_choices,
                           selected = "TCR"
                       ),
                       
                       # Checkbox z-score option
                       checkboxInput(
                           inputId = ns("ztransform"),
                           label = "Plot Z-scores",
                           value = FALSE
                       )
            ),
            
            plotBox(width = 8,
                    # Show a plot of the generated distribution
                    plotOutput(outputId = ns("diversityPlot"))
            )
        )
        
    )
}

immuneinterface <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    output$diversityPlot <- renderPlot({
        diversity_metric   <- input$diversity_metric_choice
        receptor_types <- input$receptor_type_choices
        
        diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                         sep = "_"
        )
        
        ## create dfp, the data frame for plotting, based on choices
        
        plot_df <- subset_df() %>% 
            build_immuneinterface_df(group_internal_choice(), diversity_vars) %>% 
            dplyr::select(GROUP = group_internal_choice(), "diversity", everything()) %>% 
            tidyr::drop_na()
            # get_complete_df_by_columns(c("GROUP", "diversity"))
        
        print(plot_df)
        ## adjust scales
        if (diversity_metric %in% c("Evenness", "Richness")) {
            plot_df <- plot_df %>%
                mutate(diversity = log10(diversity + 1))
            scale_label <- glue::glue("log10({metric}+1)",
                                      metric = diversity_metric
            )
        } else {
            scale_label <- diversity_metric
        }
        
        print(plot_df)
        
        if (input$ztransform) {
            plot_df <- ztransform_df(plot_df)
            scale_label <- paste0("Z-score: ", scale_label)
        }
        y_label <- glue::glue("Diversity [{label}]", label = scale_label)
        ## custom colors if available
        
        print(plot_df)
        x <- create_boxplot(
            plot_df,
            x_col = "GROUP",
            y_col = "diversity",
            xlab = input$selection_choice,
            ylab = y_label,
            fill_colors = plot_colors()
            # color_col = "receptor"
        )
        print(x)
    })
}

