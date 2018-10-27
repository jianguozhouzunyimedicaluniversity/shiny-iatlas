immuneinterface_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Clonal Diversity by Sample Group"),
        fluidRow(
            optionsBox(
                width = 4,
                
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
            
            plotBox(width = 12,
                    # Show a plot of the generated distribution
                    plotlyOutput(ns("diversityPlot")) %>% 
                        shinycssloaders::withSpinner()
            )
        )
        
    )
}

immuneinterface <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    output$diversityPlot <- renderPlotly({
        diversity_metric   <- input$diversity_metric_choice
        receptor_types <- input$receptor_type_choices
        
        diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                         sep = "_"
        )
        
        ## create dfp, the data frame for plotting, based on choices
        
        plot_df <- subset_df() %>% 
            build_immuneinterface_df(group_internal_choice(), diversity_vars, input$ztransform) 
        
        y_lab <- create_immuneinterface_y_label(
            diversity_metric, input$ztransform)
        
        if(length(receptor_types) == 1){
            create_boxplot(
                plot_df,
                xlab = input$selection_choice,
                ylab = y_lab,
                color_col = NA,
                fill_colors = plot_colors()
            )

        } else {
            create_boxplot(
                plot_df,
                xlab = input$selection_choice,
                ylab = y_lab,
                color_col = "receptor",
                fill_colors = NULL
            )
        }
        
    })
}

create_immuneinterface_y_label <- function(diversity_metric, ztransform){
    if (diversity_metric %in% c("Evenness", "Richness")){
        y_label <- glue::glue(
            "log10({metric}+1)", 
            metric = diversity_metric
        )
    } else {
        y_label <- diversity_metric
    }
    
    if(ztransform){
        y_label <- stringr::str_c("Z-score: ", y_label)
    } 
    
    glue::glue("Diversity [{label}]", label = y_label)
}

