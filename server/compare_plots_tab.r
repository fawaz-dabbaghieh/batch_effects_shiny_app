two_plots <- eventReactive(input$compare_plots_btn, {
  
  isolate({
    p1 <- plotly_pca(experiments_info_meta = experiments_info_meta(),
                     filtered_score_matrix = filtered_score_matrix(),
                     project = input$project,
                     name = "Raw Matrix",
                     type_of_score = input$type_of_score,
                     color_by = input$color_by,
                     epigenetic_mark = input$epigenetic_mark,
                     first_pc = input$first_pc,
                     second_pc = input$second_pc,
                     show_legend = TRUE)
    
    p2 <- plotly_pca(experiments_info_meta = experiments_info_meta(),
                     filtered_score_matrix = batch_adjusted_matrix()[[input$corrected_matrices]],
                     project = input$project,
                     name = input$corrected_matrices,
                     type_of_score = input$type_of_score,
                     color_by = input$color_by_batch,
                     epigenetic_mark = input$epigenetic_mark,
                     first_pc = input$first_pc_batch,
                     second_pc = input$second_pc_batch,
                     show_legend = FALSE)
    
    sub_plot <- subplot(p1, p2, shareX = TRUE, shareY = TRUE) %>%
      layout(title = "Comparing Plots")
  })

    
  return(sub_plot)
})

output$compare_plots <- renderPlotly({two_plots()})

  

# downloading batch effect plot -------------------------------------------

output$compare_plots_down <- downloadHandler(
  filename =  function() {
    
    if(input$compare_plots_down_exten == "pdf"){
      paste0(input$type_of_score," ","after Batch"," ", input$epigenetic_mark,".pdf")
      
    }else{
      paste0("Tiling Regions after Batch"," ", input$epigenetic_mark,".html")
    }
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    
    if(input$compare_plots_down_exten == "pdf"){
      export(two_plots(), file = file)

    }else{
      plot_pca_plotly <- plotly_build(two_plots())
      htmlwidgets::saveWidget(as_widget(plot_pca_plotly), file = file)
    }
  } 
)

observeEvent(input$compare_plots_previous_tab,{
  updateTabItems(session, "tabs", "batch_effect_plot_tab")
  
})