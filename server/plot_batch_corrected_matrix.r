#plotting the matrix after batch effect
plot_pca_batch <- eventReactive(input$plot_batch, {
  #get plot
  
  plot_pca_batch <- plotly_pca(experiments_info_meta = experiments_info_meta(),
                               filtered_score_matrix = batch_adjusted_matrix()[[input$corrected_matrices]],
                               name = input$corrected_matrices,
                               project = input$project,
                               type_of_score = input$type_of_score,
                               color_by = input$color_by_batch,
                               epigenetic_mark = input$epigenetic_mark,
                               first_pc = input$first_pc_batch,
                               second_pc = input$second_pc_batch,
                               show_legend = TRUE)
  
  
  showTab(inputId =  "batch_plot_box", target = "Download Plot")
  
  return(plot_pca_batch)
})
output$batch_plot <- renderPlotly(plot_pca_batch())

# downloading batch effect plot -------------------------------------------

output$plot_batch_down <- downloadHandler(
  filename =  function() {
    
    if(input$plot_batch_down_exten == "pdf"){
      paste0(input$type_of_score," ","after Batch"," ", input$epigenetic_mark,".pdf")
      
    }else{
      paste0("Tiling Regions after Batch"," ", input$epigenetic_mark,".html")
    }
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    
    if(input$plot_batch_down_exten == "pdf"){
      export(plot_pca_batch(), file = file)
      # 
      # pdf(file) # open the pdf device
      # print(plot_pca_batch())
      # dev.off()  # turn the device off
    }else{
      plot_pca_plotly <- plotly_build(plot_pca_batch())
      htmlwidgets::saveWidget(as_widget(plot_pca_plotly), file = file)
    }
  } 
)


observeEvent(input$batch_effect_plot_previous_tab,{
  updateTabItems(session, "tabs", "batch_effect_tab")
  
})
observeEvent(input$batch_effect_plot_next_tab,{
  updateTabItems(session, "tabs", "compare_plots_tab")
  
})