#correlation plut output
observeEvent(input$plot_corr,{
  
  isolate({
    
    if(input$project == "DEEP"){
      column_row_names <- experiments_info_meta()$DEEP_SAMPLE_ID
    }else{
      column_row_names <- experiments_info_meta()$experiment
    }
    
    showTab(inputId =  "corr_plot_box", target = "Downlad Plot")
    
    output$corr_plot <- renderPlot(plot_correlation(filtered_score_matrix = filtered_score_matrix(),
                                                    project = input$project,
                                                    plot_title = paste(input$type_of_score, input$project ,input$epigenetic_mark)))
  })

})

#Corr plot download handler
output$corr_plot_down <- downloadHandler(
  filename =  function() {
    
    paste(input$type_of_score, input$project ,input$epigenetic_mark,".pdf")
    
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    
    pdf(file, width = input$pdf_width, height = input$pdf_height) # open the pdf device
    
    # correlation_plot()
    plot_correlation(filtered_score_matrix = filtered_score_matrix(),
                     project = input$project,
                     plot_title = paste(input$type_of_score, input$project ,input$epigenetic_mark))
    
    dev.off()  # turn the device off
    
  }
)

observeEvent(input$corr_plot_previous_tab,{
  updateTabItems(session, "tabs", "plot_matrix_tab")
  
  # header <- paste("Plotting PCA")
  # shinyjs::html("pageHeader", header)
  
})
observeEvent(input$corr_plot_next_tab,{
  updateTabItems(session, "tabs", "batch_effect_tab")
  
  # header <- paste("Calculating Batch Effect")
  # shinyjs::html("pageHeader", header)
  
})
