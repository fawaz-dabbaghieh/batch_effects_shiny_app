#plot data
first_pca_plot <- eventReactive(input$plot_btn, {
  #get plot
  
  plotly_pca <- plotly_pca(experiments_info_meta = experiments_info_meta(),
                           filtered_score_matrix = filtered_score_matrix(),
                           name = paste("Raw Matrix", input$epigenetic_mark,
                                        input$project),
                           project = input$project,
                           type_of_score = input$type_of_score,
                           color_by = input$color_by,
                           epigenetic_mark = input$epigenetic_mark,
                           first_pc = input$first_pc,
                           second_pc = input$second_pc,
                           show_legend = TRUE)
  
  
  showTab(inputId = "plot_box", target = "Download Plot")
  
  
  return(plotly_pca)
})

output$plot <- renderPlotly(first_pca_plot())

observeEvent(input$plot_matrix_previous_tab,{
  
  # if((input$project == "TEPIC reprocessed IHEC data") &
  #    (input$genome == "GRCh38") &
  #    (input$epigenetic_mark == "Gene Expression")){
  #   updateTabItems(session, "tabs", "list_experiments_tab")
  #   
  #   header <- paste("List Experiments")
  #   shinyjs::html("pageHeader", header)
  # }else{
  updateTabItems(session, "tabs", "score_matrix_tab")
  
  # header <- paste("Calculate Score Matrix")
  # shinyjs::html("pageHeader", header)
  # }
})

observeEvent(input$plot_matrix_next_tab,{
  updateTabItems(session, "tabs", "corr_plot_tab")
  
  # header <- paste("Correlation Plot")
  # shinyjs::html("pageHeader", header)
})

# Download first plot -----------------------------------------------------------

output$plot_down <- downloadHandler(
  filename =  function() {
    
    if(input$plot_down_exten == "pdf"){
      paste0(input$type_of_score," ", input$epigenetic_mark,".pdf")
      
    }else{
      paste0(input$type_of_score," ", input$epigenetic_mark,".html")
    }
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    
    if(input$plot_down_exten == "pdf"){
      export(first_pca_plot(), file = file)
      # pdf(file) # open the pdf device
      # print(plot_pca())
      # dev.off()  # turn the device off
    }else{
      # plot_pca_plotly <- plotly_build(plot_pca())
      htmlwidgets::saveWidget(as_widget(first_pca_plot()), file = file)
      
    }
  }
)