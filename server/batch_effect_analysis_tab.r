#calculating the new matrix after batch effect
batch_adjusted_matrix <- eventReactive(input$calculate_batch_matrix,{
  
  isolate({
    if(input$batch_effect_choice == "combat"){
      temp_matrices <- batch_adjusted_matrices()
      temp_matrices[[paste(input$batch_effect_choice, input$batch_combat,
                           input$adj_var_combat, input$interest_var_combat, sep = "-")]]<-
        combat_batch_effect(filtered_score_matrix = filtered_score_matrix(),
                            batch = input$batch_combat,
                            outliers = input$outliers,
                            project = input$project,
                            adjustment_var = input$adj_var_combat,
                            interest_var = input$interest_var_combat)
      
      batch_adjusted_matrices(temp_matrices)
      
      
    }else if(input$batch_effect_choice == "sva"){
      temp_matrices <- batch_adjusted_matrices()
      
      temp_matrices[[paste(input$batch_effect_choice, input$batch_sva,
                           input$adj_var_sva, input$interest_var_sva, sep = "-")]]<-
        
        sva_batch_effect(filtered_score_matrix = filtered_score_matrix(),
                         adjustment_var = input$adj_var_sva,
                         outliers = input$outliers,
                         project = input$project,
                         interest_var = input$interest_var_sva)
      
      batch_adjusted_matrices(temp_matrices)
      
    }else if(input$batch_effect_choice == "supervised_sva"){
      temp_matrices <- batch_adjusted_matrices()
      
      temp_matrices[[paste(input$batch_effect_choice, input$batch_sva,
                           input$adj_var_supervised_sva,
                           input$interest_var_supervised_sva, sep = "-")]]<-
        
        supervised_sva_batch_effect(filtered_score_matrix = filtered_score_matrix(),
                                    adjustment_var = input$adj_var_supervised_sva,
                                    interest_var = input$interest_var_supervised_sva,
                                    project = input$project,
                                    outliers = input$outliers)
      batch_adjusted_matrices(temp_matrices)
      
    }else{
      
      temp_matrices <- batch_adjusted_matrices()
      
      temp_matrices[[paste(input$batch_effect_choice,
                           input$quantile_prob,
                           input$k_rank, sep = "-")]] <-
        
        ruv_batch_effect(filtered_score_matrix = filtered_score_matrix(),
                         experiments = all_experiments()[[1]],
                         outliers = input$outliers,
                         project = input$project,
                         regularization_par = input$regularization_par,
                         quantile_prob = input$quantile_prob,
                         k_rank = input$k_rank,
                         estimate_hkg = input$house_keeping_genes)
      
      batch_adjusted_matrices(temp_matrices)
      
    }
    updateSelectInput(session, "corrected_matrices", choices = names(temp_matrices),
                      selected = names(temp_matrices)[length(names(temp_matrices))])
    return(batch_adjusted_matrices())
  })
  
})

#Matrix summary output
observeEvent(input$corrected_matrices,{
  if(is.null(input$corrected_matrices)){
    output$batch_matrix_summary <- NULL
  }
  output$batch_matrix_summary <- DT::renderDataTable({
    matrix_summary_table(matrix_summary = summary(batch_adjusted_matrix()[[input$corrected_matrices]]),
                         col_names = colnames(batch_adjusted_matrix()[[input$corrected_matrices]]))
  })
})

#Show the download button
observeEvent(batch_adjusted_matrix(), {
  if(calculated_adjusted_matrix() == FALSE){
    shinyjs::show(id = "downloadAdjustedMatrix")
    
    calculated_adjusted_matrix(TRUE)
  }
  
})

observeEvent(input$batch_effect_previous_tab,{
  updateTabItems(session, "tabs", "corr_plot_tab")
  
  # header <- paste("Correlation Plot")
  # shinyjs::html("pageHeader", header)
  
})
observeEvent(input$batch_effect_next_tab,{
  updateTabItems(session, "tabs", "batch_effect_plot_tab")
  
  # header <- paste("Batch Effect Plot")
  # shinyjs::html("pageHeader", header)
  
})

# Download Adjusted matrix ---------------------------------------------------------

#Download matrix as text file
output$downloadAdjustedMatrix <- downloadHandler(
  filename = "adjusted_matrix_and_metadata.zip",
  
  content = function(fname) {
    #changing working directory to temp to save the files before zipping them
    current_wd = getwd()
    tempdir = tempdir()
    setwd(tempdir)
    
    write.table(batch_adjusted_matrix()[[input$corrected_matrices]],
                "adjusted_data_matrix.tsv", row.names = FALSE, col.names = TRUE,
                sep = "\t")
    
    write.table(attr(batch_adjusted_matrix()[[input$corrected_matrices]], "meta"),
                "matrix_metadata.tsv",
                sep = "\t", row.names = FALSE, col.names = TRUE)
    
    zip(zipfile = fname,files = c("adjusted_data_matrix.tsv", "matrix_metadata.tsv"))
    
    setwd(current_wd)
    
  }
)