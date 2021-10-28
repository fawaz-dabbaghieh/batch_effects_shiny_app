observeEvent(input$show_input_check,{
  # if(input$chr == ""){chr <- NULL}else{chr <- input$chr}
  if(is.null(input$chr)){
    output$warning_message <- renderText("Just drawing your attention, that you did not choose chromosome(s),
                                          which means the whole genome is selected and the calculation might
                                          take sometime.
                                          Are you sure?")
    # output$warning_message <- includeHTML(tags$html("<p>lskdjfkdjf</p>"))
  }else{
    output$warning_message <- renderText("Just making sure you have the right input!")
  }
})

# #Get the score matrix
filtered_score_matrix <- eventReactive(input$calculate_matrix, {
  #closing the Input check modal
  toggleModal(session = session, modalId = "input_check", toggle = "close")
  
  #get scoring matrix
  validate(
    need(!is.null(all_experiments()),
         "There were no experiments found. Please, try again")
  )
  
  validate(
    need(input$variance <= 1 & input$variance >= 0, 
         "The row variation has to be between 0 and 1")
  )
  # if(input$project == "TEPIC reprocessed IHEC data"){
  #   filtered_score_matrix <- dnase1_score_matrix(dnase1_experiments = all_experiments()[[1]][input$table_rows_all],
  #                                                experiments_info_meta = experiments_info_meta(),
  #                                                chr = input$chr,
  #                                                user_key = user_key)
  # }
  if((input$project == "TEPIC reprocessed IHEC data") &
           (input$epigenetic_mark == "Gene Expression")){
    filtered_score_matrix <- all_experiments()[[2]]
  }
  else if(input$type_of_score == 'tiling'){
    filtered_score_matrix<- score_matrix_tiling_regions(experiments = all_experiments()[[1]][input$table_rows_all],
                                                        experiments_info_meta =  experiments_info_meta(),
                                                        variation = input$variance,
                                                        filtering = input$filter_rows,
                                                        tiling_size = input$tiling_size,
                                                        aggregation_function = input$aggregation,
                                                        genome = input$genome,
                                                        chr = input$chr,
                                                        user_key = user_key)
  }else if (input$type_of_score == "annotation"){
    filtered_score_matrix<- score_matrix_annotations(experiments = all_experiments()[[1]][input$table_rows_all],
                                                     experiments_info_meta =  experiments_info_meta(),
                                                     variation = input$variance,
                                                     filtering = input$filter_rows,
                                                     aggregation_function = input$aggregation,
                                                     annotation = input$annotations_list,
                                                     genome = input$genome,
                                                     chr = input$chr,
                                                     user_key = user_key)
    
    
  }
  
  # #This is to check if the matrix has been calculated or not in order to show
  # #the NEXT button on this page, it's switched to false here in case
  # #the user chose different inputs and made another matrix, the NEXT button won't be added twice
  # calculated_matrix(FALSE)
  updateSelectInput(session, "outliers", choices = c("", colnames(filtered_score_matrix)))
  
  return(filtered_score_matrix)
  
})

#Output matrix summary as a table
output$matrix_summary <- DT::renderDataTable({
  matrix_summary_table(matrix_summary = summary(filtered_score_matrix()),
                       col_names = colnames(filtered_score_matrix()))
})

observeEvent(filtered_score_matrix(),{
  
  if(calculated_matrix() == FALSE){
    shinyjs::show(id = "downloadMatrix")
    
    # insertUI(selector = "#score_matrix_tab_nav", where = "beforeBegin",
    #          ui = tags$div(id = "inserted_nav_score_matrix",
    #                        fluidRow(column(width = 12, offset = 10,
    #                                        bsButton("score_matrix_previous_tab",label = "Previous", icon = icon("arrow-left"),
    #                                                 style = "default"),
    #                                        bsButton("score_matrix_next_tab",label = "Next", icon = icon("arrow-right"),
    #                                                 style = "default")
    #                                        
    #                        )),
    #                        br()
    #          ))
    shinyjs::show(id = "score_matrix_next_tab")
    calculated_matrix(TRUE)
  }
})


observeEvent(input$score_matrix_previous_tab,{
  updateTabItems(session, "tabs", "list_experiments_tab")
  
  # header <- paste("List Experiments")
  # shinyjs::html("pageHeader", header)
  
})
observeEvent(input$score_matrix_next_tab,{
  updateTabItems(session, "tabs", "plot_matrix_tab")
  
  # header <- paste("Plotting PCA")
  # shinyjs::html("pageHeader", header)
  
})

# Download matrix ---------------------------------------------------------

#Download matrix as text file
output$downloadMatrix <- downloadHandler(
  filename = "matrix_and_metadata.zip",
  
  content = function(fname) {
    #changing working directory to temp to save the files before zipping them
    current_wd = getwd()
    tempdir = tempdir()
    setwd(tempdir)
    
    write.table(filtered_score_matrix(),
                "data_matrix.tsv", row.names = FALSE, col.names = TRUE,
                sep = "\t")
    
    write.table(attr(filtered_score_matrix(), "meta"),
                "matrix_metadata.tsv",
                sep = "\t", row.names = FALSE, col.names = TRUE)
    
    zip(zipfile = fname,files = c("data_matrix.tsv", "matrix_metadata.tsv"))
    
    setwd(current_wd)
    
  }
)