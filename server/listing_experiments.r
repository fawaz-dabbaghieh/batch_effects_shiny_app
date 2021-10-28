#List experiments and info for experiments in a list
all_experiments <- eventReactive(input$list_exper_btn, {
  
  #getting experments
  isolate({
    withProgress(message = "Fetching experiments",min = 0,
                 max = 3, value = 0,
                 {
                   all_experiments <- c()
                   if(input$epigenetic_mark != "Gene Expression"){
                     all_experiments <- list_experiments(genome = input$genome,
                                                         epigenetic_mark = input$epigenetic_mark,
                                                         project = input$project,
                                                         user_key = user_key,
                                                         filter_coverage = input$filter_coverage)
                   }else{
                     all_experiments <- list_rna_seq_data(project = input$project,
                                                          user_key = user_key)
                   }
                   
                 })

    #rendering table
    output$table <- DT::renderDataTable({
      
      #all_experiments reutrns "\n" if no experiments were listed
      validate(
        need(!is.null(all_experiments),
             message = "We couldn't find experiments according to the inputs you chose")
      )
      
      if(input$epigenetic_mark == "Gene Expression"){
        experiments_list <- all_experiments()[[1]]
        experiments_list <- as.data.frame(experiments_list)
        experiments_list$id <- sapply(experiments_list$id, convert_to_link)
      }else{
        #converting experiments ids to interactive elements to show
        #metadata in a popup window
        experiments_list <- all_experiments()[[1]]
        experiments_list$id <- sapply(experiments_list$id, convert_to_link)
      }
      
      #all_experiments()[[1]]
      DT::datatable(experiments_list, filter = list(position = 'top', clear = FALSE),
                    escape = FALSE,selection = 'none',
                    options = list(
                      search = list(regex = TRUE, caseInsensitive = FALSE),
                      pageLength = 10)
      )
    })
    
    return(all_experiments)
  })

})

#Adding the next buttong if the experiments were successfully listed

observeEvent(all_experiments(),{
  if(!is.null(all_experiments()) & listed_experiments() == FALSE){
    insertUI(selector = "#list_experiments_tab_nav", where = "beforeBegin",
             ui = 
               tags$div(id = "inserted_nav",
                        fluidRow(column(width = 12, offset = 11,
                                        bsButton("list_experiments_next_tab",label = "Next", icon = icon("arrow-right"),
                                                 style = "default")
                        )),
                        br()
               )
    )
    listed_experiments(TRUE)
  }
})

#Controlling the next button
observeEvent(input$list_experiments_next_tab,{
  # if((input$project == "TEPIC reprocessed IHEC data") &
  #    (input$genome == "GRCh38") &
  #    (input$epigenetic_mark == "Gene Expression")){
  #   
  #   updateTabItems(session, "tabs", "plot_matrix_tab")
  #   
  #   header <- paste("Plotting PCA")
  #   shinyjs::html("pageHeader", header)
  #   
  # }else{
    updateTabItems(session, "tabs", "score_matrix_tab")
    
    # header <- paste("Calculate Score Matrix")
    # shinyjs::html("pageHeader", header)
  # }
  
})
