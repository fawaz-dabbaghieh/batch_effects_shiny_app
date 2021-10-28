#This is to print a summary of the metadata of a sample
summary_df <- eventReactive(input$exp_id,{
  if(input$epigenetic_mark == "Gene Expression"){
    summary_df <- t(as.data.frame(deepblue_info(id = input$exp_id, 
                                                user_key = user_key)))
    return(summary_df)
  }else{
    summary_df <- sample_summary(sample_id = input$exp_id, user_key = user_key)
    return(summary_df)
  }
})

output$summary <- DT::renderDataTable({
  DT::datatable(summary_df(), filter = list(position = 'top', clear = FALSE),
                selection = 'none', options = list(
                  search = list(regex = TRUE, caseInsensitive = TRUE),
                  pageLength = 10)
  )
})