# listing genomes ---------------------------------------------------------
#Listing the chromosome according to the genome chosen
observeEvent(input$genome, {
  # chromsome <- input$genome
  updateSelectInput(session, inputId = "project",
                    choices = check_project(genome = input$genome,
                                            user_key = user_key))
  
  updateSelectInput(session, inputId = "annotations_list",
                    choices = deepblue_list_annotations(genome = input$genome,
                                                        user_key = user_key)$name)
  
  
  updateSelectInput(session, inputId = 'chr', choices = c("",
                                                          deepblue_chromosomes(genome = input$genome,
                                                                               user_key = input$user_key)$id))
  
  
})