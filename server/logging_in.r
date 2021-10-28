
#Adding the UI after clicking on start
observeEvent(input$log_in_btn, {
  
  echo <<- strsplit(deepblue_echo(input$user_key), split = " ")[[1]]
  to <<- match("to", echo)
  logged_in(paste(echo[(to+1):length(echo)], collapse = " "))
  
  if(logged_in() == "a Stranger"){
    user_key <<- "anonymous_key"
  }else{
    user_key <<- input$user_key
  }
  #
  genomes <- deepblue_list_genomes(user_key = user_key)
  
  #removing startup message
  removeUI(selector = '#login_warning')
  btn <- input$log_in_btn
  id <- paste0("txt",btn)    #The ID of the button is just a counter
  
  #When there's already an inserted UI it's removed and reloaded and
  #the inserted_inputs list is updated with the new UI id
  if(length(inserted_inputs)==1){ 
    removeUI(selector = paste0("#", inserted_inputs[length(inserted_inputs)]))
    
    inserted_inputs <<- c(inserted_ui(user_key = input$user_key,
                                      user_name = as.character(logged_in()),
                                      echo = echo,
                                      id = id,
                                      genomes = genomes), inserted_inputs)
    
    inserted_inputs <<- inserted_inputs[-length(inserted_inputs)]
    
    #This entered when the inserted_inputs is empty
  }else{ 
    inserted_inputs <<- c(inserted_ui(user_key = input$user_key,
                                      user_name = as.character(logged_in()),
                                      echo = echo,
                                      id = id,
                                      genomes = genomes), inserted_inputs)
  }
  
  #If the user gave an incorrected user_key, he will be warned about that
  #And anonymous key will be used instead

  Sys.sleep(1)
  toggleModal(session = session, modalId = "login_popup", toggle = "close")
  
  #In case the user somewhere in the middle of the process decides to login again
  #The navigation buttons will be removed and will be taken back to the first page
  removeUI(selector = "#inserted_nav")
  removeUI(selector = "#inserted_nav_score_matrix")
  updateTabItems(session, "tabs", "list_experiments_tab")
  
  
  # header <- paste("Listing Experiments")
  # shinyjs::html("pageHeader", header)
  
  
  #Used to check if the experiments and matrix has been listed in order to add the nav buttons
  #This will be changed to TRUE once the experiments or matrix are listed, so in case the user
  #lists other experiments or matrices it stays true and it won't add other nav buttons
  listed_experiments(FALSE)
  calculated_matrix(FALSE)
  
  shinyjs::hide(id = "downloadMatrix")
  shinyjs::hide(id = "downloadAdjustedMatrix")
  shinyjs::hide(id = "score_matrix_next_tab")
  
  
})

#changing the user name in the corner according to the user's details
output$logintext <- renderText({
  if(logged_in() != "") {
    if(logged_in() == "a Stranger"){
      return("Stranger")
    }else{
      return(logged_in())
    }
  }
  return("Login here")
})