inserted_ui <- function(user_key,user_name,echo, id, genomes) {
  #The deepblue echo message as character
  # user <- deepblue_echo(user_key = user_key)

  #If user key is wrong, user get a message that he wasn't recognized and anonymous_key is used
  #else the user is greeted with his/her name
  # if (tail(strsplit(user, split = " ")[[1]], n=1) == "Stranger"){
  if (user_name == "a Stranger"){
    user_key <- "anonymous_key"
    showNotification(paste("User key was not recognized, the anonymous key will be used instead",
                           paste(echo, collapse = " ")), type = "message", duration = 5)
  }else{
    user_key <- user_key
    showNotification(paste(echo, collapse = " "), type = "message", duration = 5)
  }
  #progress bar for loading the UI
  withProgress(message = "Getting Projects, Epigenetics Marks, Chromosomes and Tests",min = 0,
               max = 2, value = 0,
               {
                 #Incrememnting progress bar manually
                 incProgress(amount = 1, message = "building input UI")
                 
                 insertUI(selector = '#addOptions',
                          where = "afterBegin",
                          #We wrap the element in a div with id to remove it when the restart buttong pressed
                          ui = tags$div(
                            
                            id = id,
                        
                            shinydashboard::box(collapsible = TRUE,width = 9,
                              selectInput('genome', "Select Genome",
                                          c(genomes$name)),
                              
                              selectInput('project', 'Select Project',choices = ""),
                              selectInput('epigenetic_mark', 'Select a Data Type',
                                          c("","Gene Expression",deepblue_list_epigenetic_marks(user_key = user_key)$name),
                                          multiple = F),
                              fluidRow(column(width = 9, offset = 9,
                                              checkboxInput("filter_coverage", "Filter coverage files", TRUE),
                                              actionButton('list_exper_btn', "List Experiments"),
                                                       style = "default"))
                            )
                          )
                 )
                 incProgress(amount = 1, message = "done")
               })
  return(id)
}