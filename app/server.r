# Done TODOs -------------------------------------------------------------------

#1 Only show download tabs and button after the data or plot has been generated so the user won't get an error (DONE!)
#2 when login again remove previous/next from second page (DONE! I think, will test for several cases later)
#3 remove the matrix and the output should be null again (Maybe use shinyjs::hide() then show again) (not really important now)
#4 Why correlation plot doesn't work (Haven't worked yet) maybe use do.call (Working now :-) DONE! )
#5 Do the batch effect with SVA and Supervised SVA (Done!)
#6 maybe start splitting the server into chuncks inside the server folder and source the chuncks (almost DONE!)
#7 Check for shiny as a R package (it's doable, need to understand how packages are organized first)
#8 Having some global variables so I won't need to fetch from DeepBlue many times (DONE!)
#9 Change all the plots to be in Plotly instead of ggplot (Done!!)
#10 Advanced option for plotting (choosing which PCAs to plot and filtering option) (DONE!)
#11 metadata to be interactive upon clicking on list of experiments (DONE!)
#12 RUV for batch effect
#13 select expression, for gene expression data
#14 activate and diactivate tabs and have a workflow for the app (Done!)
#15 genomic ranges R package
#16 Have a download button for the adjusted matrix (Done !!)
#17 Need to have the back button only in the calculate matrix page and the next is added once the calculation is done (Done!!)
#18 Fix the corrplot download (done!!)
#19 Score matrices and tables other than the first, lines don't need to be selected (Done!)
#20 Filter coverage files from the beginning with a check box (Done!)
#21 Make batch effect analysis matrices as a list (Done!!)
#22 Notification to choose a chromosome (Done!)
#23 Error for PC selection (Done!)


# TODOs -------------------------------------------------------------------
#7 Check for shiny as a R package 
#12 RUV for batch effect (from Markus' scripts to understand how it works) (Done I think)
#13 select expression, for gene expression data (almost done!)
#24 Gene expression function (Same function for one gene then all genes) (Didn't work)
#25 Gene expression ihec, map sample IDs and experiments name to get the final matrix
#27 Supervised SVA (use the method with the std of ranks) (Done I think)
#28 Add a place to choose outliers that can be used then in the RUV test and other tests (Done I think)
#29 Hide next buttong for Batch Effect tab until there's a batch effect matrix
#30 Show user the original number of rows of the raw matrix then the filtered one so they can change the cutt-off
#31 validate for the variation cuttoff not more than 1
#32 add a validate that user can't used houskeeping genes list for not gene expression
#Documentation in html using https://html-online.com/editor/ or https://html5-editor.net/

#Problems
# Can't use the std of ranks for the control on score matrix because they have some negative values

#New UI with shiny dashboard
library(foreach)
library(matrixStats)
library(DeepBlueR)
library(RUVnormalize)
library(stringr)
library(tidyr)
library(corrplot)
library(impute)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(stats) 
library(plotly)
library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(shinythemes)
library(sva)
library(shinydashboard)

#importing functions
for(f in list.files(file.path("..","functions"))){
  source(file.path("..","functions",f), local = TRUE)
}

# Server function ---------------------------------------------------------
function(input, output, session) {
  
  #identifying some variables and reactive variables
  #Need to review some of these variables and get rid of some
  inserted_inputs <- c()
  echo <- c()
  to <- c()
  user_key <- c()
  # genomes <- c()
  listed_experiments <- reactiveVal(FALSE)
  calculated_matrix <- reactiveVal(FALSE)
  calculated_adjusted_matrix <- reactiveVal(FALSE)
  logged_in <- reactiveVal("")
  batch_adjusted_matrices <- reactiveVal(list())

  observe(hideTab(inputId =  "plot_box", target = "Download Plot"))
  observe(hideTab(inputId =  "corr_plot_box", target = "Downlad Plot"))
  observe(hideTab(inputId =  "batch_plot_box", target = "Download Plot"))
  
  observeEvent(input$info,{
    #Depends which current input$tabs the user at, I'll render that help page for that tab when the help! is pressed :)
    markdown_pages <- list.files(file.path("..","help"))
    help_md <- markdown_pages[grep(input$tabs, markdown_pages)]
    
    output$help_output <- renderUI({
      # includeMarkdown(file.path("..","help",help_md))
      includeHTML(file.path("..","help",help_md))
    })
  })
  # output$myImage <- renderImage({
  #   
  #   list(src = "../help/ezgif.com-gif-to-mp4.gif",
  #        contentType = 'image/gif')
  # })
# logging in --------------------------------------------------------------
  source(file.path("..","server","logging_in.r"), local = TRUE)
  
# # listing genomes ---------------------------------------------------------
  source(file.path("..","server","listing_genomes.r"), local = TRUE)
  
# Listing Experiments -----------------------------------------------------
  source(file.path("..","server","listing_experiments.r"), local = TRUE)

# Experiments metadat (summary) -------------------------------------------
  source(file.path("..","server","experiment_metadata_summary_modal.r"), local = TRUE)
  
# Metadata and Matrix -----------------------------------------------------

  ##Original Matrix Calculation Method

  # #make a table of metadata
  experiments_info_meta <- eventReactive(input$calculate_matrix, {
    #extract metadata
    if((input$project == "TEPIC reprocessed IHEC data") &
       (input$epigenetic_mark == "Gene Expression")){
      experiments_info_meta <- attr(all_experiments()[[2]], "meta")
    }else{
      experiments_info_meta <- suppressWarnings(extract_metadata(all_experiments()[[2]][input$table_rows_all]))
      
    }
  })
  
  source(file.path("..", "server", "calculate_score_matrix.r"), local = TRUE)

# Correlation plot --------------------------------------------------------
  source(file.path("..","server","correlation_plot.r"), local = TRUE)
  
# Update batch effects selection ------------------------------------------

  #update the keys for batch effects once we retrieve score matrix and metadata
  observeEvent(filtered_score_matrix(),{
    updateSelectInput(session, inputId = "batch_combat",
                      choices = colnames(attr(filtered_score_matrix(), "meta")))
    #metadata data is an attribute of the matrix
    updateSelectInput(session, inputId = "adj_var_combat",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "interest_var_combat",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "adj_var_sva",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "interest_var_sva",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "adj_var_supervised_sva",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "interest_var_supervised_sva",
                      choices = c("",colnames(attr(filtered_score_matrix(), "meta"))))
    
    updateSelectInput(session, inputId = "color_by",
                      choices = c(colnames(attr(filtered_score_matrix(), "meta"))),
                      selected = "biosource_name")
    
    updateSelectInput(session, inputId = "color_by_batch",
                      choices = c(colnames(attr(filtered_score_matrix(), "meta"))),
                      selected = "biosource_name")
  })
  
# Plotting first matrix ---------------------------------------------------
  source(file.path("..","server","plot_raw_matrix.r"), local = TRUE)
  
# Batch adjusted matrix -----------------------------------------------------
  source(file.path("..","server","batch_effect_analysis_tab.r"), local = TRUE)
  
# Plotting matrix after batch effect --------------------------------------
  source(file.path("..","server","plot_batch_corrected_matrix.r"), local = TRUE)

# Comparing plots ---------------------------------------------------------
  source(file.path("..","server","compare_plots_tab.r"), local = TRUE)
  
# Plotting pie chart ------------------------------------------------------

  observeEvent(filtered_score_matrix(),{
    updateSelectInput(session, inputId = "key_pie",
                      choices = sort(colnames(attr(filtered_score_matrix(), "meta"))))
  })
  batch_pie <- eventReactive(input$key_pie,{
    batch_pie <- pie_chart(metadata = attr(filtered_score_matrix(), "meta"),
                           key = input$key_pie)
  })
  output$batch_pie_plot <- renderPlotly(plotly_build(batch_pie()))


# Extra Stuff ------------------------------------------------------
  
  #Updating the pageHeader according to which tab we're on
  #tab_panel_header function just returns the tab name using the tab id
  observeEvent(input$tabs,{
    header <- paste(tab_panel_header(tab_panel = input$tabs))
    shinyjs::html("pageHeader", header)
  })
}
