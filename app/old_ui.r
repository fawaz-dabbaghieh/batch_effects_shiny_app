library(foreach)
library(matrixStats)
library(DeepBlueR)
library(stringr)
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
library(DT)
library(shinythemes)
library(sva)
library(shinydashboard)

#importing functions
for(f in list.files("../functions/")){
  source(paste0('../functions/',f))
}


#JvkPLwsTHbb3Fyi4

# Header UI ---------------------------------------------------------------

fluidPage(#theme = shinytheme("flatly"),

  shinydashboard::box(width = 11,
      tags$h3("Batch effect Analysis and Visualization")
      ),
  shinydashboard::box(width = 1,
      tags$a(href = "#", onclick = "$('#login_popup').modal('show')", 
             HTML('
                <p><font size = "4"> log-in </font></p>
               ')
             )
  ),
  

# Log-in popup ------------------------------------------------------------

bsModal(id ="login_popup", title = "Log-in", trigger = "",
        
        div(
          div(
            style="display:inline-block; vertical-align: middle;",
            textInput("user_key", "User Key", value = "JvkPLwsTHbb3Fyi4")
          ),
          div(
            style="display:inline-block; vertical-align: middle;",
            #The ? buttong and the popover that will come up from clicking
            bsButton("user_key_info",label = "", icon = icon("question"), style = "default", size = "extra-small"),
            bsPopover("user_key_info", title = "info", content = "You can login with your user key, or stay anonymous",
                      placement = "right", trigger = "focus")
          ),
        size = "large"),
        
        #Start and restart buttons
        actionButton('start_btn', "Log-in")
        # actionButton('end_btn', 'Start Again')
        ),

# Login tab ---------------------------------------------------------------

  navlistPanel(
    
    # tabPanel("Login",
    #          #using div to group the input box with the ? info
    #          div(
    #            div(
    #              style="display:inline-block; vertical-align: middle;",
    #              textInput("user_key", "User Key", value = "anonymous_key")
    #            ),
    #            div(
    #              style="display:inline-block; vertical-align: middle;",
    #              #The ? buttong and the popover that will come up from clicking
    #              bsButton("user_key_info",label = "", icon = icon("question"), style = "default", size = "extra-small"),
    #              bsPopover("user_key_info", title = "info", content = "You can login with your user key, or stay anonymous",
    #                        placement = "right", trigger = "focus")
    #            )
    #          ),
    # 
    #          #Start and restart buttons
    #          actionButton('start_btn', "Start"),
    #          # actionButton('end_btn', 'Start Again'),
    # 
    #          #divider tag for where the inserted UI should be
    #          tags$div(id = 'addOptions')
    # 
    # ),

    # List experiments tab ---------------------------------------------------------------
    
    tabPanel("List Experiments",
             
             #divider tag for where the inserted UI should be
             tags$div(id = 'login_letter',
                      
                      HTML('<br></br><p><font size = "4" color = black> Please Log-in first, so you can list experiments </font></p>')
                      
                      ),
             tags$div(id = 'addOptions'),
             
             br(),
             hr(),
             # actionButton('list_exper_btn', "List Experiments"),
             
             # actionButton("list_all", "List current experiments"),
             # actionButton("list_selected", "List selected experiments"),
             DT::dataTableOutput("table"),
             bsModal("mtExperID", "Experiment Metadata", trigger = "exp_id",
                     
                     DT::dataTableOutput("summary"),
                     
                     size = "large")
             
    ),
    
    # Summary tab -------------------------------------------------------------
    
    # tabPanel("Summary",
    #          br(),
    #          actionButton("show_summary", "Show sample information"),
    #          textInput("sample_id", "Sample ID")
    #          # ,
    #          # DT::dataTableOutput("summary")
    #          ),
    
    # Score Matrix tab ------------------------------------------------------------
    
    
    tabPanel("Score Matrix",
             br(),
             fluidRow(
               shinydashboard::box(
                 
                 selectInput('aggregation', "Select Aggregation function",
                             c("min", "max", "sum", "mean", "var", "sd", "median", "count", "boolean"),
                             selected = "mean"),
                 
                 numericInput("variance","row variation cutoff", value = "0.05"),
                 tags$div(id = 'addChromosomes')
                 
                 
               ),
               
               shinydashboard::box(
                 radioButtons("type_of_score", "Choose method to calculate scoring matrix",
                              choices = c("Tiling Regions" = "tiling",
                                          "Annotations" = "annotation")),
                 conditionalPanel(
                   condition = "input.type_of_score == 'annotation'",
                   selectInput("annotations_list", "Select annotation", c(""))
                 ),
                 
                 conditionalPanel(
                   condition = "input.type_of_score == 'tiling'",
                   radioButtons("tiling_size", "Select Tiling Region Size",
                                choices = c("1000 base pairs" = "1000",
                                            "5000 base pairs" = "5000"),
                                selected = "5000"))
               )
               
             ),
             
             actionButton("calculate_matrix", "Calculate Score matrix"),
             downloadButton("downloadMatrix", "Download Data"),
             hr(),
             
             DT::dataTableOutput("matrix_summary")
    ),
    
    
    # Loading test data tab -------------------------------------------------------
    
    # tabPanel("Testing",
    #          br(),
    #          actionButton("load_test", "load testing data"),
    #          textOutput("testing_output")
    #          ),
    # 
    
    # Plot matrix tab -------------------------------------------------------------
    
    #The plot tab has a buttong to start the plotting process
    tabPanel("Plot Matrix",
             br(),
             fluidRow(
               shinydashboard::box(width = 3,
                   actionButton("plot_btn", "Calculate and Plot")
                   
               ),
               
               shinydashboard::box(width = 3,
                 downloadButton(outputId = "plot_down", label = "Download plot")

               ),
               shinydashboard::box(
                 radioButtons("plot_down_exten", label = "Choose file extension",
                              choices = c("PDF (static)" = "pdf",
                                          "HTML (Interactive)" = "html"))
               )
             ),
             
             # column(4,
             #        
             #        actionButton("plot_btn", "Calculate and Plot")
             #        
             #        ),
             # column(4,
             #        
             #        downloadButton(outputId = "plot_down", label = "Download plot"),
             #        radioButtons("plot_down_exten", label = "Choose file extension",
             #                     choices = c("PDF (static)" = "pdf",
             #                                 "HTML (Interactive)" = "html"))
             #        ),
             # # column(4,
             # #        downloadButton(outputId = "plot_down", label = "Download the plot")
             # # 
             # #        ),
             
             column(12,
                    hr(),
                    
                    div(
                      style="position:fixed",
                      plotlyOutput('plot', width = "auto", height = "auto"))
             )
    ),
    
    
    # Correlation matrix plot tab -------------------------------------------------
    
    #Correlation plot
    tabPanel("Corr. Plot",
             
             br(),
             fluidRow(
               shinydashboard::box(width = 2,
                 actionButton("plot_corr", "Plot Correlation")
               ),
               
               shinydashboard::box(
                 downloadButton(outputId = "corr_plot_down", label = "Download the plot")
               )
             ),
             # column(4,
             #        actionButton("plot_corr", "Plot Correlation")
             #        
             # ),
             # column(4,
             #        downloadButton(outputId = "corr_plot_down", label = "Download the plot"),
             #        
             #        br("Download not working yet")
             # ),
             # 
             div(
               style="position:fixed",
               plotOutput("corr_plot", width = "auto", height = "auto")
             )
    ),
    
    # Removing Batch Effect tab ---------------------------------------------------
    
    tabPanel("Batch Effect",
             br(),
             
             fluidRow(
               shinydashboard::box(width = 6,
                                   radioButtons("batch_effect_choice", "Choose method to calculate scoring matrix",
                                                choices = c("ComBat" = "combat",
                                                            "SVA" = "sva",
                                                            "Supervised SVA" = "supervised_sva")),
                                   bsButton("batch_pie",label = "", icon = icon("pie-chart"), style = "default", size = "default"),
                                   bsModal("batch_pie_chart", "Values for Key chosen", "batch_pie",size = "large",
                                           fluidPage(
                                             selectInput("key_pie", "Select Key to check the value distribution", c("")),
                                             plotlyOutput("batch_pie_plot", width = "auto", height = "auto"))),
                                   br(),br(),
                                   actionButton("calculate_batch_matrix", "Adjust matrix using ComBat")
                                   
                                             
               ),
               shinydashboard::box(width = 6,
                                   conditionalPanel(
                                     condition = "input.batch_effect_choice == 'combat'",
                                     
                                     selectInput('batch', "Select a batch to adjust for",c("")),
                                     selectInput("adj_var", "Adjustment Variable",c("")),
                                     selectInput("interest_var", "Variable of interest", c(""))
                                   )
                 
               )
             ),
             
             # 
             # column(5,
             #        radioButtons("batch_effect_choice", "Choose method to calculate scoring matrix",
             #                     choices = c("ComBat" = "combat",
             #                                 "SVA" = "sva",
             #                                 "Supervised SVA" = "supervised_sva"))
             # ),
             # 
             # column(7,
             #        conditionalPanel(
             #          condition = "input.batch_effect_choice == 'combat'",
             #          
             #          selectInput('batch', "Select a batch to adjust for",c("")),
             #          selectInput("adj_var", "Adjustment Variable",c("")),
             #          selectInput("interest_var", "Variable of interest", c(""))
             #        )
             # ),
             # 
             # # verbatimTextOutput("batches"),
             # column(5,
             #        actionButton("calculate_batch_matrix", "Adjust matrix using ComBat")
             #        
             # ),
             # 
             # column(7,
             #        div(style="display:inline-block; horizontal-align: left;",
             #            # p(style="fixed ;display:inline-block; horizontal-align: right;",
             #            #   "This buttong does magic"),
             #            bsButton("batch_pie",label = "", icon = icon("pie-chart"), style = "default", size = "default"),
             #            bsModal("batch_pie_chart", "Values for Key chosen", "batch_pie",size = "large",
             #                    fluidPage(
             #                      selectInput("key_pie", "Select Key to check the value distribution", c("")),
             #                      plotlyOutput("batch_pie_plot", width = "auto", height = "auto")
             #                      
             #                      
             #                    ))
             #        )
             # ),
             
             bsPopover("batch_pie", title = "", content = "This button will show you a pie chart for any key you choose",
                       placement = "bottom", trigger = "hover"),
             
             # div(
             #   style="display:inline-block; vertical-align: middle;",
             #   #The ? buttong and the popover that will come up from clicking
             #   bsButton("batch_pie",label = "", icon = icon("pie-chart"), style = "default", size = "extra-small"),
             #   bsModal("batch_pie_chart", "Values for Batch chosen", "batch_pie",size = "large",
             #           fluidPage(
             #             selectInput("key_pie", "Select Key to check the value distribution", c("")),
             #             plotlyOutput("batch_pie_plot", width = "100%", height = "auto")
             #             
             #           )),
             #   bsPopover("batch_pie", title = "", content = "This buttong will show you a pie chart for any key you choose",
             #             placement = "right", trigger = "hover")
             # )
             # ),
             hr(),
             DT::dataTableOutput("batch_matrix_summary")
    ),
    
    # Batch Effect Plot tab ---------------------------------------------------
    
    tabPanel("Batch Effect Plot",
             
             tabsetPanel(
               type = "tabs",
               tabPanel("Plot",
                        
                        fluidRow(
                          shinydashboard::box(width = 4,
                                              br(),
                                              actionButton("plot_batch", "Plot Adjusted matrix for Batch"),
                                              br(),
                                              br(),
                                              br(),
                                              actionButton("compare_plots", "compare the plots")
                                              
                            
                          ),
                          shinydashboard::box(
                            radioButtons("plot_batch_down_exten", label = "Choose file extension",
                                         choices = c("PDF (static)" = "pdf",
                                                     "HTML (Interactive)" = "html")),
                            downloadButton(outputId = "plot_batch_down", label = "Download the plot")
                          )
                        ),

                        br(),
                        bsModal(id ="two_plots", title = "Before and after ComBat batch analysis", trigger = "compare_plots",
                                splitLayout(
                                  plotlyOutput("batch_plot1", width = "auto", height = "auto"),
                                  plotlyOutput('plot1', width = "auto", height = "auto")
                                )
                                # fluidRow(
                                #   div(
                                #     style="position:fixed",
                                #     plotlyOutput("batch_plot1", width = "auto", height = "auto")
                                #   )
                                # ),
                                # 
                                # fluidRow(
                                #   div(
                                #     style="position:fixed",
                                #     plotlyOutput('plot1', width = "auto", height = "auto")
                                #   )
                                # )
                                # div(
                                #   div(
                                #     style="position:fixed ;display:inline-block; horizontal-align: left;",
                                #     plotlyOutput("batch_plot1", width = "auto", height = "auto")
                                #   ),
                                #   br(),
                                #   div(
                                #     style="position:fixed ;display:inline-block; horizontal-align: left;",
                                #     plotlyOutput('plot1', width = "auto", height = "auto")
                                #     )
                                # )
                                
                                
                                # fluidRow(
                                #   splitLayout(cellWidths = c("50%", "50%"),
                                #               div(
                                #                 style="position:fixed",
                                #                 plotlyOutput("batch_plot1", width = "auto", height = "auto")
                                #               ),
                                #               div(
                                #                 style="position:fixed",
                                #                 plotlyOutput('plot1', width = "auto", height = "auto"))
                                #   )
                                #   )
                        ),
                        div(
                          style="position:fixed",
                          plotlyOutput("batch_plot", width = "auto", height = "auto")
                        )
               ),
               tabPanel("Compare plots",
                        "a;slkdfj"
               )
               
             )
    )
  )
)