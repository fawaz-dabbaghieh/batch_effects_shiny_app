#New UI with shiny dashboard
library(plotly)
library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(shinythemes)
library(shinydashboard)

# Header ------------------------------------------------------------------
dashboardPage(title = "Batch Effect Analysis and Visualization",
              
  dashboardHeader(
    title = "Batch Effect Analysis", titleWidth = 300,
	tags$li(class = "dropdown",
			tags$li(class = "dropdown", actionLink("info", "Help!"))
			),
    tags$li(class = "dropdown",
            # tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("login", textOutput("logintext")))
            )
  ),


# Sidebar items -----------------------------------------------------------

  dashboardSidebar(
    disable = FALSE,
    sidebarMenu(id = "tabs",
      menuItem("List Experiments", tabName = "list_experiments_tab"),
      menuItem("Score Matrix", tabName = "score_matrix_tab"),
      menuItem("RNA seq matrix", tabName = "rna_seq_matrix"),
      menuItem("Score Matrix DNase1", tabName = "score_matrix_dnase1_tab"),
      menuItem("Plot Matrix", tabName = "plot_matrix_tab"),
      menuItem("Correlation Plot", tabName = "corr_plot_tab"),
      menuItem("Batch Effect", tabName = "batch_effect_tab"),
      menuItem("Batch Effect Plot", tabName = "batch_effect_plot_tab"),
      menuItem("Compare Plots", tabName = "compare_plots_tab")
    )
  ),

  dashboardBody(
# Log-in popup ------------------------------------------------------------
    useShinyjs(),
# 
#     #help modal style
#     tags$style(".modal-content {background-color:#ECEFF4;}
#                 .modal-header {padding: 10px 16px;
#                               background-color: #3C8DBC;
#                               color: white;}"),

    # #Login modal style
    # tags$style("#login_popup .modal-header {padding: 10px 16px;
    #                           background-colo: #3C8DBC;}"),

    #help modal style
    tags$style(".modal-header {padding: 12px 16px;
                               background-color: #3C8DBC;
                               color: white;}"),
    tags$script(HTML('
        $(document).ready(function() {
        $("header").find("nav").append(\'<div id="pageHeader" class="myClass"></div>\');
        })
        ')),
    tags$head(
      # Include custom CSS
      includeCSS(file.path("..","www","styles.css"))
    ),

    bsModal(id ="login_popup", title = "Log-in", trigger = "login",
            #Having the textInput then the question mark next to it
            div(
              div(
                style="display:inline-block; vertical-align: middle;",
                textInput("user_key", "User Key", value = "JvkPLwsTHbb3Fyi4")
              ),
              div(
                style="display:inline-block; vertical-align: middle;",
                #The ? buttong and the popover that will come up from clicking
                bsButton("user_key_info",label = "", icon = icon("question"), style = "default", size = "extra-small"),
                bsPopover("user_key_info", title = "info", content = "You can login with your user key, or stay anonymous using anonymous_key",
                          placement = "right", trigger = "focus")
              )
              ),
            size = "large",
            #Log in Button
            actionButton('log_in_btn', "Log-in")
    ),

    bsModal(id="help_page", title = "Help!", trigger = "info",
            #In Server I will check which tab is it and output the html
            #of the help part of that tab
            # htmlOutput("help_output"),
            # #Cats Cat
            # imageOutput("myImage"),
            # 
            uiOutput("help_output"),
            size = "large"
      
    ),
    
# list experiments tab ----------------------------------------------------
    tabItems(
      tabItem(tabName = "list_experiments_tab",
              #divider tag for where the inserted UI should be
              tags$div(id = "list_experiments_tab_nav"),
              
              tags$div(id = 'login_warning',
                       
                       HTML('<br>
                            </br>
                            <p>
                            <font size = "5" color = black> Please Log-in first, you can use the key "anonymous_key"
                             or your own key in case you were registered.<br /> You can also register 
                            <a target="_blank" href="http://deepblue.mpi-inf.mpg.de/register.php"> here</a>
                            </font></p>')
                       
              ),
              tags$div(id = 'addOptions'),
              
              DT::dataTableOutput("table"),
              
              bsModal("mtExperID", "Experiment Metadata", trigger = "exp_id",
                      
                      DT::dataTableOutput("summary"),
                      
                      size = "large")
              ),

# score matrix tab------------------------------------------------------------
      tabItem(tabName = "score_matrix_tab",
              
              # tags$div(id = "score_matrix_tab_nav"),
              
              fluidRow(column(width = 12, offset = 10,
                              bsButton("score_matrix_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default"),
                              bsButton("score_matrix_next_tab",label = "Next", icon = icon("arrow-right"),
                                       style = "default")
                              
              )),
              br(),
              shinydashboard::box(collapsible = TRUE,width = 8,
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
                                                 selected = "5000")),
                                  selectInput('chr', 'Select a Chromosome, or leave empty for the whole genome', c(""), multiple = TRUE),
                                  # tags$div(id = 'addChromosomes'),
                                  fluidRow(column(width = 8, offset = 9,
                                                  actionButton("show_input_check", "Calculate Score matrix"),
                                                  style = "default"))
                                  
                                  ),
              
              shinydashboard::box(collapsible = TRUE,width = 4,
                                  
                                  selectInput('aggregation', "Select Aggregation function",
                                              c("min", "max", "sum", "mean", "var", "sd", "median", "count", "boolean"),
                                              selected = "mean"),
                                  
                                  radioButtons("filter_rows", "Choose filtering method",
                                               choices = c("Filter out non-complete rows" = "filter_non_complete",
                                                           "Filter using Row Variation Cutoff" = "row_variation")),
                                  conditionalPanel(
                                    condition = "input.filter_rows == 'row_variation'",
                                    numericInput("variance","row variation cutoff", value = "0.05")
                                    
                                  ),
                                  fluidRow(column(width = 4,offset = 6,
                                                  downloadButton("downloadMatrix", "Download Data"),
                                                  style = "default"))
                                  
                                  ),
              bsModal(id ="input_check", title = "Input Check", trigger = "show_input_check",
                      textOutput(outputId = "warning_message"),
                      # uiOutput(outputID = " warning_message"),
                      actionButton("calculate_matrix", "Continue!")
                      ),

              br(),
              DT::dataTableOutput("matrix_summary")
              ),

# rna seq matrix ---------------------------------------------------------

      # tabItem(tabName = "rna_seq_matrix",
      #         
      #         fluidRow(column(width = 12, offset = 10,
      #                         bsButton("rna_seq_matrix_previous_tab",label = "Previous", icon = icon("arrow-left"),
      #                                  style = "default"),
      #                         bsButton("rna_seq_matrix_next_tab",label = "Next", icon = icon("arrow-right"),
      #                                  style = "default")
      #         )),
      #         br()
      #         ),
# Plot matrix tab ---------------------------------------------------------

      tabItem(tabName = "plot_matrix_tab",
              
              fluidRow(column(width = 12, offset = 10,
                              bsButton("plot_matrix_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default"),
                              bsButton("plot_matrix_next_tab",label = "Next", icon = icon("arrow-right"),
                                       style = "default")
              )),
              br(),
                              
              tabBox(width = 12,
                     id = "plot_box",
                title = "Plot Options",
                tabPanel("Options",
                         selectInput("color_by", label = "Color Samples By:", choices = c("")),
                         numericInput(inputId = "first_pc",value = 1, label = "First PC", min = 1, max = 100),
                         numericInput(inputId = "second_pc",value = 2, label = "Second PC", min = 1, max = 100)
                         ),
                tabPanel("Plot",
                         actionButton("plot_btn", "Plot"),
                         plotlyOutput('plot', width = "auto", height = "auto")
                         ),
                        
                tabPanel("Download Plot",
                         radioButtons("plot_down_exten", label = "Choose file extension",
                                      choices = c("PDF (static)" = "pdf",
                                                  "HTML (Interactive)" = "html")),
                         downloadButton(outputId = "plot_down", label = "Download plot"))
              )
              ),

# Correlation plot tab ----------------------------------------------------

      tabItem(tabName = "corr_plot_tab",
              
              fluidRow(column(width = 12, offset = 10,
                              bsButton("corr_plot_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default"),
                              bsButton("corr_plot_next_tab",label = "Next", icon = icon("arrow-right"),
                                       style = "default")
                              
              )),
              br(),
              
              tabBox(width = 12,id = "corr_plot_box",
                     title = "Correlation Plot",
                     tabPanel("Plot",
                              actionButton("plot_corr", "Plot Correlation"),
                              plotOutput("corr_plot")
                              
                              ),
                     tabPanel("Downlad Plot",
                              numericInput("pdf_width", label = "Width", value = 10),
                              numericInput("pdf_height", label = "Height", value = 10),
                              downloadButton(outputId = "corr_plot_down", label = "Download Corr. plot")
                              
                              )
                     )
              ),

# Batch Effect tab --------------------------------------------------------
      tabItem(tabName = "batch_effect_tab",
              
              fluidRow(column(width = 12, offset = 10,
                              bsButton("batch_effect_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default"),
                              bsButton("batch_effect_next_tab",label = "Next", icon = icon("arrow-right"),
                                       style = "default")
                              
              )),
              br(),
              
              fluidRow(
                shinydashboard::box(width = 6,
                                    radioButtons("batch_effect_choice", "Choose method to calculate scoring matrix",
                                                 choices = c("ComBat" = "combat",
                                                             "SVA" = "sva",
                                                             "Supervised SVA" = "supervised_sva",
                                                             "RUV" = "ruv")),
                                    selectInput("outliers", "Select Outliers (Optional)", choices = c(""),multiple = TRUE),
                                    bsButton("batch_pie",label = "", icon = icon("pie-chart"), style = "default", size = "default"),
                                    bsModal("batch_pie_chart", "Values for Key chosen", "batch_pie",size = "large",
                                            fluidPage(
                                              selectInput("key_pie", "Select Key to check the value distribution", c("")),
                                              plotlyOutput("batch_pie_plot", width = "auto", height = "auto"))),
                                    # br(),br(),
                                    selectInput("corrected_matrices", "Batch Corrected Matrices", choices = NULL),
                                    fluidRow(column(width = 6, offset = 5),
                                             actionButton("calculate_batch_matrix", "Adjust matrix"),
                                             style = "default")
                                    # actionButton("calculate_batch_matrix", "Adjust matrix")
                ),
                shinydashboard::box(width = 6,
                                    conditionalPanel(
                                      condition = "input.batch_effect_choice == 'combat'",
                                      
                                      selectInput('batch_combat', "Select a batch to adjust for",c("")),
                                      selectInput("adj_var_combat", "Adjustment Variable",c("")),
                                      selectInput("interest_var_combat", "Variable of interest", c(""))
                                    ),
                                    conditionalPanel(
                                      condition = "input.batch_effect_choice == 'sva'",
                                      
                                      selectInput("adj_var_sva", "Adjustment Variable",c("")),
                                      selectInput("interest_var_sva", "Variable of interest", c(""))
                                    ),
                                    conditionalPanel(
                                      condition = "input.batch_effect_choice == 'supervised_sva'",
                                      
                                      # selectInput('batch_supervised_sva', "Select a batch to adjust for",c("")),
                                      selectInput("adj_var_supervised_sva", "Adjustment Variable",c("")),
                                      selectInput("interest_var_supervised_sva", "Variable of interest", c(""))
                                    ),
                                    conditionalPanel(
                                      condition = "input.batch_effect_choice == 'ruv'",
                                      
                                      numericInput("regularization_par",
                                                   "Regularization parameter for the unwanted variation (nu.coeff)", value = 0.00001),
                                      
                                      # radioButtons("house_keeping_genes", "Method to estimate House Keeping Genes",
                                      #              choices = c("Estimate using rank analysis" = "estimate_hkg",
                                      #                          "Use a predefined list" = "use_hkg_list")),
                                      #This html is generated from the previous radiobuttons function
                                      #I just wanted to have a href there and I tweaked the raw html and re-introduced it
                                      div(
                                        HTML('<div id="house_keeping_genes" class="form-group shiny-input-radiogroup shiny-input-container">
                                              <label class="control-label" for="house_keeping_genes">Method to estimate House Keeping Genes</label>
                                              <div class="shiny-options-group">
                                                <div class="radio">
                                                  <label>
                                                    <input type="radio" name="house_keeping_genes" value="estimate_hkg" checked="checked"/>
                                                    <span>Estimate using rank analysis</span>
                                                  </label>
                                                </div>
                                                <div class="radio">
                                                  <label>
                                                    <input type="radio" name="house_keeping_genes" value="use_hkg_list"/>
                                                    <span>Use a predefined <a href="https://www.nanostring.com/application/files/7014/8943/0117/TN_Normalization_of_Expression_Data.pdf" target="_blank" rel="noopener">list</a></span>
                                                  </label>
                                                </div>
                                              </div>
                                            </div>')
                                      ),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.house_keeping_genes == 'estimate_hkg'",
                                        
                                        # numericInput("regularization_par",
                                        #              "Regularization parameter for the unwanted variation",
                                        #              value = 0.00001),
                                        
                                        numericInput("quantile_prob",
                                                     "Quantile probability used in the rank analysis estimation [0,1]",
                                                     value = 0.005, min = 0, max = 1)

                                      ),
                                      
                                      numericInput("k_rank", "Desired rank for the estimated unwanted variation term",
                                                   value = 5)
                                    ),
                                    fluidRow(column(width = 6, offset = 5),
                                    downloadButton("downloadAdjustedMatrix", "Download Data"),
                                    style = "default")
                )
              ),
              
              bsPopover("batch_pie", title = "", content = "This button will show you a pie chart for any key you choose",
                        placement = "bottom", trigger = "hover"),

              hr(),
              DT::dataTableOutput("batch_matrix_summary")
              ),

# Batch effect plot tab ---------------------------------------------------

      tabItem(tabName = "batch_effect_plot_tab",

              fluidRow(column(width = 12, offset = 10,
                              bsButton("batch_effect_plot_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default"),
                              bsButton("batch_effect_plot_next_tab",label = "Next", icon = icon("arrow-right"),
                                       style = "default")
                              
              )),
              br(),              
              # tabsetPanel(
              #   type = "tabs",
               tabBox(width = 12,id = "batch_plot_box",
                      title = "Plot Options",
                      tabPanel("Options",
                               selectInput("color_by_batch", label = "Color Samples By:", choices = c("")),
                               numericInput(inputId = "first_pc_batch",value = 1, label = "First PC", min = 1, max = 100),
                               numericInput(inputId = "second_pc_batch",value = 2, label = "Second PC", min = 1, max = 100)
                      ),
                      tabPanel("Plot",
                               actionButton("plot_batch", "Plot Adjusted matrix"),
                               plotlyOutput("batch_plot", width = "auto", height = "auto")

                      ),
                      
                      tabPanel("Download Plot",
                               radioButtons("plot_batch_down_exten", label = "Choose file extension",
                                            choices = c("PDF (static)" = "pdf", "HTML (Interactive)" = "html")),
                               downloadButton(outputId = "plot_batch_down", label = "Download the plot")
                                                   
                               )
                      )

                # )
              ),
# Compare Plots tab ---------------------------------------------------

      tabItem(tabName = "compare_plots_tab",
              
              fluidRow(column(width = 12, offset = 10,
                              bsButton("compare_plots_previous_tab",label = "Previous", icon = icon("arrow-left"),
                                       style = "default")
                              
              )),
              br(),
              tabBox(width = 12,id = "compare_plots_box",
                     title = "Compare Plots",
                     tabPanel("Plot",
                              actionButton("compare_plots_btn", "Plot Adjusted matrix"),
                              plotlyOutput("compare_plots", width = "auto", height = "auto")
                              
                     ),
                     
                     tabPanel("Download Plot",
                              radioButtons("compare_plots_down_exten", label = "Choose file extension",
                                           choices = c("PDF (static)" = "pdf", "HTML (Interactive)" = "html")),
                              downloadButton(outputId = "compare_plots_down", label = "Download the plot")
                              
                     )
              )
                       
             # shinydashboard::box(width = 12, title = "Batch-Corrected Matrix Plot",
             #                     actionButton("compare_plot", "Compare Plots"),
             #                     plotlyOutput("two_plots", width = "auto", height = "auto"))
              )
          )
      )
)
