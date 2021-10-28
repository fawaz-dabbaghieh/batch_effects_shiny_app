plotly_pca <- function(experiments_info_meta,
                       filtered_score_matrix,
                       name,
                       project,
                       type_of_score,
                       color_by = "biosource_name",
                       epigenetic_mark = "No Epigenetic mark selected",
                       first_pc="1",
                       second_pc="2",
                       show_legend = TRUE){
  
  if (epigenetic_mark == "Gene Expression"){
    experiments_info_meta$experiment <- colnames(filtered_score_matrix)
  }
  
  #calculating PCA
  pca <- prcomp(filtered_score_matrix, center = TRUE, scale. = TRUE)
  
  #Checking if PC chosen is available or not
  validate(
    need(paste0("PC", first_pc) %in% colnames(pca[['rotation']]),
         message = paste("The PC you chose is out of bound","upper bound is",
                         colnames(pca[['rotation']])[length(colnames(pca[['rotation']]))])),
    
    need(paste0("PC", second_pc) %in% colnames(pca[['rotation']]),
         message = paste("The PC you chose is out of bound","upper bound is",
                         colnames(pca[['rotation']])[length(colnames(pca[['rotation']]))]))
  )
  
  #preparing the plot data by taking the PCAs and adding metadata
  plot.data <- as.data.frame(pca$rotation) %>%
    tibble::rownames_to_column(var = "experiment") %>%
    dplyr::left_join(experiments_info_meta, by=c("experiment"))
  
  # #Getting colour pallet
  # colourCount <- 9
  # getPalette <- colorRampPalette(brewer.pal(colourCount, "Set1"))
  
  # if((input$project == "TEPIC reprocessed IHEC data") &
  #    (input$epigenetic_mark == "Gene Expression")){
  #   
  # }
  if(project == "DEEP"){
    label <- "DEEP_SAMPLE_ID"
    hover = ~paste("Sample ID: ", DEEP_SAMPLE_ID,
                   '</br>Biosource Name: ', biosource_name)
  }else{
    label <- "experiment"
    hover = ~paste("Experiment: ", experiment,
                   '</br>Biosource Name: ', biosource_name)
  }
  
  x_lab <- paste0(paste0("PC", first_pc," ", "("),
                  round(pca$sdev[as.integer(first_pc)]^2/sum(pca$sdev^2), 2) * 100, "%)")
  
  y_lab <- paste0(paste0("PC", second_pc," ", "("),
                  round(pca$sdev[as.integer(second_pc)]^2/sum(pca$sdev^2), 2) * 100, "%)")
  
  p <-
    plot.data %>%
    arrange(plot.data[,color_by]) %>%
    plot_ly(x = as.formula(paste0("~","PC", first_pc)),
            text = hover,
            color = as.formula(paste0("~",color_by)),
            legendgroup = as.formula(paste0("~",color_by)),
            colors = brewer.pal(9, "Set1"),
            marker = list(size = 17.5)) %>%
    add_markers(y = as.formula(paste0("~","PC", second_pc)), showlegend = show_legend) %>%
      layout(title = paste("PC plot", name, type_of_score, epigenetic_mark),
             yaxis = list(title = y_lab, zeroline = FALSE),
             xaxis = list(title = x_lab, zeroline = FALSE))
  
  return(p)
  
}