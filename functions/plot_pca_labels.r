#Plotting the score matrix with the relevant metadata with it

plot_pca_labels <- function(experiments_info_meta,
                            filtered_score_matrix,
                            project,
                            color_by = "biosource_name",
                            first_pc="1", second_pc="2",
                            epigenetic_mark = "Not Selected",
                            legend_position = "right",
                            legend_direction = "vertical",
                            show_legend = TRUE){
  
  #calculating PCA
  pca <- prcomp(filtered_score_matrix, center = TRUE, scale. = TRUE)
  
  #preparing the plot data by taking the PCAs and adding metadata
  plot.data <- as.data.frame(pca$rotation) %>%
    tibble::rownames_to_column(var = "experiment") %>%
    dplyr::left_join(experiments_info_meta, by=c("experiment"))
  
  #Getting colour pallet
  colourCount <- 9
  getPalette <- colorRampPalette(brewer.pal(colourCount, "Set1"))
  
  if(project == "DEEP"){
    label <- "DEEP_SAMPLE_ID"
  }else{
    label <- "experiment"
  }
  
  plot <- ggplot(plot.data, aes_string(x = paste0("PC",first_pc),
                               y = paste0("PC",second_pc),
                               label = label,
                               color = color_by))+
    geom_point(size = 5, alpha = 0.5)+
    # geom_label_repel(data = plot.data)+
    theme_bw() +
    ggtitle(paste("Tiling Regions", epigenetic_mark)) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(legend.position = legend_position,legend.direction = legend_direction,
          legend.box.just = "right",
          legend.justification = c("right", "top"))+
    xlab(paste(paste0("PC", first_pc," ", "("), round(pca$sdev[as.integer(first_pc)]^2/sum(pca$sdev^2), 2) * 100, "%)")) +
    ylab(paste(paste0("PC", second_pc," ", "("), round(pca$sdev[as.integer(second_pc)]^2/sum(pca$sdev^2), 2) * 100, "%)"))
  
  if(show_legend){
    return(plot)
  }
  return(plot + theme(legend.position = "none"))
}