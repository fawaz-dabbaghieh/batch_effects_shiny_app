tab_panel_header <- function(tab_panel){
  
  if(tab_panel == "list_experiments_tab"){
    return("List Experiments")
  }else if(tab_panel == "score_matrix_tab"){
    return("Score Matrix")
  }else if(tab_panel == "rna_seq_matrix"){
    return("RNA seq Matrix")
  }else if(tab_panel == "score_matrix_dnase1_tab"){
    return("Score Matrix DNase1")
  }else if(tab_panel == "plot_matrix_tab"){
    return("Plot Matrix")
  }else if(tab_panel == "corr_plot_tab"){
    return("Correlation Plot")
  }else if(tab_panel == "batch_effect_tab"){
    return("Batch Effects")
  }else if(tab_panel == "batch_effect_plot_tab"){
    return("Batch Effect Plot")
  }else if(tab_panel == "compare_plots_tab"){
    return("Compare plots")
  }else{
    return(NULL)
  }
}