ruv_batch_effect <- function(filtered_score_matrix,
                             experiments,
                             outliers,
                             project,
                             regularization_par = 0.00001,
                             quantile_prob,
                             k_rank = 5,
                             estimate_hkg){
  
  #hkg: House Keeping Genes, estimate_hkg = T or F
  #regularization_par nu.coeff for unwanted variation for the naiveRandRUV()
  #k_rank Desired rank for the estimated unwanted variation term for naiveRandRUV()
  #estimate_hkg if TRUE, hkg will be defined by a rank analysis
    #if FALSE a list of hkg will be used
    #quantile_prob used as probability for the quantile() to estimate hkg
  metadata <- as.data.frame(attr(filtered_score_matrix, "meta"))
  
  if(project == "TEPIC reprocessed IHEC data"){
    outliers <- unique(c(outliers,"R_ENCBS150HBC_ENCBS376RZJ",
                         "R_ENCBS559QNR_ENCBS568FYY_ENCBS945MCY",
                         "R_ENCBS853LFM","E_520VFV"))
  }
  
  filtered_score_matrix <- filtered_score_matrix[,!(colnames(filtered_score_matrix) %in% outliers)]
  metadata <- metadata[!(colnames(filtered_score_matrix) %in% outliers),]
  
  if(estimate_hkg == "use_hkg_list"){
    # housekeepingGenes<-c("ENSG00000204574","ENSG00000075624","ENSG00000023330",
    #                      "ENSG00000166710","ENSG00000141367","ENSG00000160211",
    #                      "ENSG00000111640","ENSG00000169919","ENSG00000165704",
    #                      "ENSG00000134333","ENSG00000102144","ENSG00000125630",
    #                      "ENSG00000181222","ENSG00000108298","ENSG00000089157",
    #                      "ENSG00000073578","ENSG00000112592","ENSG00000196230")
    
    housekeepingGenes <- c("ABCF1", "ACTB", "ALAS1", "B2M","CLTC","G6PD","GAPDH",
                           "GUSB","HPRT1","LDHA","PGK1","POLR1B","POLR2A",
                           "RPL19", "RPLP0","SDHA","TBP","TUBB")
    
    rowsum<-apply(filtered_score_matrix,1,sum)
    filtered_score_matrix <- filtered_score_matrix[!(rowsum == 0),]
    
    # if(!length(which(rowsum == 0)) == 0){
    #   filtered_score_matrix<-filtered_score_matrix[-which(rowsum==0),]
    # }
    
    # if(outliers != ""){
    #   filtered_score_matrix <- filtered_score_matrix[,-match(outliers,colnames(filtered_score_matrix))]
    #   metadata <- metadata[-match(outliers, metadata["experiment"]),]
    #   
    # }   
    
    inRUV<-t(as.matrix(log2(filtered_score_matrix+1)))
    
    ruv_adjusted_matrix <- naiveRandRUV(inRUV,which(housekeepingGenes %in% colnames(inRUV)),
                                        nu.coeff=regularization_par,k=k_rank)
    
    #esetimate houskeeping genes
    
  }else if(estimate_hkg == "estimate_hkg"){
    
    rowsum<-apply(filtered_score_matrix,1,sum)
    filtered_score_matrix <- filtered_score_matrix[!(rowsum == 0),]
    
    # if(!length(which(rowsum == 0)) == 0){
    #   filtered_score_matrix<-filtered_score_matrix[-which(rowsum==0),]
    # }
    
    # if(outliers != ""){
    #   filtered_score_matrix <- filtered_score_matrix[,-match(outliers,colnames(filtered_score_matrix))]
    #   metadata <- metadata[-match(outliers, metadata["experiment"]),]
    #   
    # }
    
    rankedData<-apply(filtered_score_matrix,2,rank)
    stdRanks<-apply(rankedData,1,sd)
    
    meanRanks<-apply(rankedData,1,mean)
    
    if(project != "TEPIC reprocessed IHEC data"){
      housekeepingGenes <- match(meanRanks[stdRanks <= quantile(stdRanks,quantile_prob)],
                                 meanRanks)
      inRUV<-t(as.matrix(log2(filtered_score_matrix+1)))
      
    }else{
      inRUV<-t(as.matrix(log2(filtered_score_matrix+1)))
      
      # housekeepingGenes <- names(meanRanks[which(stdRanks <= quantile(stdRanks,quantile_prob))])
      housekeepingGenes <- names(meanRanks[stdRanks <= quantile(stdRanks, quantile_prob)])
      housekeepingGenes <- match(housekeepingGenes, colnames(inRUV))
    }

    ruv_adjusted_matrix <-naiveRandRUV(inRUV,housekeepingGenes,
                                       nu.coeff=regularization_par , k=k_rank)
  }
  
  
  ruv_adjusted_matrix <- t(ruv_adjusted_matrix)
  attr(ruv_adjusted_matrix, "meta") <- metadata
  
  return(ruv_adjusted_matrix)
}