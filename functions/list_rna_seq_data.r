list_rna_seq_data <- function(project,
                              user_key){
  
  protein_coding_genes_id <- readLines(file.path("..","gene_ids","protein_coding_HAVANA_genes_ids.txt"))
  
  gene_expr_query <- deepblue_select_expressions(
    expression_type = "gene",
    gene_model = "gencode v23",
    identifiers = protein_coding_genes_id,
    project = project,
    user_key = user_key)
  
  gene_expr_request_id <- deepblue_get_regions(
    query_id = gene_expr_query,
    output_format ="CHROMOSOME,START,END,@STRAND(gencode v23),@GENE_NAME(gencode v23),@SAMPLE_ID,TPM,FPKM",
    user_key = user_key)
  
  incProgress(amount = 1, message = "Listing all experiments")
  
  while(deepblue_info(gene_expr_request_id, user_key = user_key)$state == "running"){
    Sys.sleep(10)
  }
  
  gene_expr_regions <- deepblue_download_request_data(gene_expr_request_id, 
                                                      user_key = user_key)
  
  ##I need to check heree if FPKM or TPM that should be used then drop one
  
  incProgress(amount = 2, message = "Building Matrix")
  
  gene_expr_df <- as.data.frame(gene_expr_regions) %>% select(-FPKM) %>%
    spread(key = `X.SAMPLE_ID`, value = "TPM")
  
  sample_cols <- grep(colnames(gene_expr_df), pattern = "s[0-9].", value = TRUE)
  
  #I'm getting each sample as one line and all joined alread, didn't need to process them
  sample_info <- as.data.frame(deepblue_info(sample_cols, user_key = user_key))
  sample_info <- sample_info[,-grep("id",colnames(sample_info))]
  sample_info[,"SAMPLE_ID"] <- sample_cols
  
  gene_expr_matrix <- data.matrix(gene_expr_df[,sample_cols])
  rownames(gene_expr_matrix) <- gene_expr_df$X.GENE_NAME.gencode.v23.
  
  if(project == "TEPIC reprocessed IHEC data"){
    experiments <- tepic_sample_id_mapping(sample_cols = sample_cols,
                                           user_key = user_key)
  }
  
  attr(gene_expr_matrix, "meta") <- deepblue_info(sample_cols,
                                                  user_key = user_key)
  all_experiments <- list(experiments, gene_expr_matrix)
  incProgress(amount = 0, message = "Done!!")
  
  return(all_experiments)
}