rna_seq_data <- function(project,
                         experiments_info_meta,
                         user_key){
  
  all_genes <- readr::read_tsv("../genecode_v23_genes.tsv")
  protein_coding_genes <- all_genes[all_genes$gene_type == "protein_coding" &
                                      all_genes$source == "HAVANA",]
  protein_coding_genes_id <- protein_coding_genes$gene_id
  
  if(project == "TEPIC reprocessed IHEC data"){
    output_format <- "CHROMOSOME,START,END,@STRAND(gencode v23),@GENE_NAME(gencode v23),@SAMPLE_ID,TPM"
  }else if(project == "DEEP"){
    output_format <- "CHROMOSOME,START,END,@STRAND(gencode v23),@GENE_NAME(gencode v23),@SAMPLE_ID,FPKM"
  }

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
  
  while(deepblue_info(gene_expr_request_id)$state == "running"){
    Sys.sleep(10)
  }
  
  gene_expr_regions <- deepblue_download_request_data(gene_expr_request_id)
  
  gene_expr_df <- as.data.frame(gene_expr_regions) %>%
    spread(key = `X.SAMPLE_ID`, value = TPM)
  
  sample_cols <- grep(colnames(gene_expr_df), pattern = "s[0-9].", value = TRUE)
  gene_expr_matrix <- data.matrix(gene_expr_df[,sample_cols])
  rownames(gene_expr_matrix) <- gene_expr_df$X.GENE_NAME.gencode.v23.
  
  attr(gene_expr_df, "meta") <- experiments_info_meta
  return(gene_expr_df)
}