inserted_chr_list <- function(user_key, id_genome, genomes){
  insertUI(selector = '#addChromosomes',
           where = "afterEnd",
           ui = tags$div(
             selectInput('chr', 'Select a Chromosome, or nothign for the whole genome',
                         c("",deepblue_chromosomes(genome = input$genome, user_key = user_key)),selected = "",
                         multiple = TRUE),
             id = id_genome
           )
  )
  return(id_genome)
}