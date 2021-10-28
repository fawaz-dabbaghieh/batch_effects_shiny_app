#function to hperlink the experiments ID in table
#And show the modal with metadata

on_click_js <- "Shiny.onInputChange('exp_id', '%s');
  $('#mtExperID').modal('show')" 

convert_to_link = function(x) {
  as.character(tags$a(href = "#", onclick = sprintf(on_click_js,x), x))
}