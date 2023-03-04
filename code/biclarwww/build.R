# Build the website 


# Main pages ----------------------------------------------------------------------------------


# página principal
rmarkdown::render(input = "code/biclarwww/index.Rmd", encoding = "UTF-8", output_dir = "biclarwww", output_file = "index.html") #run this


# página da AML - demora cerca de 5min a render. Agora sem 2 mapas é rápido (1min)
rmarkdown::render(input = "code/biclarwww/aml_index.Rmd", encoding = "UTF-8", output_dir = "biclarwww/aml", output_file = "index.html") #run this



# Create outputs for each region --------------------------------------------------------------

# i = "oeiras"
# i = "barreiro"
# i = "lisboa"
# i = "odivelas"
# i = "setubal"

# for (i in municipios){

for (i in municipios){
  
  # dir.create(pub_folder) #comment this one after run first time
  
  if (i == "vila franca de xira"){
    pub_folder = paste0("biclarwww/", "vfxira")
      } else {
    pub_folder = paste0("biclarwww/", i) #this needs to be updated for each municipio
      }
  
  if (i %in% c("alcochete", "mafra", "montijo", "odivelas")){
    rmarkdown::render(input = "code/biclarwww/municipio_minor_index.Rmd", encoding = "UTF-8", output_dir = pub_folder, output_file = "index.html")
    
  } else {
  rmarkdown::render(input = "code/biclarwww/municipio_index.Rmd", encoding = "UTF-8", output_dir = pub_folder, output_file = "index.html")
  }
  
  print(i)
}


