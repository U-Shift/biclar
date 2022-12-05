# Build the website 


# Read in data --------------------------------------------------------------------------------


# página principal
rmarkdown::render(input = "code/biclarwww/index.Rmd", output_dir = "biclarwww", output_file = "index.html") #run this


# página da AML - demora cerca de 5min a render. Agora sem 2 mapas é rápido (1min)
rmarkdown::render(input = "code/biclarwww/aml_index.Rmd", output_dir = "biclarwww/aml", output_file = "index.html") #run this



# Create outputs for each region --------------------------------------------------------------



i = "barreiro"

for (i in municipios){
  
  if (i = "vila franca de xira"){
    pub_folder = paste0("biclarwww/", "vfxira")
      } else {
    pub_folder = paste0("biclarwww/", i) #this needs to be updated for each municipio
  }
  # dir.create(pub_folder) #comment this one??
  rmarkdown::render(input = "code/biclarwww/municipio_index.Rmd", output_dir = pub_folder, output_file = "index.html")
  # rmarkdown::render(input = "stats.Rmd", output_dir = pub_folder, output_file = "stats.html")
  # rmarkdown::render(input = "baseline.Rmd", output_dir = pub_folder, output_file = "baseline.html")
  # rmarkdown::render(input = "route-types.Rmd", output_dir = pub_folder, output_file = "route-types.html")
  # rmarkdown::render(input = "downloads.Rmd", output_dir = pub_folder, output_file = "downloads.html")
  
}


