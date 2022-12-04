# Build the website 


# Read in data --------------------------------------------------------------------------------







# Create outputs for each region --------------------------------------------------------------


rmarkdown::render(input = "code/biclarwww/index.Rmd", output_dir = "biclarwww", output_file = "index.html") #run this


# code to build the site

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

