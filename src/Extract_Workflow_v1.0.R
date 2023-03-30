# List of libraries to load/install
libs <- c("terra", "raster", "rgeos")

# Loop through the libraries and check if they are installed
for (lib in libs) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

csv <- read.csv2("./data/output/results.csv")
names(csv)

cr <- list.files("./data/2_crowns/", pattern = ".*\\.(JPG|jpg|tif|TIF)$", full.names = TRUE)
cr

# setup dataframe
df <- data.frame(code = numeric(), 
                 code_expert = numeric(), 
                 image = character(), 
                 polygon = character(), 
                 wkt = character(),
                 red = numeric(), 
                 green = numeric(), 
                 blue = numeric())

for (i in 1:length(cr)) {
  #i <- 1
  # read in polygon
  #poly <- vect(paste0("./data/roi/", i, "_roi_", sub('.\\(JPG|jpg|tif|TIF)$', '', basename(cr[i])), ".shp"))

  # or read in WKT
  poly_wkt <- readWKT(csv$wkt[i])
  poly <- vect(poly_wkt)
  
  # plot to check polygon
  terra::plotRGB(rast(cr[i]), r = 1, g = 2, b = 3, axes = FALSE)
  plot(poly, add = TRUE, border = "red", lwd = 3)
  
  # down sample
  rta <- terra::aggregate(rast(cr[i]), fact = 100, fun = mean)
  rta
  pix <- terra::extract(rta, poly)
  nrow(pix)
  names(pix) <- c("ID", "red", "green", "blue")
  
  df <- rbind(df, data.frame(code = csv$code[i], 
                             code_expert = csv$code_expert[i], 
                             image = csv$image[i], 
                             polygon = csv$polygon[i], 
                             wkt = csv$wkt[i], 
                             red = pix[2], 
                             green = pix[3],
                             blue = pix[4]))
  
}

View(df)
df$red <- as.integer(df$red)
df$green <- as.integer(df$green)
df$blue <- as.integer(df$red)

write.csv2(df, "./data/output/training.csv", row.names = FALSE, quote = FALSE)
