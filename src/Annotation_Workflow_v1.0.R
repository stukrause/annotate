# List of libraries to load/install
libs <- c("magick", "terra", "raster", "nngeo", "sf")

# Loop through the libraries and check if they are installed
for (lib in libs) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

# get list of terrestrial images
te <- list.files("./data/1_terrestrial/", pattern = ".*\\.(JPG|jpg|tif|TIF)$", full.names = TRUE)
te

# get list of tree corwn images
cr <- list.files("./data/2_crowns/", pattern = ".*\\.(JPG|jpg|tif|TIF)$", full.names = TRUE)
cr

# get expert annotations for comparison purposes
code_expert <- c(5, 35, 55, 100, 45, 99)

# open plots in separate window
plot.new()
dev.new()

# setup dataframe
df <- data.frame(code = numeric(), 
                 code_expert = numeric(), 
                 image = character(), 
                 polygon = character(), 
                 wkt = character())

for(i in 1:length(te)) {
  # start here for line by line testing. Uncomment i <- 1 to choose list position.
  #i <- 1
  
  # read in terrestrial image using MAgick so we can rotate
  ter <- image_read(te[i])
  
  # rotate image. Terrestrial crown images are often taken in potrait.
  # Check the value of i and rotate the raster accordingly. This is because last 3 images had reversed portrait mode.
  if (i >= 1 & i <= 3) {
    tero <- image_rotate(ter, degrees = 90)
  } else if (i >= 4 & i <= 6) {
    tero <- image_rotate(ter, degrees = 270)
  }
  
  # convert to raster (rast) for terra package
  tera <- as.raster(tero) |> as.matrix() |> rast()

  # plot terrestrial image
  terra::plotRGB(tera, r = 1, g = 2, b = 3, axes = FALSE)

  # prompt user to enter a defoliation assessment code with error handling
  co <- ""
  while (TRUE) {
    co <- readline(prompt = "Please enter a defoliation code (between 0 and 100) and press [enter]: ")
    if (grepl("^\\d+(\\.\\d+)?$", co)) {
      co <- as.numeric(co)
      if (co >= 0 && co <= 100) {
        break
      }
    }
    cat("Invalid input. Please try again.\n")
  }
  
  # import crown raster
  crr <- rast(cr[i])
  
  # get aspect
  #asp <- nrow(crr)/ncol(crr)
  
  # plot crown in rgb
  #terra::plotRGB(crr, r = 1, g = 2, b = 3, axes = FALSE, smooth = TRUE, asp = asp)
  terra::plotRGB(crr, r = 1, g = 2, b = 3, axes = FALSE)
  
  # prompt user for ROI creation
  cat("Draw ROI by clicking the top left side of tree crown and then the bottom right")
  # draw roi
  roi <- sel(crr, use="pol", draw=TRUE, col="cyan")

  # create vector extent from roi
  tp <- as.polygons(ext(roi))
  # create centroid from polygon
  ce <- centroids(tp)
  
  # plot Roi extent as rectangle
  plot(tp, add = TRUE, border = "blue", lwd = 3)
  
  # plot centroid from rectangle
  #plot(ce, add = TRUE, col = "red", lwd = 10)
  
  # determine min and max
  xl <- terra::xmax(tp) - terra::xmin(tp)
  yl <- terra::ymax(tp) - terra::ymin(tp)
  
  # create ellisoid or circle of roi
  to <- st_ellipse(st_as_sf(ce), ex = xl/2, ey = yl/2)

  # plot ellispoid
  plot(to, add = TRUE, border = "red", lwd = 3)
  
  # create directory to save polygons
  dir.create("./data/roi", showWarnings = FALSE)
  
  # write to disk
  writeVector(vect(to), paste0("./data/roi/", i, "_roi_", basename(cr[i]), ".shp"),
             overwrite = TRUE)
  
  # create well known text string
  wkt <- st_as_text(to)
  print(wkt)
  
  # add values to dataframe
  df <- rbind(df, data.frame(code = co, 
                             code_expert = code_expert[i], 
                             image = cr[i], 
                             polygon = paste0(i, "_roi_", basename(cr[i]), ".shp"), 
                             wkt = wkt))
  
  #readline(prompt="Press [enter] to continue")

}

View(df)

# create directory to save output
dir.create("./data/output", showWarnings = FALSE)

# write csv, use ; because wkt has commas
write.csv2(df, "./data/output/results.csv", row.names = FALSE, quote = FALSE)






