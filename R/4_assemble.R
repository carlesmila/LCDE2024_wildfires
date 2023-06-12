#-----------------------------------------------------------------------------#
#                          4. Extract daily exposures                         #
#-----------------------------------------------------------------------------#

pathroot <- "HPCpath"

pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
dfnuts <- as.data.frame(pointnuts)
dayseq <- seq.Date(as.Date("2003-01-01"), as.Date("2022-12-31"), by = "1 day")
population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
names(population) <- as.character(2003:2022)

# Extract geographical IDs, population, SILAM for each day-cell per year
for(y in 2003:2022){

  print(y)

  # Extract population ----
  ypop <- population[as.character(y)]
  names(ypop) <- "population"
  ypop <- terra::extract(ypop, pointnuts)[-1]

  # Extract SILAM ----
  yindex <- year(dayseq) == y
  ysilam <- rast(paste0(pathroot, "data/raw/SILAM/europePM25fire-2003to2022daymean.nc4"), lyrs = yindex)
  names(ysilam) <- as.character(as.Date(time(ysilam)))
  ysilam <- ysilam*1e+9 # change of units
  ysilam <- terra::extract(ysilam, pointnuts)[-1]

  # Merge, reformat and write----
  ydata <- cbind(dfnuts, ypop, ysilam)
  ydata <- tidyr::pivot_longer(ydata,
                               cols = -c("GRD_ID","NUTS_0","NUTS_2","NUTS_mort","region","population"),
                               names_to = "date", values_to = "pm25")
  readr::write_csv(ydata, paste0(pathroot, "data/processed/assembled/data_", y,".csv"))
  rm("ypop", "yindex", "ysilam", "ydata")
}
rm(list = ls())
