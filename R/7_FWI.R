#-----------------------------------------------------------------------------#
#                           7. FWI indicators                                 #
#-----------------------------------------------------------------------------#

pathroot <- ""

# exposure blueprint
exposure <- rast(paste0(pathroot, "data/raw/SILAM/europePM25fire-2003to2022daymean.nc4"))[[1]]

# raw FWI file, global 0.5º resolution hourly
danger <- rast(paste0(pathroot, "data/raw/FWI/fireWeather-1979to2022-Glob05deg_daylight_correction.nc4"))
dangertimes <- year(time(danger))

# extraction points
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
dfnuts <- as.data.frame(pointnuts)

# population data
population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
names(population) <- as.character(2003:2022)

# For each year, crop, rotate and average
allFWI_nuts2 <- list()
allFWI_country <- list()
allFWI_region <- list()
allFWI_euro <- list()

for(y in 1980:2022){

  print(y)

  # Read annual data, crop and average
  yFWI <- rast(paste0(pathroot, "data/raw/FWI/fireWeather-1979to2022-Glob05deg_daylight_correction.nc4"),
                     lyrs = dangertimes == y)
  yFWI <- rotate(yFWI)
  crs(yFWI)  <- "epsg:4326"
  yFWI <- crop(yFWI, exposure)
  yFWI <- app(yFWI, "mean")

  # Resample at the exposure geometries, extract
  yFWI <- resample(yFWI, exposure, method = "bilinear")
  names(yFWI) <- "FWI"
  yFWI <- terra::extract(yFWI, pointnuts)[-1]

  # extract population
  ypop <- population[as.character(max(c(y, 2003)))]
  names(ypop) <- "population"
  ypop <- terra::extract(ypop, pointnuts)[-1]

  # Merge
  ydata <- cbind(dfnuts, ypop, yFWI)
  ydata <- setDT(ydata)

  # Population and spatial averages: NUTS 2
  FWI_nuts2 <- copy(ydata)
  FWI_nuts2[, popw := population/sum(population), by = .(NUTS_2)]
  FWI_nuts2 <- FWI_nuts2[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                           by = .(NUTS_2)]
  FWI_nuts2$year <- y
  setDF(FWI_nuts2)

  # Population and spatial averages: country
  FWI_country <- copy(ydata)
  FWI_country[, popw := population/sum(population), by = .(NUTS_0)]
  FWI_country <- FWI_country[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                         by = .(NUTS_0)]
  FWI_country$year <- y
  setDF(FWI_country)

  # Population and spatial averages: region
  FWI_region <- copy(ydata)
  FWI_region[, popw := population/sum(population), by = .(region)]
  FWI_region <- FWI_region[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                         by = .(region)]
  FWI_region$year <- y
  setDF(FWI_region)

  # Population and spatial averages: Europe
  FWI_euro <- copy(ydata)
  FWI_euro[, popw := population/sum(population)]
  FWI_euro <- FWI_euro[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI))]
  FWI_euro$year <- y
  setDF(FWI_euro)

  # Store and clean
  allFWI_nuts2[[as.character(y)]] <- FWI_nuts2
  allFWI_country[[as.character(y)]] <- FWI_country
  allFWI_region[[as.character(y)]] <- FWI_region
  allFWI_euro[[as.character(y)]] <- FWI_euro
  rm("FWI_nuts2", "FWI_country", "FWI_region", "FWI_euro",
     "ypop", "ydata", "yFWI")

}

# Save to disk
allFWI_nuts2 <- do.call(rbind, allFWI_nuts2)
write_csv(allFWI_nuts2, "data/processed/FWI_nuts2.csv")
allFWI_country <- do.call(rbind, allFWI_country)
write_csv(allFWI_country, "data/processed/FWI_country.csv")
allFWI_region <- do.call(rbind, allFWI_region)
write_csv(allFWI_region, "data/processed/FWI_region.csv")
allFWI_euro <- do.call(rbind, allFWI_euro)
write_csv(allFWI_euro, "data/processed/FWI_euro.csv")
rm(list = ls())
