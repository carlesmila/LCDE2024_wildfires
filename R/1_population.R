#-----------------------------------------------------------------------------#
#         1. Clean GEOSTAT gridded population data and create rasters         #
#-----------------------------------------------------------------------------#

pathroot <- ""

# 1. Clean ----

## GEOSTAT 2006 ----

# Read polygon grid geometries
pop06_geom <-
  read_sf(paste0(pathroot, "data/raw/population/2006/Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.shp"),
          quiet = TRUE) |>
  rename(GRD_ID = GRD_INSPIR)
# Read population counts
pop06_counts <-
  read_delim(paste0(pathroot, "data/raw/population/2006/GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv"),
             delim=";") |>
  dplyr::select(GRD_ID, POP_TOT) |>
  rename(POP06 = POP_TOT)
# Merge the two sources of information
pop06 <- left_join(pop06_geom, pop06_counts, by = "GRD_ID")
rm("pop06_geom", "pop06_counts")
# To centroids and WGS84
pop06 <- st_centroid(pop06)
pop06 <- st_transform(pop06, crs = 4326)


## GEOSTAT 2011 ----

# Read polygon grid geometries
pop11_geom <-
  read_sf(paste0(pathroot,
                 "data/raw/population/2011/GEOSTATReferenceGrid/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp"),
          quiet = TRUE)
# Read population counts, there may be duplicated if a cell is part of several countries
pop11_counts <- read_csv(paste0(pathroot,
                                "data/raw/population/2011/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv"))  |>
  dplyr::select(GRD_ID, TOT_P) |>
  rename(POP11 = TOT_P)
# Read modelled population counts
pop11_modcounts <- read_csv(paste0(pathroot,
                                   "data/raw/population/2011/JRC-GHSL_AIT-grid-POP_1K_2011.csv"))  |>
  dplyr::select(GRD_ID, TOT_P) |>
  rename(POP11 = TOT_P) |>
  filter(!GRD_ID %in% pop11_counts$GRD_ID) # Remove if available in the non-modelled dataset
pop11_counts <- bind_rows(pop11_counts, pop11_modcounts)
# Merge the two sources of information
pop11 <- left_join(pop11_geom, pop11_counts, by = "GRD_ID")
rm("pop11_geom", "pop11_counts", "pop11_modcounts")
# To centroids and WGS84
pop11 <- st_centroid(pop11)
pop11 <- st_transform(pop11, crs = 4326)


## GEOSTAT 2018 ----

# Read polygon grid geometries with population counts already included
pop18 <- read_sf(paste0(pathroot, "data/raw/population/2018/JRC_POPULATION_2018.shp")) |>
  dplyr::select(GRD_ID, TOT_P_2018) |>
  rename(POP18 = TOT_P_2018)
# To centroids and WGS84
pop18 <- st_centroid(pop18)
pop18 <- st_transform(pop18, crs = 4326)


# 2. Rasterize ----
gridgeom <- rast(paste0(pathroot, "data/raw/SILAM/europePM25fire-2003to2022daymean.nc4"), lyrs = 1)

## GEOSTAT 2006 ----
pop06_raster <- rasterize(vect(pop06), gridgeom, field="POP06", fun=sum, background=0)
names(pop06_raster) <- "population"
time(pop06_raster, tstep="years") <- 2006
rm("pop06")

## GEOSTAT 2011 ----
pop11_raster <- rasterize(vect(pop11), gridgeom, field="POP11", fun=sum, background=0)
names(pop11_raster) <- "population"
time(pop11_raster, tstep="years") <- 2011
rm("pop11")

## GEOSTAT 2018 ----
pop18_raster <- rasterize(vect(pop18), gridgeom, field="POP18", fun=sum, background=0)
names(pop18_raster) <- "population"
time(pop18_raster, tstep="years") <- 2018
rm("pop18")

## GEOSTAT 2006 imputed  ----
# Assign 2011 counts where no data is available in 2006, namely in the Balcans
pop06_raster <- ifel(pop06_raster == 0, pop11_raster, pop06_raster)


# 3. Interpolate and stack ----
popempty <- pop06_raster; values(popempty)<- NA
pop03_raster<- deepcopy(popempty); time(pop03_raster, tstep="years") <- 2003
pop04_raster<- deepcopy(popempty); time(pop04_raster, tstep="years") <- 2004
pop05_raster<- deepcopy(popempty); time(pop05_raster, tstep="years") <- 2005
pop07_raster<- deepcopy(popempty); time(pop07_raster, tstep="years") <- 2007
pop08_raster<- deepcopy(popempty); time(pop08_raster, tstep="years") <- 2008
pop09_raster<- deepcopy(popempty); time(pop09_raster, tstep="years") <- 2009
pop10_raster<- deepcopy(popempty); time(pop10_raster, tstep="years") <- 2010
pop12_raster<- deepcopy(popempty); time(pop12_raster, tstep="years") <- 2012
pop13_raster<- deepcopy(popempty); time(pop13_raster, tstep="years") <- 2013
pop14_raster<- deepcopy(popempty); time(pop14_raster, tstep="years") <- 2014
pop15_raster<- deepcopy(popempty); time(pop15_raster, tstep="years") <- 2015
pop16_raster<- deepcopy(popempty); time(pop16_raster, tstep="years") <- 2016
pop17_raster<- deepcopy(popempty); time(pop17_raster, tstep="years") <- 2017
pop19_raster<- deepcopy(popempty); time(pop19_raster, tstep="years") <- 2019
pop20_raster<- deepcopy(popempty); time(pop20_raster, tstep="years") <- 2020
pop21_raster<- deepcopy(popempty); time(pop21_raster, tstep="years") <- 2021
pop22_raster<- deepcopy(popempty); time(pop22_raster, tstep="years") <- 2022
popall <- c(pop03_raster,pop04_raster,pop05_raster,
            pop06_raster,
            pop07_raster,pop08_raster,pop09_raster,pop10_raster,
            pop11_raster,
            pop12_raster,pop13_raster,pop14_raster,pop15_raster,pop16_raster,pop17_raster,
            pop18_raster,
            pop19_raster,pop20_raster,pop21_raster,pop22_raster)
rm("pop03_raster","pop04_raster","pop05_raster",
   "pop06_raster",
   "pop07_raster","pop08_raster","pop09_raster","pop10_raster",
   "pop11_raster",
   "pop12_raster","pop13_raster","pop14_raster","pop15_raster","pop16_raster","pop17_raster",
   "pop18_raster",
   "pop19_raster","pop20_raster","pop21_raster","pop22_raster")
popall <- approximate(popall, method = "linear", rule = 2)
# write_rds(popall, paste0(pathroot, "data/processed/population_yearly.rds"))
# writeRaster(popall, paste0(pathroot, "data/processed/population_yearly.tif"))

# Clean
rm(list=ls())
