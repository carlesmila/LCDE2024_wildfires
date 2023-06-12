#-----------------------------------------------------------------------------#
#                       2. Clean NUTS and regional data                       #
#-----------------------------------------------------------------------------#

pathroot <- ""


# 1. Read and split by NUTS level ----

# Warning: There are no NUTS codes for Bosnia and Herzegobina (BA) or Kosovo (XK)
nuts <- read_sf(paste0(pathroot, "data/raw/boundaries/NUTS_RG_01M_2021_3035.geojson")) |>
  dplyr::select(NUTS_ID, LEVL_CODE) |>
  st_transform(crs = 4326)
nuts_0 <- dplyr::filter(nuts, LEVL_CODE == 0) |>
  rename(NUTS_0 = NUTS_ID)|>
  dplyr::select(NUTS_0)
nuts_1 <- dplyr::filter(nuts, LEVL_CODE == 1) |>
  rename(NUTS_1 = NUTS_ID)|>
  dplyr::select(NUTS_1)
nuts_2 <- dplyr::filter(nuts, LEVL_CODE == 2) |>
  rename(NUTS_2 = NUTS_ID) |>
  dplyr::select(NUTS_2)
rm("nuts")


# 2. Merge with grid ----

# Exposure boundaries
gridgeom <- rast(paste0(pathroot, "data/raw/SILAM/europePM25fire-2003to2022daymean.nc4"), lyrs = 1)
expo_polys <- st_as_sf(as.polygons(gridgeom, trunc = FALSE, dissolve = FALSE)) |>
  mutate(GRD_ID = 1:n()) |>
  dplyr::select(GRD_ID)
expo_points <- st_centroid(expo_polys)

# Extract NUTS codes at the exposure grid geometries by largest overlap
add_nuts <- function(expolys, nutspolys){
  # st_join(largest = T) is not efficient, we can substantially speed it up by
  # applying it only when necessary (more than one intersection)
  expolys_nuts <- st_join(expolys, nutspolys)
  expolys_1 <- expolys_nuts[!expolys_nuts$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- expolys[expolys$GRD_ID %in% expolys_nuts$GRD_ID[duplicated(expolys_nuts$GRD_ID)],]
  expolys_2 <- st_join(expolys_2, nutspolys, largest = T)
  expolys_nuts <- rbind(expolys_1, expolys_2) |>
    arrange(GRD_ID)
  expolys_nuts
}
expo_points$NUTS_2 <- add_nuts(expo_polys, nuts_2)$NUTS_2
expo_points <- expo_points[!is.na(expo_points$NUTS_2),]
expo_points <- group_by(expo_points, NUTS_2) |>
  mutate(NUTS_0 = substr(NUTS_2, 1, 2),
         NUTS_1 = substr(NUTS_2, 1, 3)) |>
  ungroup()


# 3. Add region IDs ----
euroregions <- read_excel("data/raw/regions/regions.xlsx", skip = 1) |>
  select(4, 9)
euroregions <- euroregions[complete.cases(euroregions),]
# Change cyprus to Southern Europe
names(euroregions) <- c("NUTS_0", "region")
euroregions$region[euroregions$NUTS_0=="CY"] <- "Southern Europe"
expo_points <- left_join(expo_points, euroregions, by = "NUTS_0")


# 4. Filter regions and mortality ID ----

# Turkey (TR) and Jan Mayen and Svalbard (NO0B) - no population data
# Madeira (PT30) - no Atlantic islands
expo_points <- expo_points[expo_points$NUTS_0 != "TR" & !expo_points$NUTS_2 %in% c("NO0B", "PT30"),]

# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) - mortality at the NUTS1 level
expo_points$NUTS_mort <- ifelse(expo_points$NUTS_0 %in% c("DE", "IE", "HR", "SI"),
                                expo_points$NUTS_1, expo_points$NUTS_2)

# We don't need NUTS_1 anymore
expo_points$NUTS_1 <- NULL

# 5. Write ----
# write_sf(expo_points, paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
# rm(list = ls())
