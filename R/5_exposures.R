#-----------------------------------------------------------------------------#
#                    5. Wildfire-PM2.5 exposure indicators                    #
#-----------------------------------------------------------------------------#

pathroot <- "HPCpath"

# 1. Exposure Function ----
expo_aggr <- function(expofile, yearint){

  # Read data + check
  expodata <- fread(expofile)
  if(any(tapply(expodata$population, expodata$NUTS_2, sum) == 0)){
    stop("Some NUTS have a population of 0. Please revise the data.")
  }

  # Yearly exposure and risk by NUTS 2, population-weighted + spatial average
  expo_nuts2 <- copy(expodata)
  expo_nuts2[, popw := population/sum(population), by = .(NUTS_2, date)]
  expo_nuts2 <- expo_nuts2[, .(pm25_pop = sum(popw*pm25), pm25_spatial = mean(pm25)),
                           by = .(NUTS_2, date)]
  expo_nuts2 <- expo_nuts2[, .(pm25_pop = mean(pm25_pop), pm25_spatial = mean(pm25_spatial)),
                           by = .(NUTS_2)]
  expo_nuts2$year <- yearint
  setDF(expo_nuts2)

  # Yearly exposure and risk by country, population-weighted + spatial average
  expo_country <- copy(expodata)
  expo_country[, popw := population/sum(population), by = .(NUTS_0, date)]
  expo_country <- expo_country[, .(pm25_pop = sum(popw*pm25), pm25_spatial = mean(pm25)),
                               by = .(NUTS_0, date)]
  expo_country <- expo_country[, .(pm25_pop = mean(pm25_pop), pm25_spatial = mean(pm25_spatial)),
                               by = .(NUTS_0)]
  expo_country$year <- yearint
  setDF(expo_country)

  # Yearly exposure and risk by region, population-weighted + spatial average
  expo_region <- copy(expodata)
  expo_region[, popw := population/sum(population), by = .(region, date)]
  expo_region <- expo_region[, .(pm25_pop = sum(popw*pm25), pm25_spatial = mean(pm25)),
                               by = .(region, date)]
  expo_region <- expo_region[, .(pm25_pop = mean(pm25_pop), pm25_spatial = mean(pm25_spatial)),
                               by = .(region)]
  expo_region$year <- yearint
  setDF(expo_region)

  # Europe-wide early exposure and risk, population-weighted + spatial average
  expo_euro <- copy(expodata)
  expo_euro[, popw := population/sum(population), by = .(date)]
  expo_euro <- expo_euro[, .(pm25_pop = sum(popw*pm25), pm25_spatial = mean(pm25)),
                             by = .(date)]
  expo_euro <- expo_euro[, .(pm25_pop = mean(pm25_pop), pm25_spatial = mean(pm25_spatial))]
  expo_euro$year <- yearint
  setDF(expo_euro)

  # Return data
  list("expo_nuts2" = expo_nuts2,
       "expo_country" = expo_country,
       "expo_region" = expo_region,
       "expo_euro" = expo_euro)
}


# 2. Execute  ----
expo03 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2003.csv"), 2003)
expo04 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2004.csv"), 2004)
expo05 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2005.csv"), 2005)
expo06 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2006.csv"), 2006)
expo07 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2007.csv"), 2007)
expo08 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2008.csv"), 2008)
expo09 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2009.csv"), 2009)
expo10 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2010.csv"), 2010)
expo11 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2011.csv"), 2011)
expo12 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2012.csv"), 2012)
expo13 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2013.csv"), 2013)
expo14 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2014.csv"), 2014)
expo15 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2015.csv"), 2015)
expo16 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2016.csv"), 2016)
expo17 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2017.csv"), 2017)
expo18 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2018.csv"), 2018)
expo19 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2019.csv"), 2019)
expo20 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2020.csv"), 2020)
expo21 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2021.csv"), 2021)
expo22 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2022.csv"), 2022)

# Yearly exposure and risk by NUTS 2, population-weighted + spatial average
expo_nuts2 <- bind_rows(expo03[[1]], expo04[[1]], expo05[[1]], expo06[[1]], expo07[[1]],
                        expo08[[1]], expo09[[1]], expo10[[1]], expo11[[1]], expo12[[1]],
                        expo13[[1]], expo14[[1]], expo15[[1]], expo16[[1]], expo17[[1]],
                        expo18[[1]], expo19[[1]], expo20[[1]], expo21[[1]], expo22[[1]])
write_csv(expo_nuts2, paste0(pathroot, "data/processed/pm25_nuts2.csv"))

# Yearly exposure and risk by country, population-weighted + spatial average
expo_country <- bind_rows(expo03[[2]], expo04[[2]], expo05[[2]], expo06[[2]], expo07[[2]],
                          expo08[[2]], expo09[[2]], expo10[[2]], expo11[[2]], expo12[[2]],
                          expo13[[2]], expo14[[2]], expo15[[2]], expo16[[2]], expo17[[2]],
                          expo18[[2]], expo19[[2]], expo20[[2]], expo21[[2]], expo22[[2]])
write_csv(expo_country, paste0(pathroot, "data/processed/pm25_country.csv"))

# Yearly exposure and risk by European region, population-weighted + spatial average
expo_region <- bind_rows(expo03[[3]], expo04[[3]], expo05[[3]], expo06[[3]], expo07[[3]],
                         expo08[[3]], expo09[[3]], expo10[[3]], expo11[[3]], expo12[[3]],
                         expo13[[3]], expo14[[3]], expo15[[3]], expo16[[3]], expo17[[3]],
                         expo18[[3]], expo19[[3]], expo20[[3]], expo21[[3]], expo22[[3]])
write_csv(expo_region, paste0(pathroot, "data/processed/pm25_region.csv"))

# Europe-wide yearly exposure and risk, population-weighted + spatial average
expo_euro <- bind_rows(expo03[[4]], expo04[[4]], expo05[[4]], expo06[[4]], expo07[[4]],
                       expo08[[4]], expo09[[4]], expo10[[4]], expo11[[4]], expo12[[4]],
                       expo13[[4]], expo14[[4]], expo15[[4]], expo16[[4]], expo17[[4]],
                       expo18[[4]], expo19[[4]], expo20[[4]], expo21[[4]], expo22[[4]])
write_csv(expo_euro, paste0(pathroot, "data/processed/pm25_euro.csv"))
