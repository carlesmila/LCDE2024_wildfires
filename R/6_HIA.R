#-----------------------------------------------------------------------------#
#                  6. Attributable mortality indicators (main)                #
#-----------------------------------------------------------------------------#

library("lubridate")
library("tidyverse")
library("data.table")

pathroot <- "HPCpath"

# 1. RRs Orellano et al. (2020) ----
set.seed(1234)
point_RR10 <- 1.0065
nsim <- 200
sim_RR10 <- exp(rnorm(nsim, mean = log(point_RR10), sd = log(1.0086/1.0044)/(qnorm(0.975)*2)))
names(sim_RR10) <- paste0("RR10_", str_pad(1:nsim, 3, "left", "0"))

# 2. Mortality ----
mortality <- fread(paste0(pathroot, "data/processed/mortality.csv"))
mortality <- mortality[,.(NUTS_mort, date, year, deaths)]

# 3. HIA Function ----
hia <- function(expofile, pRR10 = point_RR10, sRR10 = sim_RR10, mort = mortality){

  # Read exposure data, delete cells with no population
  expodata <- read_csv(expofile)
  expodata <- expodata[c("NUTS_mort", "NUTS_0", "region", "date", "population", "pm25")]
  expodata <- expodata[expodata$population > 0,]

  # Checks
  if(any(tapply(expodata$population, expodata$NUTS_mort, sum) == 0)){
    stop("Some NUTS have a population of 0. Please revise the data.")
  }
  if(any(!mort$NUTS_mort %in% expodata$NUTS_mort)){
    stop("Missing exposure data for some NUTS.")
  }

  # Compute RRs
  expodata$RR <- exp(log(pRR10)*expodata$pm25/10)
  expodata_sims <- map_dfc(sRR10, function(x) exp(log(x)*expodata$pm25/10))
  names(expodata_sims) <- gsub("RR10", "RR", names(expodata_sims))
  expodata <- cbind(expodata, expodata_sims)
  rm("expodata_sims")

  # Compute population weights by NUTS, daily population-weighted exposure + PAFs
  setDT(expodata)
  expodata[, popw := population/sum(population), by = .(NUTS_mort, NUTS_0, region, date)]
  RRcols <- names(expodata)[grepl("RR", names(expodata))]
  expodata <- expodata[, c(pm25w=sum(popw*pm25),
                           lapply(.SD, function(x) (sum(popw*x)-1)/sum(popw*x))),
                       by = .(NUTS_mort, NUTS_0, region, date), .SDcols = RRcols]
  names(expodata) <- gsub("RR", "PAF", names(expodata))

  # Merge mortality and exposure and estimate attributable mortality
  setkey(mort, NUTS_mort, date)
  setkey(expodata, NUTS_mort, date)
  mort_nuts <- mort[expodata, nomatch = 0]
  PAFcols <- names(mort_nuts)[grepl("PAF", names(mort_nuts))]
  mort_nuts <- mort_nuts[, lapply(.SD, function(x) x * deaths),
                         by = .(NUTS_mort, NUTS_0, region, year, date), .SDcols = PAFcols]
  names(mort_nuts) <- gsub("PAF", "attr", names(mort_nuts))

  # Aggregate to years ----
  attrcols <- names(mort_nuts)[grepl("attr", names(mort_nuts))]
  mort_nuts_year <- mort_nuts[, c(N = .N, lapply(.SD, sum)),
                              by = .(NUTS_mort, NUTS_0, region, year),
                              .SDcols = attrcols]
  setDF(mort_nuts_year)
  mort_nuts_year <- filter(mort_nuts_year, N >= 365) %>%
    dplyr::select(-N) %>%
    complete(NUTS_mort, year)

  # Aggregate to countries ----
  mort_country_year <- mort_nuts_year %>%
    group_by(year, NUTS_0, region) %>%
    summarise(across(starts_with("attr"), sum)) %>%
    ungroup()

  # Aggregate to regions ----
  mort_region_year <- mort_country_year %>%
    group_by(year, region) %>%
    summarise(across(starts_with("attr"), sum),
              Ncountries = n()) %>%
    ungroup()

  # Aggregate to Europe ----
  mort_euro_year <- mort_country_year %>%
    group_by(year) %>%
    summarise(across(starts_with("attr"), sum),
              Ncountries = n()) %>%
    ungroup()

  # Compute CIs ----
  mort_nuts_year$attrlower <-
    apply(dplyr::select(mort_nuts_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_nuts_year$attrupper <-
    apply(dplyr::select(mort_nuts_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_country_year$attrlower <-
    apply(dplyr::select(mort_country_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_country_year$attrupper <-
    apply(dplyr::select(mort_country_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_region_year$attrlower <-
    apply(dplyr::select(mort_region_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_region_year$attrupper <-
    apply(dplyr::select(mort_region_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_euro_year$attrlower <-
    apply(dplyr::select(mort_euro_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.025, na.rm=T))
  mort_euro_year$attrupper <-
    apply(dplyr::select(mort_euro_year, starts_with("attr_")), 1,
          function(x) quantile(x, 0.975, na.rm=T))
  mort_nuts_year <- dplyr::select(mort_nuts_year, -starts_with("attr_"))
  mort_country_year <- dplyr::select(mort_country_year, -starts_with("attr_"))
  mort_region_year <- dplyr::select(mort_region_year, -starts_with("attr_"))
  mort_euro_year <- dplyr::select(mort_euro_year, -starts_with("attr_"))

  # Return data
  list("mortality_nuts" = mort_nuts_year,
       "mortality_country" = mort_country_year,
       "mort_region_year" = mort_region_year,
       "mortality_euro" = mort_euro_year)
}


# 4. Execute  ----
hia03 <- hia(paste0(pathroot, "data/processed/assembled/data_2003.csv"))
hia04 <- hia(paste0(pathroot, "data/processed/assembled/data_2004.csv"))
hia05 <- hia(paste0(pathroot, "data/processed/assembled/data_2005.csv"))
hia06 <- hia(paste0(pathroot, "data/processed/assembled/data_2006.csv"))
hia07 <- hia(paste0(pathroot, "data/processed/assembled/data_2007.csv"))
hia08 <- hia(paste0(pathroot, "data/processed/assembled/data_2008.csv"))
hia09 <- hia(paste0(pathroot, "data/processed/assembled/data_2009.csv"))
hia10 <- hia(paste0(pathroot, "data/processed/assembled/data_2010.csv"))
hia11 <- hia(paste0(pathroot, "data/processed/assembled/data_2011.csv"))
hia12 <- hia(paste0(pathroot, "data/processed/assembled/data_2012.csv"))
hia13 <- hia(paste0(pathroot, "data/processed/assembled/data_2013.csv"))
hia14 <- hia(paste0(pathroot, "data/processed/assembled/data_2014.csv"))
hia15 <- hia(paste0(pathroot, "data/processed/assembled/data_2015.csv"))
hia16 <- hia(paste0(pathroot, "data/processed/assembled/data_2016.csv"))
hia17 <- hia(paste0(pathroot, "data/processed/assembled/data_2017.csv"))
hia18 <- hia(paste0(pathroot, "data/processed/assembled/data_2018.csv"))
hia19 <- hia(paste0(pathroot, "data/processed/assembled/data_2019.csv"))
hia20 <- hia(paste0(pathroot, "data/processed/assembled/data_2020.csv"))
hia21 <- hia(paste0(pathroot, "data/processed/assembled/data_2021.csv"))
hia22 <- hia(paste0(pathroot, "data/processed/assembled/data_2022.csv"))

# Attributable mortality by NUTS
attr_nuts <- bind_rows(hia03[[1]], hia04[[1]], hia05[[1]], hia06[[1]], hia07[[1]],
                       hia08[[1]], hia09[[1]], hia10[[1]], hia11[[1]], hia12[[1]],
                       hia13[[1]], hia14[[1]], hia15[[1]], hia16[[1]], hia17[[1]],
                       hia18[[1]], hia19[[1]], hia20[[1]], hia21[[1]], hia22[[1]])
write_csv(attr_nuts, paste0(pathroot, "data/processed/attributable_nuts.csv"))

# Attributable mortality by country
attr_country <- bind_rows(hia03[[2]], hia04[[2]], hia05[[2]], hia06[[2]], hia07[[2]],
                          hia08[[2]], hia09[[2]], hia10[[2]], hia11[[2]], hia12[[2]],
                          hia13[[2]], hia14[[2]], hia15[[2]], hia16[[2]], hia17[[2]],
                          hia18[[2]], hia19[[2]], hia20[[2]], hia21[[2]], hia22[[2]])
write_csv(attr_country, paste0(pathroot, "data/processed/attributable_country.csv"))

# Attributable mortality by region
attributable_region <- bind_rows(hia03[[3]], hia04[[3]], hia05[[3]], hia06[[3]], hia07[[3]],
                                 hia08[[3]], hia09[[3]], hia10[[3]], hia11[[3]], hia12[[3]],
                                 hia13[[3]], hia14[[3]], hia15[[3]], hia16[[3]], hia17[[3]],
                                 hia18[[3]], hia19[[3]], hia20[[3]], hia21[[3]], hia22[[3]])
write_csv(attributable_region, paste0(pathroot, "data/processed/attributable_region.csv"))

# Attributable mortality euro
attr_euro <- bind_rows(hia03[[4]], hia04[[4]], hia05[[4]], hia06[[4]], hia07[[4]],
                       hia08[[4]], hia09[[4]], hia10[[4]], hia11[[4]], hia12[[4]],
                       hia13[[4]], hia14[[4]], hia15[[4]], hia16[[4]], hia17[[4]],
                       hia18[[4]], hia19[[4]], hia20[[4]], hia21[[4]], hia22[[4]])
write_csv(attr_euro, paste0(pathroot, "data/processed/attributable_euro.csv"))
