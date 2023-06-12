#-----------------------------------------------------------------------------#
#                     3. Clean and weekly mortality data                      #
#-----------------------------------------------------------------------------#

# Warning:
# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI) available at the NUTS1 level
# No data for Macedonia (MK)
# The rest of the countries are available at NUTS 2 level
# Estonia (EE) mortality data is for NUTS16, but that makes no difference at the
# NUTS2 level.
pathroot <- ""

# 1. Clean mortality ----

# Read data
mort <- read_tsv(paste0(pathroot, "data/raw/mortality/demo_r_mwk2_ts.tsv"))
# Take years 2003-2022
mort <- dplyr::select(mort, 1, contains(as.character(2003:2022)))
# Disentangle 1st column, take both sexes and NUTS codes
names(mort)[1] <- "meta"
mort <- group_by(mort, meta) |>
  mutate(sex = strsplit(meta, ",")[[1]][1],
         NUTS_mort = strsplit(meta, ",")[[1]][3]) |>
  ungroup() |>
  filter(sex == "T") |>
  filter(nchar(NUTS_mort)==4|(nchar(NUTS_mort) == 3 & grepl("DE|IE|HR|SI", NUTS_mort))) |> # NUTS2 or NUTS1 for selected countries
  dplyr::select(-meta, -sex)
# Wide to long format, parse NAs and remove provisional flags
mort <- pivot_longer(mort, -NUTS_mort, names_to = "week", values_to = "deaths")
mort <- mutate(mort,
               deaths = ifelse(deaths == ":", NA, deaths),
               deaths = gsub(" p", "", deaths),
               deaths = as.numeric(deaths)) |>
  filter(!is.na(deaths))
# Discard XX/SIX NUTS2: unclassified and minor counts, no geometry available
mort <- mort[!grepl("XX", mort$NUTS_mort)&!grepl("SIX", mort$NUTS_mort),]
# Discard NO0B NUTS2 (Jan Mayen and Svalbard): always 0
mort <- mort[!grepl("NO0B", mort$NUTS_mort),]
# Discard extra Norway NUTS2: superseded (merged into larger NUTS2) in 2021 version
mort <- mort[!grepl("NO01|NO03|NO04|NO05", mort$NUTS_mort),]
# Discard more NUTS2 for which we have no exposure data: Canary Islands (ES70),
# Guadeloupe (FRY1), Martinique (FRY2), Guyane (FRY3), La Reunion (FRY4),
# Mayotte (FRY5), Acores (PT20), Madeira (PT30)
mort <- mort[!grepl("ES70|FRY1|FRY2|FRY3|FRY4|FRY5|PT20|PT30", mort$NUTS_mort),]
# Delete missing time
mort <- mort[!grepl("W99", mort$week),]
# Order by NUTS unit and week
mort <- arrange(mort, NUTS_mort, week)

# Check that all mortality codes have an available geometry
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
if(any(mort$NUTS_mort[!mort$NUTS_mort %in% pointnuts$NUTS_mort])){
  stop("Something has gone wrong when processing the weekly death counts.")
}
# Check that only North Macedonia has exposure but not mortality data
if(any(pointnuts$NUTS_mort[!pointnuts$NUTS_mort %in% mort$NUTS_mort] != "MK00")){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# 2. Weekly to daily counts (ISO 8601) ----
# https://ec.europa.eu/eurostat/data/database?node_code=demomwk

# Expand weeks to days by dividing all death counts by 7 and stacking 7 times
mort_day <- mutate(mort,
                   year_week = paste0(substr(week, 1, 4), "-", substr(week, 5, 7)))
mort_day <- mutate(mort_day, deaths = deaths/7)
mort_day <- bind_rows(mutate(mort_day, year_week_day = paste0(year_week, "-1")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-2")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-3")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-4")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-5")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-6")),
                       mutate(mort_day, year_week_day = paste0(year_week, "-7")))
mort_day <- mutate(mort_day, date = ISOweek2date(year_week_day))
mort_day <- arrange(mort_day, NUTS_mort, date)
# Check that the counts before vs. after processing match
if(sum(mort$deaths) != sum(mort_day$deaths)){
  stop("Something has gone wrong when processing the weekly death counts.")
}

# Filter by days and write
mort_day <- filter(mort_day, between(date, as.Date("2003-01-01"), as.Date("2022-12-31"))) |>
  mutate(year = year(date)) |>
  dplyr::select(-week)
# write_csv(mort_day, paste0(pathroot, "data/processed/mortality.csv"))
