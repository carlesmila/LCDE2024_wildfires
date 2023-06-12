#-----------------------------------------------------------------------------#
#                LCD Europe Wildfire Smoke indicator: Makefile                #
#-----------------------------------------------------------------------------#

library("terra")
library("sf")
library("lubridate")
library("ISOweek")
library("tidyverse")
library("readxl")
library("data.table")

# 1. Yearly population grids ----
source("R/1_population.R")

# 2. NUTS codes ----
source("R/2_nuts.R")

# 3. Mortality data ----
source("R/3_mortality.R")

# 4. Merge exposures, population, NUTS codes (HPC)
source("R/4_assemble.R")

# 5. Exposure and fire risk indicators by area (HPC)
source("R/5_exposure.R")

# 6. Attributable mortality indicators (HPC)
source("R/6_HIA.R")

# 6. Attributable mortality sensitivity analysis (HPC)
source("R/6_HIA_sens.R")

# 7. FWI fire risk
source("R/7_FWI.R")

# 8. Figures and tables
source("R/8_figtab.R")
