# Lancet Countdown Europe 2024: Wildfire smoke indicator

This repository contains the code to generate the wildfire smoke indicators for the Lancet Countdown Europe 2024 edition, as well as the figures and tables included in the report and appendix. Please refer to the appendix file for a complete description of the methods.

## Analysis

The workflow to compute the indicators is structured in a sequential set of steps that can be run from a root [makefile](makefile.R) script. Note that some of the steps were performed in a High Performance Computing (HPC) cluster due to memory usage. The steps to be run are:

* [1. Population data processing](R/1_population.R): Clean GEOSTAT gridded population data and create raster at exposure geometries.
* [2. NUTS boundary data processing](R/2_nuts.R): Assign NUTS and region codes to each of the exposure geometries.
* [3. Mortality data processing](R/3_mortality.R): Clean EUROSTAT mortality time series.
* [4. Exposure data processing](R/4_assemble.R): Extract daily exposures, population and NUTS codes (HPC).
* [5. Exposure analysis](R/5_exposures.R): Construct wildfire-PM2.5 exposure indicators (HPC).
* [6a. Health Impact Assessment (HIA) analysis](R/6_HIA.R): Construct attributable mortality indicators (HPC).
* [6b. HIA sensitivity analysis](R/6_HIA_sens.R): HIA indicators using an alternative exposure-response function (HPC).
* [7. Fire weather index analysis](R/7_FWI.R): Construct Forest fire Weather Index (FWI) indicators.
* [8. Tables and figures](R/8_figtab.R): Code to generate the figures and tables included in the report.

## Preparation

The indicator uses a set of open datasets (NUTS and regional boundaries, mortality time series, gridded population estimates) that can be easily obtained from Eurostat. Those should be placed in the folder `data/raw/boundaries`, `data/raw/regions`, `data/raw/population`, `data/raw/mortality`, respectively. These are not included in the repository due to large file sizes. 

Furthermore, the indicator uses wildfire-PM2.5 exposure data derived from the Integrated System for wild-land Fires (IS4FIRES) and the System for Integrated modelling of Atmospheric composition (SILAM) models, as well as Forest fire Weather Index (FWI) information computed using ERA5 data. Please contact the authors of the indicator (carles.mila@isglobal.com, Mikhail.Sofiev@fmi.fi, risto.hanninen@fmi.fi, cathryn.tonne@isglobal.org) for details about how to get access to the data. SILAM data should be placed in the `data/raw/SILAM` folder and fire danger data should be placed in the `data/raw/FWI` folder.

This code was run using R version 4.2.1 with the packages specified in the makefile script for the analysis part, as well as the visualization packages included in the tables and figures script.
