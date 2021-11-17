library(magrittr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(feather)
library(ggplot2)
library(sf)
library(sp)
library(geofacet)
library(ggpubr)
library(ggh4x)


httpgd::hgd()
httpgd::hgd_browse()

# R program for getting missing values
see_missing <- function(df) {
    as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Count")
                            }

fip_pth <- "C:/Code/GITHUB/Project3-njones738/p3_JoneQuinBoyd/personal/njones738/state_fp_codes.csv" # nolint
st_fp <- read_csv(fip_pth)

desired_fips <- st_fp %>%
                    filter(Mainland_plus == 1) %>%
                    rename(region = "Postal Code")

us_zips <- zctas(year = 2019)

us_counties <- counties(year = 2019) %>%
    filter(STATEFP %in% desired_fips$FIPS)