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

on_brand_Palette <- c("#ffc629","#b0b3b2","#9c5555","#7e6f9e","#52949a")
# R program for getting missing values
see_missing <- function(df) {
    as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Count")
                            }


# us_tracts <- tibble()
# us_school <- tibble()

# for (fip in desired_fips$region) {
#     tracts <- tracts(state = as.character(fip), year = 2019)
#     us_tracts <- rbind(us_tracts, tracts)
#     school <- school_districts(state = as.character(fip), year = 2019)
#     us_school <- rbind(us_school, tracts)
# }
us_states <- states(year = 2019) %>%
    filter(STUSPS %in% desired_fips$region)
us_counties <- counties(year = 2019) %>%
    filter(STATEFP %in% desired_fips$FIPS)
# us_zips <- zctas(year = 2019)

us_states <- st_transform(us_states, crs = 4326)
us_counties <- st_transform(us_counties, crs = 4326)
# us_tracts <- st_transform(us_tracts, crs = 4326)
# us_school <- st_transform(us_school, crs = 4326)
# us_zips <- st_transform(us_zips, crs = 4326)

csc_data2 <- csc_data %>%
                select("UNITID", "INSTNM", "LATITUDE", "LONGITUDE", "DEPPLUS_DEBT_MDN",
                       "CITY", STUSPS = "STABBR", "REGION", "ZIP", "PELLCAT", "PCTPELL",
                       "ST_FIPS", "LOCALE", "OPEFLAG", "OPENADMP",
                       "CURROPER", "MAIN", "CONTROL", "ICLEVEL",
                       "HIGHDEG", "PREDDEG", "HCM2", "ACCREDCODE",
                       "DEBT_MDN", "DEP_DEBT_MDN", "IND_DEBT_MDN", "PELL_DEBT_MDN", # nolint
                       "NOPELL_DEBT_MDN", "FIRSTGEN_DEBT_MDN", "NOTFIRSTGEN_DEBT_MDN",  # nolint
                       "DEBT_N", "DEP_DEBT_N", "IND_DEBT_N", 
                       "PELL_DEBT_N", "NOPELL_DEBT_N", "FIRSTGEN_DEBT_N", 
                       "NOTFIRSTGEN_DEBT_N", "DEP_DEBT_MDN", "IND_DEBT_MDN", 
                       "PELL_DEBT_MDN", "NOPELL_DEBT_MDN", 
                       "FIRSTGEN_DEBT_MDN", "NOTFIRSTGEN_DEBT_MDN", 
                       "DEBT_N", "DEP_DEBT_N", "IND_DEBT_N", 
                       "PELL_DEBT_N", "NOPELL_DEBT_N", 
                       "FIRSTGEN_DEBT_N", "NOTFIRSTGEN_DEBT_N", 
                       "PLUS_DEBT_INST_N", "PLUS_DEBT_INST_MD", 
                       "PLUS_DEBT_INST_COMP_N", "PLUS_DEBT_INST_COMP_MD", 
                       "PLUS_DEBT_INST_COMP_MDPAY10", "PLUS_DEBT_INST_COMP_MD_SUPP",  # nolint
                       "PLUS_DEBT_INST_COMP_MDPAY10_SUPP", "PLUS_DEBT_INST_NOCOMP_N", # nolint
                       "PLUS_DEBT_INST_NOCOMP_MD", "PLUS_DEBT_INST_PELL_N", 
                       "PLUS_DEBT_INST_PELL_MD", "PLUS_DEBT_INST_NOPELL_N", 
                       "PLUS_DEBT_INST_NOPELL_MD", "PLUS_DEBT_INST_STAFFTHIS_N",
                       "PLUS_DEBT_INST_STAFFTHIS_MD", "PLUS_DEBT_INST_NOSTAFFTHIS_N",  # nolint
                       "PLUS_DEBT_INST_NOSTAFFTHIS_MD")
csc_sf <- st_as_sf(csc_data2,
                   coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

state_list <- csc_data2 %>%
    group_by(STUSPS) %>%
    summarise(count = n()) %>%
    select(STUSPS) %>% 
    as.list()

slopes <- tibble()
AK <- c("AK",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "AK")))$coefficients[2])
AL <- c("AL",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "AL")))$coefficients[2])
AR <- c("AR",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "AR")))$coefficients[2])
AZ <- c("AZ",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "AZ")))$coefficients[2])
CA <- c("CA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "CA")))$coefficients[2])
CO <- c("CO",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "CO")))$coefficients[2])
CT <- c("CT",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "CT")))$coefficients[2])
DC <- c("DC",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "DC")))$coefficients[2])
DE <- c("DE",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "DE")))$coefficients[2])
FL <- c("FL",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "FL")))$coefficients[2])
GA <- c("GA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "GA")))$coefficients[2])
HI <- c("HI",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "HI")))$coefficients[2])
IA <- c("IA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "IA")))$coefficients[2])
ID <- c("ID",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "ID")))$coefficients[2])
IL <- c("IL",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "IL")))$coefficients[2])
IN <- c("IN",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "IN")))$coefficients[2])
KS <- c("KS",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "KS")))$coefficients[2])
KY <- c("KY",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "KY")))$coefficients[2])
LA <- c("LA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "LA")))$coefficients[2])
MA <- c("MA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MA")))$coefficients[2])
MD <- c("MD",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MD")))$coefficients[2])
ME <- c("ME",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "ME")))$coefficients[2])
MI <- c("MI",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MI")))$coefficients[2])
MN <- c("MN",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MN")))$coefficients[2])
MO <- c("MO",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MO")))$coefficients[2])
MS <- c("MS",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MS")))$coefficients[2])
MT <- c("MT",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "MT")))$coefficients[2])
NC <- c("NC",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NC")))$coefficients[2])
ND <- c("ND",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "ND")))$coefficients[2])
NE <- c("NE",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NE")))$coefficients[2])
NH <- c("NH",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NH")))$coefficients[2])
NJ <- c("NJ",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NJ")))$coefficients[2])
NM <- c("NM",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NM")))$coefficients[2])
NV <- c("NV",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NV")))$coefficients[2])
NY <- c("NY",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "NY")))$coefficients[2])
OH <- c("OH",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "OH")))$coefficients[2])
OK <- c("OK",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "OK")))$coefficients[2])
OR <- c("OR",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "OR")))$coefficients[2])
PA <- c("PA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "PA")))$coefficients[2])
RI <- c("RI",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "RI")))$coefficients[2])
SC <- c("SC",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "SC")))$coefficients[2])
SD <- c("SD",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "SD")))$coefficients[2])
TN <- c("TN",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "TN")))$coefficients[2])
TX <- c("TX",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "TX")))$coefficients[2])
UT <- c("UT",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "UT")))$coefficients[2])
VA <- c("VA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "VA")))$coefficients[2])
VT <- c("VT",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "VT")))$coefficients[2])
WA <- c("WA",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "WA")))$coefficients[2])
WI <- c("WI",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "WI")))$coefficients[2])
WV <- c("WV",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "WV")))$coefficients[2])
WY <- c("WY",summary(lm(DEPPLUS_DEBT_MDN ~ IND_DEBT_MDN, csc_data2 %>% filter(STUSPS == "WY")))$coefficients[2])
slopes <- tibble(AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN, KS, KY, LA, MA, MD, ME, MI, MN,
                 MO, MS, MT, NC, ND, NE, NH, NJ, NM, NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, 
                 WI, WV, WY) %>%
          slice(2) %>% 
          pivot_longer(everything(), names_to = "STABBR", values_to = "slope")

slopes %>% arrange(slope) %>% view()


# csc_dict <- read_csv("D:/CURRENT/GITHUB/Projects for Special Topics in Statistics - STAT4490.FALL21/Personal_Project_CollegeSc_SafeGaurd/csc_dict.csv") # nolint

# subdict_csc2 <- csc_dict %>%
#     filter(base::grepl("debt", Definition)) %>%
#     filter(base::grepl("institution", Definition)) %>%
#     filter(!base::grepl("any institution|institutions", Definition)) %>%
#     filter(!base::grepl("male|female", Definition))
# #subdict_csc2 %>% view()

# subdict_csc <- csc_dict %>%
#     filter(Variable == "IND_DEBT_N" |
#            Variable == "IND_DEBT_MDN" |
#            Variable == "DEP_DEBT_N" |
#            Variable == "DEP_DEBT_MDN" |
#            Variable == "DEP_DEBT_N" |
#            Variable == "DEP_DEBT_MDN" |
#            Variable == "DEP_DEBT_N" |
#            Variable == "DEP_DEBT_MDN" |
#            Variable == "PELL_DEBT_N" |
#            Variable == "PELL_DEBT_MDN" |
#            Variable == "NOPELL_DEBT_N" |
#            Variable == "NOPELL_DEBT_MDN" |
#            Variable == "FIRSTGEN_DEBT_N" |
#            Variable == "FIRSTGEN_DEBT_MDN" |
#            Variable == "NOTFIRSTGEN_DEBT_N" |
#            Variable == "NOTFIRSTGEN_DEBT_MDN" |
#            Variable == "DEBT_N" |
#            Variable == "DEBT_MD")
# subdict_csc %<>% rbind(subdict_csc, subdict_csc2)
    
# subdict_csc %>% view()

debt_state <- csc_data2 %>%
    group_by(STUSPS) %>%
    summarise(PCTPELL = median(PCTPELL, na.rm =T),
              DEBT_MDN = median(DEBT_MDN, na.rm = T),
              DEPPLUS_DEBT_MDN = median(DEP_DEBT_MDN+PLUS_DEBT_INST_MD, na.rm = T),
              DEP_DEBT_MDN = median(DEP_DEBT_MDN, na.rm = T),
              IND_DEBT_MDN = median(IND_DEBT_MDN, na.rm = T),
              PELL_DEBT_MDN = median(PELL_DEBT_MDN, na.rm = T),
              NOPELL_DEBT_MDN = median(NOPELL_DEBT_MDN, na.rm = T),
              FIRSTGEN_DEBT_MDN = median(FIRSTGEN_DEBT_MDN, na.rm = T),
              NOTFIRSTGEN_DEBT_MDN = median(NOTFIRSTGEN_DEBT_MDN , na.rm = T), # nolint
              PLUS_DEBT_INST_MD = median(PLUS_DEBT_INST_MD, na.rm = T),
              PLUS_DEBT_INST_COMP_MD = median(PLUS_DEBT_INST_COMP_MD , na.rm = T), # nolint
              PLUS_DEBT_INST_NOCOMP_MD = median(PLUS_DEBT_INST_NOCOMP_MD, na.rm = T), # nolint
              PLUS_DEBT_INST_PELL_MD = median(PLUS_DEBT_INST_PELL_MD , na.rm = T), # nolint
              PLUS_DEBT_INST_NOPELL_MD = median(PLUS_DEBT_INST_NOPELL_MD , na.rm = T), # nolint
              PLUS_DEBT_INST_STAFFTHIS_MD = median(PLUS_DEBT_INST_STAFFTHIS_MD, na.rm = T), # nolint
              PLUS_DEBT_INST_NOSTAFFTHIS_MD = median(PLUS_DEBT_INST_NOSTAFFTHIS_MD, na.rm = T), # nolint
              DEBT_N = sum(DEBT_N, na.rm = T),
              DEP_DEBT_N = sum(DEP_DEBT_N, na.rm = T),
              IND_DEBT_N = sum(IND_DEBT_N, na.rm = T),
              PELL_DEBT_N = sum(PELL_DEBT_N, na.rm = T),
              NOPELL_DEBT_N = sum(NOPELL_DEBT_N, na.rm = T),
              FIRSTGEN_DEBT_N = sum(FIRSTGEN_DEBT_N, na.rm = T),
              NOTFIRSTGEN_DEBT_N = sum(NOTFIRSTGEN_DEBT_N, na.rm = T),
              PLUS_DEBT_INST_N = sum(PLUS_DEBT_INST_N, na.rm = T),
              PLUS_DEBT_INST_COMP_N = sum(PLUS_DEBT_INST_COMP_N, na.rm = T), # nolint
              PLUS_DEBT_INST_NOCOMP_N = sum(PLUS_DEBT_INST_NOCOMP_N, na.rm = T), # nolint
              PLUS_DEBT_INST_PELL_N = sum(PLUS_DEBT_INST_PELL_N, na.rm = T), # nolint
              PLUS_DEBT_INST_NOPELL_N = sum(PLUS_DEBT_INST_NOPELL_N, na.rm = T) #, # nolint
             )

count_state <- csc_data2 %>%
                    dplyr::group_by(STUSPS) %>%
                    summarise(count = n())
#us_states %<>% mutate(lon = as.numeric(us_states$INTPTLON), lat = as.numeric(us_states$INTPTLAT)) %>% select(-INTPTLON, -INTPTLAT)
us_states2 <- left_join(us_states, debt_state, by = "STUSPS")
us_states2 <- left_join(us_states2, count_state, by = "STUSPS")

# ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI"))) +
#     geom_sf(aes(geometry = geometry, fill = count)) +
#     labs(title = "Count of schools by state") +
#     theme_bw()

# ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI"))
#       ) +
#     geom_sf(aes(geometry = geometry, fill = PCTPELL)) +
#     labs(title = "Median proportion of Pell students for a university by state") +
#     theme_bw()

# p1 <- ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")),
#        aes(geometry = geometry)
#       ) +
#     geom_sf(data = us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")) %>%
#                         filter(PCTPELL >= .5), 
#             aes(geometry = geometry, fill = DEP_DEBT_MDN)) +
#     geom_sf(data = us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")) %>%
#                         filter(PCTPELL < .5), 
#             aes(geometry = geometry), fill = "white") +
#     labs(title = "MAJORITY PELL: Median debt a Dependent student takes on by state") +
#     theme_bw()
# p2 <- ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")),
#        aes(geometry = geometry)
#       ) +
#     geom_sf(data = us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")) %>%
#                         filter(PCTPELL < .5), 
#             aes(geometry = geometry, fill = DEP_DEBT_MDN)) +
#     geom_sf(data = us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI")) %>%
#                         filter(PCTPELL >= .5), 
#             aes(geometry = geometry), fill = "white") +
#     labs(title = "MINORITY PELL: Median debt a Dependent student takes on by state") +
#     theme_bw()
# ggarrange(p1, p2, ncol = 2)

# ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI"))) +
#     geom_sf(aes(geometry = geometry, fill = DEBT_MDN)) +
#     labs(title = "Median debt any student takes on by state")
#     theme_bw()

# ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI"))) +
#     geom_sf(aes(geometry = geometry, fill = IND_DEBT_MDN)) +
#     labs(title = "Median debt a Independent student takes on by state") +
#     theme_bw()

# ggplot(us_states2 %>% 
#                 filter(!STUSPS %in% c("AK", "HI"))) +
#     geom_sf(aes(geometry = geometry, fill = DEP_DEBT_MDN)) +
#     labs(title = "Median debt a Dependent student takes on by state") +
#     theme_bw()



##############################################################
##############################################################

dat_space <- csc_sf %>%
    filter(!is.na(DEP_DEBT_MDN)) %>%
    filter(!is.na(IND_DEBT_MDN)) %>%
    filter(!is.na(PLUS_DEBT_INST_MD)) %>%
    mutate(DEPPLUS_DEBT_MDN = DEP_DEBT_MDN + PLUS_DEBT_INST_MD) %>%
    select(UNITID, INSTNM, CITY, STUSPS, PCTPELL, IND_DEBT_MDN,
           DEPPLUS_DEBT_MDN, CONTROL, ICLEVEL) %>%
    filter(!is.na(DEPPLUS_DEBT_MDN)) %>%
    group_by(STUSPS) %>% 
    st_drop_geometry()


states <- left_join(us_states, dat_space, by = "STUSPS") %>%
            rename(name = NAME) %>%
            mutate(PELLCAT = case_when(PCTPELL < .5 ~ "Minority Pell school",
                                       PCTPELL >= .5 ~ "Majority Pell school"),
                   CONTROL2 = case_when(CONTROL == 1 ~ "Public",
                                        CONTROL == 2 | CONTROL == 3 ~ "Private",
                                        T ~ "999")) %>%
            filter(CONTROL2 != "999")
states$CONTROL2 <- as.factor(states$CONTROL2)

bin_num <- 5

x_min <- floor(min(states$IND_DEBT_MDN)/1000)*1000
x_max <- ceiling(max(states$IND_DEBT_MDN)/1000)*1000

y_min <- floor(min(states$DEPPLUS_DEBT_MDN)/1000)*1000
y_max <- ceiling(max(states$DEPPLUS_DEBT_MDN)/1000)*1000

states2 <- states %>%
            filter(!is.na(PELLCAT)) %>%
            select(-PELLCAT)

color <- on_brand_Palette[1:2]

ggplot(states %>% filter(!is.na(PELLCAT)),
       aes(x = IND_DEBT_MDN,
           y = DEPPLUS_DEBT_MDN)
      ) +
    facet_geo(~STUSPS, grid = "us_state_grid2", label = "code") +
    geom_point(data = states2, aes(x = IND_DEBT_MDN,
                            y = DEPPLUS_DEBT_MDN),
                            colour = "#dfdfdf") +
    geom_point(aes(colour = PELLCAT, shape = CONTROL2,
               group = PELLCAT)
               ) + # , show.legend = F
    scale_color_manual(name="group",
                      values= on_brand_Palette[1:2],
                      labels=c("0", "1")) +
    geom_smooth(method = "lm", se = F, size = 0.6) +
    geom_segment(aes(x = 0, y = 0,
                     xend = x_max, yend = x_max), size = 0.3,
                     colour = "grey2", show.legend = F) +
    coord_cartesian(xlim = c(x_min, x_max),
                    ylim = c(y_min, y_max)) +
    scale_x_continuous(breaks = seq(x_min, x_max,
                                    x_min + x_max / bin_num)) + # nolint
    xlab("Typical debt taken on by Independent Students") +
    ylab("Typical debt taken on by Dependent Students and their parents") + # nolint
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, vjust = 0.2, hjust = 0.1),
          panel.spacing = unit(0.1, "lines"),
          legend.position = "top",
          strip.text = element_text(face = "bold"),
          strip.text.x = element_text(size = 12, margin = margin()),
          strip.text.y = element_text(size = 40, margin = margin())) +
    force_panelsizes(rows = unit(1.5, "cm"), 
                     cols = unit(3, "cm"), 
                     TRUE)
ggsave("images/usa_debt2a.png", width = 5120, height = 2160, units = "px")

# ggplot(states %>% filter(!is.na(PELLCAT)), aes(x = IND_DEBT_MDN, y = DEPPLUS_DEBT_MDN)) +
#     geom_point() +
#     geom_smooth(method = "lm", se = F) +
#     geom_segment(aes(x = 0, y = 0,
#                      xend = x_max, yend = x_max,
#                      colour = "red"), show.legend = F) +
#     coord_cartesian(xlim = c(x_min, x_max),
#                     ylim = c(y_min, y_max)) +
#     scale_x_continuous(breaks = seq(x_min, x_max,
#                                     x_min + x_max / bin_num)) + # nolint
#     facet_geo(~STUSPS, grid = "us_state_grid2", label = "code") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#           panel.spacing = unit(0.1, "lines"))
# ggsave("images/usa_debt.png", width = 3840, height = 2160, units = "px")

# ggplot(states %>% filter(!is.na(PELLCAT)), aes(x = IND_DEBT_MDN, y = DEPPLUS_DEBT_MDN)) +
#     geom_point(aes(shape = PELLCAT, colour = CONTROL)) +
#     geom_smooth(method = "lm", se = F) +
#     geom_segment(aes(x = 0, y = 0,
#                      xend = x_max, yend = x_max)) +
#     coord_cartesian(xlim = c(x_min, x_max),
#                     ylim = c(y_min, y_max)) +
#     scale_x_continuous(breaks = seq(x_min, x_max,
#                                     x_min + x_max / bin_num)) + # nolint
#     facet_geo(~STUSPS, grid = "us_state_grid2", label = "code") +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#           panel.spacing = unit(0.1, "lines"))
# ggsave("images/usa_debt3.png", width = 7680, height = 4320, units = "px")


# cool code
# install.packages("gt")
# library(gt)

# subdict_csc %>% 
#     gt() %>%
#     gt::tab_options(
#         table.font.names = "Montserrat"
#     )