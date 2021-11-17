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
library(waffle)

httpgd::hgd()
httpgd::hgd_browse()

on_brand_Palette <- c("#ffc629","#b0b3b2","#9c5555","#7e6f9e","#52949a")

see_missing <- function(df) {
    as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Count") %>%
            mutate(nmiss = max(n) - n, 
                   pctn = n / max(n), 
                   pctmiss = nmiss / max(n))
                            }

get_description <- function(df) {
    d1 <- as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "n") %>%
            mutate(nmiss = max(n) - n, 
                   pctn = n / max(n), 
                   pctmiss = nmiss / max(n))
    d2 <- as_tibble(lapply(df, function(x) median(x, na.rm = T))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "median")
    d3 <- as_tibble(lapply(df, function(x) mean(x, na.rm = T))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "mean")
    d4 <- as_tibble(lapply(df, function(x) quantile(x, 1/4, na.rm = T))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Q1")                      
    d5 <- as_tibble(lapply(df, function(x) quantile(x, 3/4, na.rm = T))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Q3")   

    left_join(d1,
              left_join(d2,
                        left_join(d3,
                                  left_join(d4, d5, by = "Variable"),
                                  by = "Variable"),
                        by = "Variable"),
              by = "Variable")
                            }

csc_data <- csc %>%
                filter(!is.na(PELLCAT)) %>%
                mutate(CONTROL2 = case_when(CONTROL == "1" ~ "Public",
                                            CONTROL == "2" ~ "Private",
                                            CONTROL == "3" ~ "Private"),
                       ICLEVEL2 = case_when(ICLEVEL == "1" ~ "4-year",
                                            ICLEVEL == "2" ~ "2-year",
                                            ICLEVEL == "3" ~ "2-year"),
                       PELLCAT2 = case_when(PELLCAT == 0 ~ "0 Minority Pell",
                                           PELLCAT == 1 ~ "1 Majority Pell"),
                       CONTROL_ICLEVEL = paste0(CONTROL2, " ", ICLEVEL2))


bas <- c("UNITID", "INSTNM", "PELLCAT2", "CONTROL2", "ICLEVEL2", "CONTROL_ICLEVEL", "STABBR", "LONGITUDE", "LATITUDE", "BBRR2_FED_UG_N", "PCTFLOAN")

G_CE_T_U_rate <- c("OMAWDP8_ALL", "OMENRYP_ALL", "OMENRAP_ALL", "OMENRUP_ALL")
debt_variables <- c("DEBT_MDN", "IND_DEBT_MDN", "DEP_DEBT_MDN", 
                    "PLUS_DEBT_INST_MD", "PLUS_DEBT_INST_PELL_MD", "PLUS_DEBT_INST_NOPELL_MD",
                    "PLUS_DEBT_INST_COMP_MD", "PLUS_DEBT_INST_NOCOMP_MD",
                    "LO_INC_DEBT_MDN", "MD_INC_DEBT_MDN", "HI_INC_DEBT_MDN", 
                    "FIRSTGEN_DEBT_MDN", "NOTFIRSTGEN_DEBT_MDN", 
                    "PELL_DEBT_MDN", "NOPELL_DEBT_MDN", 
                    "GRAD_DEBT_MDN", "WDRAW_DEBT_MDN",
                    "PPLUS_PCT_LOW", "PPLUS_PCT_HIGH")
UG_loan_2yrs <- c("BBRR2_FED_UG_DFLT", "BBRR2_FED_UG_DLNQ", "BBRR2_FED_UG_FBR", "BBRR2_FED_UG_DFR", "BBRR2_FED_UG_NOPROG", "BBRR2_FED_UG_MAKEPROG", "BBRR2_FED_UG_PAIDINFULL", "BBRR2_FED_UG_DISCHARGE")
UGCOMP_loan_2yrs <- c("BBRR2_FED_UGCOMP_DFLT", "BBRR2_FED_UGCOMP_DLNQ", "BBRR2_FED_UGCOMP_FBR", "BBRR2_FED_UGCOMP_DFR", "BBRR2_FED_UGCOMP_NOPROG", "BBRR2_FED_UGCOMP_MAKEPROG", "BBRR2_FED_UGCOMP_PAIDINFULL", "BBRR2_FED_UGCOMP_DISCHARGE")
UGNOCOMP_loan_2yrs <- c("BBRR2_FED_UGNOCOMP_DFLT", "BBRR2_FED_UGNOCOMP_DLNQ", "BBRR2_FED_UGNOCOMP_FBR", "BBRR2_FED_UGNOCOMP_DFR", "BBRR2_FED_UGNOCOMP_NOPROG", "BBRR2_FED_UGNOCOMP_MAKEPROG", "BBRR2_FED_UGNOCOMP_PAIDINFULL", "BBRR2_FED_UGNOCOMP_DISCHARGE")

GCETU_rate <- csc_data %>% 
    select(all_of(bas), all_of(G_CE_T_U_rate)
           ) %>%
    mutate(onehundred = OMAWDP8_ALL + # Percentage of all student receiving an award within 8 years of entry
                         OMENRYP_ALL + # Percentage of all students that did not receive an award and are still enrolled at this institution 8 years after entry
                         OMENRAP_ALL + # Percentage of all students that did not receive an award and enrolled at another institution after leaving this institution within 8 years of entry
                         OMENRUP_ALL   # Percentage of all students that did not receive an award and whose enrollment status is unknown after leaving this institution within 8 years of entry
          )

UGLOAN2yr <- csc_data %>% 
    select(all_of(bas), all_of(UG_loan_2yrs)
           ) %>%
    mutate(onehundred = BBRR2_FED_UG_DFLT + # Percentage of graduate federal student loan borrowers in default after 2 years
                        BBRR2_FED_UG_DLNQ + # Percentage of graduate federal student loan borrowers in delinquency after 2 years
                        BBRR2_FED_UG_FBR + # Percentage of graduate federal student loan borrowers in forbearance after 2 years
                        BBRR2_FED_UG_DFR + # Percentage of graduate federal student loan borrowers in deferment after 2 years
                        BBRR2_FED_UG_NOPROG + # Percentage of graduate federal student loan borrowers not making progress after 2 years
                        BBRR2_FED_UG_MAKEPROG + # Percentage of graduate federal student loan borrowers making progress after 2 years
                        BBRR2_FED_UG_PAIDINFULL + # Percentage of graduate federal student loan borrowers paid in full after 2 years
                        BBRR2_FED_UG_DISCHARGE, # Percentage of graduate federal student loan borrowers with all loans discharged after 2 years
                        BBRR2_FED_UG_DFLT_DLNQ_FBR_DFR_NOPROG = BBRR2_FED_UG_DFLT + BBRR2_FED_UG_DLNQ + BBRR2_FED_UG_FBR + BBRR2_FED_UG_DFR + BBRR2_FED_UG_NOPROG,
                        NUM_BBRR2_FED_UG_DFLT = BBRR2_FED_UG_DFLT * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_DLNQ = BBRR2_FED_UG_DLNQ * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_FBR = BBRR2_FED_UG_FBR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_DFR = BBRR2_FED_UG_DFR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_NOPROG = BBRR2_FED_UG_NOPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_MAKEPROG = BBRR2_FED_UG_MAKEPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_PAIDINFULL = BBRR2_FED_UG_PAIDINFULL * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UG_DISCHARGE = BBRR2_FED_UG_DISCHARGE * BBRR2_FED_UG_N
          )
UGCOMPloan2yrs <- csc_data %>% 
    select(all_of(bas), all_of(UGCOMP_loan_2yrs)
           ) %>%
    mutate(onehundred = BBRR2_FED_UGCOMP_DFLT + # Percentage of graduate completer graduate federal student loan borrowers in default after 2 years
                        BBRR2_FED_UGCOMP_DLNQ + # Percentage of graduate completer graduate federal student loan borrowers in delinquency after 2 years
                        BBRR2_FED_UGCOMP_FBR + # Percentage of graduate completer graduate federal student loan borrowers in forbearance after 2 years
                        BBRR2_FED_UGCOMP_DFR + # Percentage of graduate completer graduate federal student loan borrowers in deferment after 2 years
                        BBRR2_FED_UGCOMP_NOPROG + # Percentage of graduate completer graduate federal student loan borrowers not making progress after 2 years
                        BBRR2_FED_UGCOMP_MAKEPROG + # Percentage of graduate completer graduate federal student loan borrowers making progress after 2 years
                        BBRR2_FED_UGCOMP_PAIDINFULL + # Percentage of graduate completer graduate federal student loan borrowers paid in full after 2 years
                        BBRR2_FED_UGCOMP_DISCHARGE, # Percentage of graduate completer graduate federal student loan borrowers with all loans discharged after 2 years
                        BBRR2_FED_UGCOMP_DFLT_DLNQ_FBR_DFR_NOPROG = BBRR2_FED_UGCOMP_DFLT + BBRR2_FED_UGCOMP_DLNQ + BBRR2_FED_UGCOMP_FBR + BBRR2_FED_UGCOMP_DFR + BBRR2_FED_UGCOMP_NOPROG,
                        NUM_BBRR2_FED_UGCOMP_DFLT = BBRR2_FED_UGCOMP_DFLT * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_DLNQ = BBRR2_FED_UGCOMP_DLNQ * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_FBR = BBRR2_FED_UGCOMP_FBR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_DFR = BBRR2_FED_UGCOMP_DFR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_NOPROG = BBRR2_FED_UGCOMP_NOPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_MAKEPROG = BBRR2_FED_UGCOMP_MAKEPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_PAIDINFULL = BBRR2_FED_UGCOMP_PAIDINFULL * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGCOMP_DISCHARGE = BBRR2_FED_UGCOMP_DISCHARGE * BBRR2_FED_UG_N
          )

UGNOCOMPloan2yrs <- csc_data %>% 
    select(all_of(bas), all_of(UGNOCOMP_loan_2yrs)
           ) %>%
    mutate(onehundred = BBRR2_FED_UGNOCOMP_DFLT + # Percentage of graduate non-completer graduate federal student loan borrowers in default after 2 years
                        BBRR2_FED_UGNOCOMP_DLNQ + # Percentage of graduate non-completer graduate federal student loan borrowers in delinquency after 2 years
                        BBRR2_FED_UGNOCOMP_FBR + # Percentage of graduate non-completer graduate federal student loan borrowers in forbearance after 2 years
                        BBRR2_FED_UGNOCOMP_DFR + # Percentage of graduate non-completer graduate federal student loan borrowers in deferment after 2 years
                        BBRR2_FED_UGNOCOMP_NOPROG + # Percentage of graduate non-completer graduate federal student loan borrowers not making progress after 2 years
                        BBRR2_FED_UGNOCOMP_MAKEPROG + # Percentage of graduate non-completer graduate federal student loan borrowers making progress after 2 years
                        BBRR2_FED_UGNOCOMP_PAIDINFULL + # Percentage of graduate non-completer graduate federal student loan borrowers paid in full after 2 years
                        BBRR2_FED_UGNOCOMP_DISCHARGE, # Percentage of graduate non-completer graduate federal student loan borrowers with all loans discharged after 2 years
                        BBRR2_FED_UGNOCOMP_DFLT_DLNQ_FBR_DFR_NOPROG = BBRR2_FED_UGNOCOMP_DFLT + BBRR2_FED_UGNOCOMP_DLNQ + BBRR2_FED_UGNOCOMP_FBR + BBRR2_FED_UGNOCOMP_DFR + BBRR2_FED_UGNOCOMP_NOPROG,
                        NUM_BBRR2_FED_UGNOCOMP_DFLT = BBRR2_FED_UGNOCOMP_DFLT * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_DLNQ = BBRR2_FED_UGNOCOMP_DLNQ * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_FBR = BBRR2_FED_UGNOCOMP_FBR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_DFR = BBRR2_FED_UGNOCOMP_DFR * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_NOPROG = BBRR2_FED_UGNOCOMP_NOPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_MAKEPROG = BBRR2_FED_UGNOCOMP_MAKEPROG * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_PAIDINFULL = BBRR2_FED_UGNOCOMP_PAIDINFULL * BBRR2_FED_UG_N,
                        NUM_BBRR2_FED_UGNOCOMP_DISCHARGE = BBRR2_FED_UGNOCOMP_DISCHARGE * BBRR2_FED_UG_N
          )


debt_var <- csc_data %>% 
    select(all_of(bas), all_of(debt_variables)
           ) %>%
    mutate(DEPplusPARENT_DEBT_MDN = DEP_DEBT_MDN + PLUS_DEBT_INST_MD,
           DEPplusPARENTPELL_DEBT_MDN = DEP_DEBT_MDN + PLUS_DEBT_INST_PELL_MD)

GCETU_rate
UGLOAN2yr
UGCOMPloan2yrs
UGNOCOMPloan2yrs
debt_var
###############################
mu <- plyr::ddply(GCETU_rate, c("PELLCAT2"), summarise, grp.median=median(OMAWDP8_ALL * 100, na.rm = T))
mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2"), summarise, grp.median=median(OMAWDP8_ALL * 100, na.rm = T))

ggplot(GCETU_rate, aes(x = OMAWDP8_ALL * 100, fill = PELLCAT2)) +
    geom_histogram(binwidth=.07 * 100,color="black",closed="right") +
    scale_x_continuous(breaks=seq(0,1,.1) * 100) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    scale_y_continuous(limits = c(0, 380)) +
    geom_vline(data=mu, aes(xintercept=grp.median, color = PELLCAT2),
               linetype="dashed",lwd=.8,show.legend = F) +
    stat_bin(aes(y=..count..,label=..count..),binwidth=.07 * 100, geom="text",vjust=-.5) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()
ggsave("images/PELLCATvsGRAD.png", width = 5120, height = 2160, units = "px")

ggplot(GCETU_rate, aes(x = OMAWDP8_ALL * 100, fill = PELLCAT2)) +
    geom_histogram(binwidth=.07 * 100,color="black",closed="right") +
    scale_x_continuous(breaks=seq(0,1,.1) * 100) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    scale_y_continuous(limits = c(0, 275)) +
    stat_bin(aes(y=..count..,label=..count..),binwidth=.07 * 100, geom="text",vjust=-.5) +
    geom_vline(data=mu2, aes(xintercept=grp.median, color=PELLCAT2),
               linetype="dashed",lwd=.8,show.legend = F) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/PELLCATvsGRAD_2yrVS4yrVSpubVSpriv.png", width = 5120, height = 2160, units = "px")
############################
############################
mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2"), summarise, grp.median=median(OMENRUP_ALL * 100, na.rm = T))
ggplot(GCETU_rate, aes(x = OMENRUP_ALL * 100, fill = PELLCAT2)) + # dropout
    geom_histogram(aes(y = ..count..),
                   binwidth = .08 * 100,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,1,.1) * 100) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    scale_y_continuous(limits = c(0, 480)) +
    geom_vline(data = mu2, 
            aes(xintercept = grp.median, 
                color = ICLEVEL2),
            linetype = "dashed",
            lwd = .8,
            show.legend = F) +
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .08 * 100, 
             geom = "text", color = "black",
             vjust = -.5) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/dropoutrate.png", width = 5120, height = 2160, units = "px")

ggplot() +
    geom_histogram(data = GCETU_rate %>% filter(PELLCAT2 == "0 Minority Pell"), aes(x = OMENRUP_ALL, y = ..count..), fill=on_brand_Palette[1],
                   binwidth = .08, color = "black", closed = "right", position = "identity") +
    geom_label(aes(x=.75, y=400, label="Dropout rate for Minority Pell schools"), color=on_brand_Palette[1]) +
    stat_bin(data = GCETU_rate %>% filter(PELLCAT2 == "0 Minority Pell"), 
             aes(x = OMENRUP_ALL, y = ..count.., 
             label = ..count..),
             binwidth = .08, 
             geom = "text", color = "black",
             vjust = -.5) +
    geom_histogram(data = GCETU_rate %>% filter(PELLCAT2 == "1 Majority Pell"), aes(x = OMENRUP_ALL, y = -..count..), fill= on_brand_Palette[2],
                   binwidth = .08, color = "black", closed = "right", position = "identity") +
        stat_bin(data = GCETU_rate %>% filter(PELLCAT2 == "0 Minority Pell"), 
                 aes(x = OMENRUP_ALL,y = -..count.., 
                 label = ..count..),
                 binwidth = .08, 
                 geom = "text", color = "black",
                 vjust = 18) +
    geom_label(aes(x=.75, y=-400, label="Dropout rate for Majority Pell schools"), color=on_brand_Palette[2]) +
    theme_bw()
ggsave("images/dropoutrate2.png", width = 5120, height = 2160, units = "px")
############################
############################
mu3 <- plyr::ddply(UGLOAN2yr, c("PELLCAT2"), summarise, grp.median=median(BBRR2_FED_UG_DFLT, na.rm = T))
ggplot(UGLOAN2yr, aes(x = BBRR2_FED_UG_DFLT * 100, fill = PELLCAT2)) +
    geom_histogram(aes(y = ..count..),
                   binwidth = .03 * 100,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,1,.1) * 100) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    scale_y_continuous(limits = c(0, 600)) +
    geom_vline(data = mu3, 
        aes(xintercept = grp.median * 100,
            color = PELLCAT2),
        linetype = "dashed",
        lwd = .8,
        show.legend = F) +
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .03 * 100, 
             geom = "text", color = "black",
             vjust = -.5) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()
ggsave("images/defaultrate.png", width = 5120, height = 2160, units = "px")
############################
############################
mu <- plyr::ddply(GCETU_rate, c("PELLCAT2"), summarise, grp.median=median(OMENRYP_ALL, na.rm = T))
mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2"), summarise, grp.median=mean(OMENRYP_ALL, na.rm = T))
ggplot(GCETU_rate, aes(x = OMENRYP_ALL * 100, fill = PELLCAT2)) +
    geom_histogram(binwidth=.01 * 100,color="black",closed="right") +
    scale_x_continuous(breaks=seq(0,1,.1) * 100) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    scale_y_continuous(limits = c(0, 380)) +
    # geom_vline(data=mu, aes(xintercept=grp.median, color = PELLCAT2),
    #            linetype="dashed",lwd=.8,show.legend = F) +
    geom_vline(data=mu2, aes(xintercept=grp.median, color = PELLCAT2),
               linetype="dashed",lwd=.8,show.legend = F) +
    stat_bin(aes(y=..count..,label=..count..),binwidth=.01 * 100, geom="text",vjust=-.5) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()

GCETU_rate %>% select(OMENRYP_ALL) %>% get_description()

csc %>%
    group_by(STABBR) %>%
    summarise(STATE_median_nevergivesup = median(OMENRYP_ALL, na.rm = T),
              STATE_median_dropouts = median(OMENRUP_ALL, na.rm = T),
              STATE_median_default_all = median(BBRR2_FED_UG_DFLT, na.rm = T),
              STATE_median_default_completed = median(BBRR2_FED_UGCOMP_DFLT, na.rm = T),
              STATE_median_default_notcompeleted = median(BBRR2_FED_UGNOCOMP_DFLT, na.rm = T),

              STATE_median_MAKEPROG_all = median(BBRR2_FED_UG_MAKEPROG, na.rm = T),
              STATE_median_MAKEPROG_completed = median(BBRR2_FED_UGCOMP_MAKEPROG, na.rm = T),
              STATE_median_MAKEPROG_notcompeleted = median(BBRR2_FED_UGNOCOMP_MAKEPROG, na.rm = T),

              STATE_median_NOPROG_all = median(BBRR2_FED_UG_NOPROG, na.rm = T),
              STATE_median_NOPROG_completed = median(BBRR2_FED_UGCOMP_NOPROG, na.rm = T),
              STATE_median_NOPROG_notcompeleted = median(BBRR2_FED_UGNOCOMP_NOPROG, na.rm = T),

              ) %>%
    view()





































