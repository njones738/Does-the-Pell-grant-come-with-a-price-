library(magrittr)
library(tidyverse)
library(feather)
library(ggplot2)
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

csc_data <- csc %>%
                select(UNITID, INSTNM, PELLCAT, ICLEVEL, CONTROL, STABBR,
                       DEBT_MDN, DEP_DEBT_MDN, IND_DEBT_MDN, PLUS_DEBT_INST_MD,
                       PELL_DEBT_MDN, NOPELL_DEBT_MDN) %>%
                mutate(CONTROL2 = case_when(CONTROL == "1" ~ "Public",
                                            CONTROL == "2" ~ "Private",
                                            CONTROL == "3" ~ "Private"),
                       ICLEVEL2 = case_when(ICLEVEL == "1" ~ "4-year",
                                            ICLEVEL == "2" ~ "2-year",
                                            ICLEVEL == "3" ~ "2-year"),
                       PELLCAT = case_when(PELLCAT == 0 ~ "0 Minority Pell",
                                           PELLCAT == 1 ~ "1 Majority Pell")) %>%
                filter(!is.na(PELLCAT))


full_debt <- csc_data %>%
    group_by(STABBR) %>%
    summarise(DEBT_MDN = median(DEBT_MDN, na.rm = T),
              DEP_DEBT_MDN = median(DEP_DEBT_MDN, na.rm = T),
              IND_DEBT_MDN = median(IND_DEBT_MDN, na.rm = T),
              PELL_DEBT_MDN = median(PELL_DEBT_MDN, na.rm = T),
              NOPELL_DEBT_MDN = median(NOPELL_DEBT_MDN, na.rm = T),
              PLUS_DEBT_INST_MD = median(PLUS_DEBT_INST_MD, na.rm = T),
              DEPPLUS_DEBT_MDN = median(DEP_DEBT_MDN + ifelse(is.na(PLUS_DEBT_INST_MD), # nolint
                                                                median(csc$PLUS_DEBT_INST_MD, na.rm = T), # nolint
                                                                    PLUS_DEBT_INST_MD), # nolint
                                                              na.rm = T),
              DEPPLUSminusIND_DEBT_MDN = median(DEPPLUS_DEBT_MDN - IND_DEBT_MDN, na.rm = T))

minority_debt <- csc_data %>%
    filter(PELLCAT == "0 Minority Pell") %>%
    group_by(STABBR) %>%
    summarise(DEBT_MDN = median(DEBT_MDN, na.rm = T),
              DEP_DEBT_MDN = median(DEP_DEBT_MDN, na.rm = T),
              IND_DEBT_MDN = median(IND_DEBT_MDN, na.rm = T),
              PELL_DEBT_MDN = median(PELL_DEBT_MDN, na.rm = T),
              NOPELL_DEBT_MDN = median(NOPELL_DEBT_MDN, na.rm = T),
              PLUS_DEBT_INST_MD = median(PLUS_DEBT_INST_MD, na.rm = T),
              DEPPLUS_DEBT_MDN = median(DEP_DEBT_MDN + ifelse(is.na(PLUS_DEBT_INST_MD), # nolint
                                                                median(csc$PLUS_DEBT_INST_MD, na.rm = T), # nolint
                                                                    PLUS_DEBT_INST_MD), # nolint
                                                              na.rm = T),
              DEPPLUSminusIND_DEBT_MDN = median(DEPPLUS_DEBT_MDN - IND_DEBT_MDN, na.rm = T))


majority_debt <- csc_data %>%
    filter(PELLCAT == "1 Majority Pell") %>%
    group_by(STABBR) %>%
    summarise(DEBT_MDN = median(DEBT_MDN, na.rm = T),
              DEP_DEBT_MDN = median(DEP_DEBT_MDN, na.rm = T),
              IND_DEBT_MDN = median(IND_DEBT_MDN, na.rm = T),
              PELL_DEBT_MDN = median(PELL_DEBT_MDN, na.rm = T),
              NOPELL_DEBT_MDN = median(NOPELL_DEBT_MDN, na.rm = T),
              PLUS_DEBT_INST_MD = median(PLUS_DEBT_INST_MD, na.rm = T),
              DEPPLUS_DEBT_MDN = median(DEP_DEBT_MDN + ifelse(is.na(PLUS_DEBT_INST_MD), # nolint
                                                                median(csc$PLUS_DEBT_INST_MD, na.rm = T), # nolint
                                                                    PLUS_DEBT_INST_MD), # nolint
                                                              na.rm = T),
              DEPPLUSminusIND_DEBT_MDN = median(DEPPLUS_DEBT_MDN - IND_DEBT_MDN, na.rm = T))


ggplot() +
    geom_col(data = majority_debt, 
             aes(x = DEBT_MDN, y = reorder(STABBR, DEBT_MDN)),
             fill = "#b0b3b2"
            ) +
    geom_col(data = minority_debt,
             aes(x = -DEBT_MDN, y = STABBR),
             fill = "#ffc629"
            ) +
    theme_bw()

ggplot() +
    geom_col(data = majority_debt, 
             aes(x = DEPPLUS_DEBT_MDN, y = reorder(STABBR, DEPPLUS_DEBT_MDN)),
             fill = "#b0b3b2"
            ) +
    geom_col(data = minority_debt,
             aes(x = -DEPPLUS_DEBT_MDN, y = STABBR),
             fill = "#ffc629"
            ) +
    scale_x_continuous(limits = c(-75000, 25000)) +
    theme_bw()
ggsave("images/barchart_DEPPLUS.png", width = 5120, height = 2160, units = "px")


ggplot() +
    geom_vline(aes(xintercept = 30000),
                   linetype = "dashed",
                   lwd = .8,
                   show.legend = F) +
    geom_vline(aes(xintercept = 20000),
                   linetype = "dashed",
                   lwd = .8,
                   show.legend = F) +
    geom_col(data = full_debt, 
             aes(x = DEPPLUS_DEBT_MDN, y = reorder(STABBR, DEPPLUS_DEBT_MDN)),
             fill = "#b0b3b2"
            ) +
    geom_col(data = full_debt,
             aes(x = IND_DEBT_MDN, y = reorder(STABBR, DEPPLUS_DEBT_MDN)),
             fill = "#ffc629"
            ) +
    theme_bw()







ggsave("images/barchart_IND.png", width = 5120, height = 2160, units = "px")


csc_data %>%
    group_by(STABBR) %>%
    summarise(DEBT_MDN = median(IND_DEBT_MDN, na.rm = T)) %>%
    ggplot() +
    geom_col(aes(x = IND_DEBT_MDN, y = reorder(STABBR, IND_DEBT_MDN)),
             fill = "#b0b3b2"
            ) +
    stat_bin(aes(y = ..count.., 
            label = ..count.. ),
            binwidth = .03 , 
            geom = "text", color = "black",
            vjust = -.5) +
    geom_vline(aes(xintercept = median(csc$IND_DEBT_MDN, na.rm = T)),
                   linetype = "dashed",
                   lwd = .8,
                   show.legend = F)

ggplot() +

    geom_col(data = full_debt, 
             aes(x = (DEPPLUS_DEBT_MDN - IND_DEBT_MDN), y = reorder(STABBR, (DEPPLUS_DEBT_MDN - IND_DEBT_MDN))),
             fill = "#2066B9"
            ) +
    theme_bw()

csc %>%
    filter(STABBR == "RI") %>% 
    group_by(STABBR) %>%
    select(INSTNM, DEBT_MDN, DEBT_N, DEP_DEBT_MDN, IND_DEBT_MDN, PLUS_DEBT_INST_MD) %>%
    mutate(DEPPLUS_DEBT_MDN = median(DEP_DEBT_MDN + ifelse(is.na(PLUS_DEBT_INST_MD), # nolint
                                                                median(csc$PLUS_DEBT_INST_MD, na.rm = T), # nolint
                                                                    PLUS_DEBT_INST_MD), # nolint
                                                              na.rm = T)) %>%
    summarise(med_dep = median(DEPPLUS_DEBT_MDN, na.rm = T),
              med_ind = median(IND_DEBT_MDN, na.rm = T)) %>%
    mutate(diff = med_dep - med_ind) %>%
    ggplot() +
        geom_col(aes(x = diff, y = STABBR)) + 

    theme_bw()