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

csc2 <- csc_data %>%
       select(INSTNM, STABBR, PELLCAT, DEP_DEBT_MDN, PLUS_DEBT_INST_MD, IND_DEBT_MDN) %>%
       mutate(DEPPLUS_DEBT_MDN = DEP_DEBT_MDN + PLUS_DEBT_INST_MD)
       
csc2 %>%
       group_by(STABBR) %>%
       summarise(med_DEPPLUS = median(DEPPLUS_DEBT_MDN, na.rm = T),
                 med_IND = median(IND_DEBT_MDN, na.rm = T)) %>%
       ggplot() +
              geom_col(aes(x = med_DEPPLUS, y = reorder(STABBR, med_DEPPLUS)),
                            fill = "#b0b3b2") + 
              geom_col(aes(x = med_IND, y = reorder(STABBR, med_DEPPLUS)),
                            fill = "#ffc629") +
       geom_vline(aes(xintercept = median(csc2$IND_DEBT_MDN, na.rm = T)),
                     linetype = "dashed",
                     lwd = .8, color = "red",
                     show.legend = F) +
       geom_vline(aes(xintercept = 20000),
                     linetype = "dashed",
                     lwd = .8, color = "blue",
                     show.legend = F) +
       theme_bw()
ggsave("images/barchart_DEPPLUS.png", width = 5120, height = 2160, units = "px")








