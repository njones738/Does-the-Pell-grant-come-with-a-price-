library(tidycensus)
library(tigris)
library(ggplot2)
library(sf)
library(sp)
library(geofacet)
library(ggpubr)
library(ggh4x)
library(waffle)

httpgd::hgd()
httpgd::hgd_browse()

mu <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2","CONTROL2"), summarise, grp.median=median(OMAWDP8_ALL * 100, na.rm = T))
mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2","CONTROL2"), summarise, grp.median=median(OMAWDP8_ALL * 100, na.rm = T))
ggplot(GCETU_rate, aes(x = OMAWDP8_ALL * 100, fill = PELLCAT2)) +
    geom_histogram(binwidth = .07 * 100,
                   color = "black",
                   closed = "right") +
    scale_x_continuous(breaks = seq(0, 1, 0.1) * 100) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 275)) +
    stat_bin(aes(y = ..count.., label = ..count..),
             binwidth = 0.07 * 100, 
             geom = "text",
             vjust = -0.5) +
    geom_vline(data = mu,
               aes(xintercept = grp.median,
               color = CONTROL2),
               linetype = "dashed",
               lwd = 0.8,
               show.legend = F) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/PELLCATvsGRAD_2yrVS4yrVSpubVSpriv.png", width = 5120, height = 2160, units = "px")

ggplot(GCETU_rate,
       aes(x = OMENRYP_ALL, fill = PELLCAT2)) + # Percentage of all students that did not receive an award and are still enrolled at this institution 8 years after entry
    geom_histogram(aes(y = ..count..),
                   binwidth = .02,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,.6,.05)) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    # scale_y_continuous(limits = c(0, 420)) +
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .02, 
             geom = "text", color = "black",
             vjust = -.5) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggplot(GCETU_rate %>%
            filter(OMENRYP_ALL > .125),
       aes(x = OMENRYP_ALL, fill = PELLCAT2)) + # Percentage of all students that did not receive an award and are still enrolled at this institution 8 years after entry
    geom_histogram(aes(y = ..count..),
                   binwidth = .02,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,.6,.05)) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    # scale_y_continuous(limits = c(0, 420)) +
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .02, 
             geom = "text", color = "black",
             vjust = -.5) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggplot(GCETU_rate %>%
            filter(OMENRYP_ALL < .125),
       aes(x = OMENRYP_ALL, fill = PELLCAT2)) + # Percentage of all students that did not receive an award and are still enrolled at this institution 8 years after entry
    geom_histogram(aes(y = ..count..),
                   binwidth = .005,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,.11,.01)) + 
    scale_fill_manual(name="group",
                      values=on_brand_Palette[1:4],
                      labels=c("0", "1")) +
    # scale_y_continuous(limits = c(0, 420)) +
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .005, 
             geom = "text", color = "black",
             vjust = -.5) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()


mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2"), summarise, grp.median=median(OMENRUP_ALL, na.rm = T))
ggplot(GCETU_rate, aes(x = OMENRUP_ALL, fill = PELLCAT2)) + # dropout
    geom_histogram(aes(y = ..count..),
                   binwidth = .08,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks=seq(0,1,.1)) + 
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
             binwidth = .08, 
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


mu3 <- plyr::ddply(UGLOAN2yr, c("PELLCAT2", "ICLEVEL2"), summarise, grp.median=median(BBRR2_FED_UG_DFLT, na.rm = T))
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
    stat_bin(aes(y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .03 * 100, 
             geom = "text", color = "black",
             vjust = -.5) +
    geom_vline(data = mu3, 
        aes(xintercept = grp.median * 100,
            color = PELLCAT2),
        linetype = "dashed",
        lwd = .8,
        show.legend = F) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/defaultrate.png", width = 5120, height = 2160, units = "px")




ggplot() +
    geom_histogram(data = UGCOMPloan2yrs, aes(x = BBRR2_FED_UGCOMP_DFLT, y = ..count..), fill="#69b3a2",
                   binwidth = .03, color = "black", closed = "right", position = "identity") +
    geom_label(aes(x=.4, y=200, label="2 year default rate for students whom graduated"), color="#69b3a2") +
    stat_bin(data = UGCOMPloan2yrs, 
             aes(x = BBRR2_FED_UGCOMP_DFLT, y = ..count.., 
             label = round( ..count.. , 2)),
             binwidth = .03, 
             geom = "text", color = "black",
             vjust = -.5) +
    geom_histogram(data = UGNOCOMPloan2yrs, aes(x = BBRR2_FED_UGNOCOMP_DFLT, y = -..count..), fill= "#404080",
                   binwidth = .03, color = "black", closed = "right", position = "identity") +
        stat_bin(data = UGNOCOMPloan2yrs, 
                 aes(x = BBRR2_FED_UGNOCOMP_DFLT,y = -..count.., 
                 label = round( ..count.. , 2)),
                 binwidth = .03, 
                 geom = "text", color = "black",
                 vjust = 25) +
    geom_label(aes(x=.4, y=-200, label="2 year default rate for students whom did not graduate"), color="#404080") +
    theme_bw()
ggsave("images/COMPvsNOCOMPdefaultrate.png", width = 5120, height = 2160, units = "px")


csc %>% filter(ICLEVEL == 2) %>% filter(CONTROL == 3) %>% select(INSTNM, PCTPELL) %>% view()
######################################################################

UGLOAN2yr %<>% select(all_of(bas), -starts_with("BBRR2"), BBRR2_FED_UG_N, starts_with("NUM_"))
UGCOMPloan2yrs %<>% select(all_of(bas), -starts_with("BBRR2"), BBRR2_FED_UG_N, starts_with("NUM_"))
UGNOCOMPloan2yrs %<>% select(all_of(bas), -starts_with("BBRR2"), BBRR2_FED_UG_N, starts_with("NUM_"))

ggplot(UGLOAN2yr, 
       aes(x = (NUM_BBRR2_FED_UG_DFLT + NUM_BBRR2_FED_UG_DLNQ + NUM_BBRR2_FED_UG_FBR + NUM_BBRR2_FED_UG_DFR + NUM_BBRR2_FED_UG_NOPROG))) +
    labs(title = "Default") +
    geom_histogram(color = "black", fill = "#ffc629") +
    # facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = , fill = PELLCAT2)) +
    labs(title = "Delinquency") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = , fill = PELLCAT2)) +
    labs(title = "Forbearance") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = , fill = PELLCAT2)) +
    labs(title = "Deferment") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = , fill = PELLCAT2)) +
    labs(title = "No progression") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = NUM_BBRR2_FED_UG_MAKEPROG, fill = PELLCAT2)) +
    labs(title = "Making Progression") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = NUM_BBRR2_FED_UG_PAIDINFULL, fill = PELLCAT2)) +
    labs(title = "Paid off") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

ggplot(UGLOAN2yr, aes(x = NUM_BBRR2_FED_UG_DISCHARGE, fill = PELLCAT2)) +
    labs(title = "Loans discharged") +
    geom_histogram(color = "black") +
    facet_wrap(. ~ PELLCAT2, nrow = 2, ncol = 1) +
    theme_bw()

BBRR2_FED_UG_DFLT
BBRR2_FED_UG_DLNQ
BBRR2_FED_UG_FBR
BBRR2_FED_UG_DFR
BBRR2_FED_UG_NOPROG
BBRR2_FED_UG_MAKEPROG
BBRR2_FED_UG_PAIDINFULL
BBRR2_FED_UG_DISCHARGE

BBRR2_FED_UGCOMP_DFLT
BBRR2_FED_UGCOMP_DLNQ
BBRR2_FED_UGCOMP_FBR
BBRR2_FED_UGCOMP_DFR
BBRR2_FED_UGCOMP_NOPROG
BBRR2_FED_UGCOMP_MAKEPROG
BBRR2_FED_UGCOMP_PAIDINFULL
BBRR2_FED_UGCOMP_DISCHARGE

BBRR2_FED_UGNOCOMP_DFLT
BBRR2_FED_UGNOCOMP_DLNQ
BBRR2_FED_UGNOCOMP_FBR
BBRR2_FED_UGNOCOMP_DFR
BBRR2_FED_UGNOCOMP_NOPROG
BBRR2_FED_UGNOCOMP_MAKEPROG
BBRR2_FED_UGNOCOMP_PAIDINFULL
BBRR2_FED_UGNOCOMP_DISCHARGE


# Default             = Default is the failure to repay a loan according to the terms agreed to in the promissory note. For most federal student loans, you will default if you have not made a payment in more than 270 days.  
# Deliquency          = A natural person or entity failing to perform an obligation.
# Forbearance         = A loan forbearance allows you to temporarily stop making principal payments or reduce your monthly payment amount for up to 12 months
# Deferment           = A loan deferment allows you to temporarily halt making payments on the principal  
# No Progress         =   
# Progress            =   
# Paid                =   
# Loans Discharged    =   


