###############################################
###############################################
###    Figure 1                             ###
###############################################
###############################################
csc_data %>%
       select(INSTNM, STABBR, PELLCAT, DEPPLUS_DEBT_MDN,
              DEP_DEBT_MDN, PLUS_DEBT_INST_MD, IND_DEBT_MDN) %>%
       group_by(STABBR) %>%
       summarise(med_DEPPLUS = median(DEPPLUS_DEBT_MDN, na.rm = T),
                 med_IND = median(IND_DEBT_MDN, na.rm = T)) %>%
       ggplot() +
              geom_col(aes(x = med_DEPPLUS,
                           y = reorder(STABBR, med_DEPPLUS)),
                       fill = "#b0b3b2") +
              geom_col(aes(x = med_IND,
                           y = reorder(STABBR, med_DEPPLUS)),
                       fill = "#ffc629") +
       geom_vline(aes(xintercept = median(csc_data$IND_DEBT_MDN, na.rm = T)),
                  linetype = "dashed",
                  lwd = 0.8,
                  color = "red",
                  show.legend = F) +
       geom_vline(aes(xintercept = 20000),
                  linetype = "dashed",
                  lwd = 0.8,
                  color = "blue",
                  show.legend = F) +
       theme_bw()
ggsave("images/barchart_DEPPLUS.png",
       width = 5120, height = 2160, units = "px")

###############################################
###############################################
###    Figure 2                             ###
###############################################
###############################################

# See the file labelled UnitedState_spatialGraphic.r

###############################################
###############################################
###    Figure 3                             ###
###############################################
###############################################
mu <- plyr::ddply(GCETU_rate,
                  c("PELLCAT2"),
                  summarise,
                  grp.median = median(OMENRUP_ALL, na.rm = T))
ggplot(GCETU_rate, aes(x = OMENRUP_ALL,
                       fill = PELLCAT2)) + # dropout
    geom_histogram(aes(y = ..count..),
                   binwidth = 0.04,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 500)) +
    geom_vline(data = mu,
               aes(xintercept = grp.median,
                   color = PELLCAT2),
               linetype = "dashed",
               lwd = 0.8,
               show.legend = F) +
    stat_bin(aes(y = ..count..,
                 label = round(..count.., 2)),
             binwidth = 0.04,
             geom = "text",
             color = "black",
             vjust = -0.5) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()
ggsave("images/dropoutrate.png", width = 5120, height = 2160, units = "px")

mu2 <- plyr::ddply(GCETU_rate, c("PELLCAT2","ICLEVEL2"), summarise, grp.median=median(OMENRUP_ALL, na.rm = T))
ggplot(GCETU_rate, aes(x = OMENRUP_ALL,
                       fill = PELLCAT2)) + # dropout
    geom_histogram(aes(y = ..count..),
                   binwidth = 0.08,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 480)) +
    geom_vline(data = mu2,
               aes(xintercept = grp.median,
                   color = ICLEVEL2),
               linetype = "dashed",
               lwd = 0.8,
               show.legend = F) +
    stat_bin(aes(y = ..count..,
                 label = round(..count.., 2)),
             binwidth = 0.08,
             geom = "text",
             color = "black",
             vjust = -0.5) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/dropoutrateby2yrVS4yr.png",
       width = 5120, height = 2160, units = "px")

###############################################
###############################################
###    Figure 4                             ###
###############################################
###############################################
mu <- plyr::ddply(GCETU_rate,
                  c("PELLCAT2"),
                  summarise,
                  grp.median = median(OMAWDP8_ALL * 100, na.rm = T))
ggplot(GCETU_rate, aes(x = OMAWDP8_ALL * 100, fill = PELLCAT2)) +
    geom_histogram(binwidth = 0.035 * 100,
                   color = "black",
                   closed = "right") +
    scale_x_continuous(breaks = seq(0, 1, 0.1) * 100) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 350)) +
    stat_bin(aes(y = ..count.., label = ..count..),
             binwidth = 0.035 * 100,
             geom = "text",
             vjust = -0.5) +
    geom_vline(data = mu,
               aes(xintercept = grp.median,
               color = PELLCAT2),
               linetype = "dashed",
               lwd = 0.8,
               show.legend = F) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()
ggsave("images/PELLCATvsGRAD.png",
       width = 5120, height = 2160, units = "px")
##############
mu <- plyr::ddply(GCETU_rate,
                  c("PELLCAT2","ICLEVEL2","CONTROL2"),
                  summarise, 
                  grp.median = median(OMAWDP8_ALL * 100, na.rm = T))
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
ggsave("images/PELLCATvsGRAD_2yrVS4yrVSpubVSpriv.png",
       width = 5120, height = 2160, units = "px")

###############################################
###############################################
###    Figure 5                             ###
###############################################
###############################################
num_rows <- 5
oneboxequals <- 20

csc_data %>%
    count(PELLCAT2) %>%
    mutate(n = n / oneboxequals) %>%
    ggplot(aes(fill = PELLCAT2, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(PELLCAT2 ~ ., strip.position = "bottom") +
    scale_y_continuous(labels = function(x) x * num_rows, # nolint
                        expand = c(0, 0)) +
    scale_fill_manual(
        name = NULL,
        values = on_brand_Palette[1:2]
    ) +
    coord_equal() +
    theme_ipsum_rc(grid = "") +
    theme_enhance_waffle()
ggsave("images/MINvsMAJwaffle.png",
       width = 5120, height = 2160, units = "px")
##############################################
csc_data %>%
    filter(PELLCAT2 == "0 Minority Pell") %>%
    count(PELLCAT2, ICLEVEL2) %>%
    mutate(n = n / oneboxequals) %>%
    ggplot(aes(fill = ICLEVEL2, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 ~ ., strip.position = "bottom") +
    scale_y_continuous(labels = function(x) x * num_rows, # nolint
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = golds
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MIN2yrVS4yrwaffle.png",
       width = 5120, height = 2160, units = "px")

csc_data %>%
    filter(PELLCAT2 == "1 Majority Pell") %>%
    count(PELLCAT2, ICLEVEL2) %>%
    mutate(n = n / oneboxequals) %>%
    ggplot(aes(fill = ICLEVEL2, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 ~ ., strip.position = "bottom") +
    scale_y_continuous(labels = function(x) x * num_rows,
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = greys
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MAJ2yrVS4yrwaffle.png",
       width = 5120, height = 2160, units = "px")

csc_data %>%
    filter(PELLCAT2 == "0 Minority Pell") %>%
    count(PELLCAT2, ICLEVEL2, CONTROL2) %>%
    mutate(n = n / oneboxequals,
           NAME = seq(1,4,1)) %>%
    ggplot(aes(fill = NAME, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 + CONTROL2 ~ ., strip.position = "bottom",
               nrow = 1, ncol = 4) +
    scale_y_continuous(labels = function(x) x * num_rows,
                        expand = c(0, 0)) +
    scale_fill_manual(
        name = NULL,
        values = golds2
    ) +
    coord_equal() +
    theme_ipsum_rc(grid = "") +
    theme_enhance_waffle()
ggsave("images/MINpubVSprivwaffle.png",
       width = 5120, height = 2160, units = "px")


csc_data %>%
    filter(PELLCAT2 == "1 Majority Pell") %>%
    count(PELLCAT2, ICLEVEL2, CONTROL2) %>%
    mutate(n = n / oneboxequals,
           NAME = seq(1, 4, 1)) %>%
    ggplot(aes(fill = NAME, 
               values = n)) +
    geom_waffle(n_rows = num_rows,
                flip = TRUE,
                show.legend = T,
                colour = "white",
                size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 + CONTROL2 ~ .,
               strip.position = "bottom",
               nrow = 1,
               ncol = 4) +
    scale_y_continuous(labels = function(x) x * num_rows,
                        expand = c(0, 0)) +
    scale_fill_manual(
        name = NULL,
        values = greys2
    ) +
    coord_equal() +
    theme_ipsum_rc(grid = "") +
    theme_enhance_waffle()
ggsave("images/MAJpubVSprivwaffle.png",
       width = 5120, height = 2160, units = "px")



###############################################
###############################################
###    Figure 6                             ###
###############################################
###############################################
CSC18_GRAD <- csc_data %>%
                filter(is.na(GRAD_CAT) == F) %>%
                filter(is.na(BBRR2_FED_UG_DFLT) == F) %>%
                mutate(GRAD_CAT = case_when(GRAD_CAT == 3 ~ "> 75% and <= 100",
                                            GRAD_CAT == 2 ~ "> 50% and <= 75",
                                            GRAD_CAT == 1 ~ "> 25% and <= 50",
                                            GRAD_CAT == 0 ~ "<= 25%"))
Cat_var <- CSC18_GRAD$GRAD_CAT

med <- plyr::ddply(CSC18_GRAD,
                   "GRAD_CAT",
                   summarise,
                   grp.median = median(BBRR2_FED_UG_DFLT))
Zero_data_GRAD <- subset(CSC18_GRAD,
                         subset = CSC18_GRAD$GRAD_CAT == "<= 25%") # <  30%
One_data_GRAD  <- subset(CSC18_GRAD,
                         subset = CSC18_GRAD$GRAD_CAT == "> 25% and <= 50") # >= 30% # nolint
Two_data_GRAD  <- subset(CSC18_GRAD,
                         subset = CSC18_GRAD$GRAD_CAT == "> 50% and <= 75") # >  65% # nolint
Three_data_GRAD  <- subset(CSC18_GRAD,
                           subset = CSC18_GRAD$GRAD_CAT == "> 75% and <= 100") # >  65% # nolint
Quant_var <- CSC18_GRAD$BBRR2_FED_UG_DFLT

G0 <- Zero_data_GRAD$BBRR2_FED_UG_DFLT
G1 <- One_data_GRAD$BBRR2_FED_UG_DFLT
G2 <- Two_data_GRAD$BBRR2_FED_UG_DFLT
G3 <- Three_data_GRAD$BBRR2_FED_UG_DFLT

ggplot(CSC18_GRAD, aes(x = BBRR2_FED_UG_DFLT,
                       fill = GRAD_CAT)) +
       geom_histogram(binwidth = 0.034,
                      color = "black",
                      closed = "right") +
       scale_x_continuous(breaks = seq(0, 1, 0.034)) + 
       scale_fill_manual(name = "group",
                         values = on_brand_Palette[1:4],
                         labels = c("< 25%",">= 25% <= 50%","> 50 <= 75","> 75%")) + # nolint
       facet_grid(GRAD_CAT ~ .) +
       xlab("Percentage of BBRR2_FED_UG_DFLT.(BBRR2_FED_UG_DFLT)") +
       labs(title = "Figure 1: Histogram of BBRR2_FED_UG_DFLT stratified by the categories of GRAD_CAT. (n = 5065)") + # nolint
       geom_vline(data = med,
                  aes(xintercept = grp.median,
                      color = GRAD_CAT),
                  linetype = "dashed",
                  lwd = 1.1,
                  show.legend = F) +
       stat_bin(aes(y = ..count..,
                    label = ..count..),
                binwidth = 0.034,
                geom = "text",
                vjust = -0.8) +
       theme_bw()
ggsave("images/GRAD_CATvsDEFAULT",
       width = 5120, height = 2160, units = "px")


###############################################
###############################################
###    Figure 7                             ###
###############################################
###############################################
mu3 <- plyr::ddply(UGLOAN2yr,
                   c("PELLCAT2"),
                   summarise,
                   grp.median = median(BBRR2_FED_UG_DFLT, na.rm = T))
ggplot(UGLOAN2yr,
       aes(x = BBRR2_FED_UG_DFLT * 100,
           fill = PELLCAT2)) +
    geom_histogram(aes(y = ..count..),
                   binwidth = .03 * 100,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks = seq(0, 1, 0.1) * 100) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 600)) +
    stat_bin(aes(y = ..count..,
             label = round(..count.., 2)),
             binwidth = 0.03 * 100,
             geom = "text", color = "black",
             vjust = -0.5) +
    geom_vline(data = mu3,
        aes(xintercept = grp.median * 100,
            color = PELLCAT2),
        linetype = "dashed",
        lwd = 0.8,
        show.legend = F) +
    facet_grid(PELLCAT2 ~ .) +
    theme_bw()
ggsave("images/defaultrate.png",
       width = 5120, height = 2160, units = "px")

mu3 <- plyr::ddply(UGLOAN2yr,
                   c("PELLCAT2", "ICLEVEL2"),
                   summarise,
                   grp.median = median(BBRR2_FED_UG_DFLT, na.rm = T))
ggplot(UGLOAN2yr,
       aes(x = BBRR2_FED_UG_DFLT * 100,
           fill = PELLCAT2)) +
    geom_histogram(aes(y = ..count..),
                   binwidth = .03 * 100,
                   color = "black",
                   closed = "right",
                   position = "identity") +
    scale_x_continuous(breaks = seq(0, 1, 0.1) * 100) +
    scale_fill_manual(name = "group",
                      values = on_brand_Palette[1:4],
                      labels = c("0", "1")) +
    scale_y_continuous(limits = c(0, 600)) +
    stat_bin(aes(y = ..count..,
             label = round(..count.., 2)),
             binwidth = 0.03 * 100,
             geom = "text", color = "black",
             vjust = -0.5) +
    geom_vline(data = mu3,
        aes(xintercept = grp.median * 100,
            color = PELLCAT2),
        linetype = "dashed",
        lwd = 0.8,
        show.legend = F) +
    facet_grid(PELLCAT2 ~ ICLEVEL2) +
    theme_bw()
ggsave("images/defaultrate2yrVS4yr.png",
       width = 5120, height = 2160, units = "px")
