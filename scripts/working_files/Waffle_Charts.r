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

# R program for getting missing values
see_missing <- function(df) {
    as_tibble(lapply(df, function(x) sum(!is.na(x)))) %>%
            pivot_longer(cols = everything(),
                         names_to = "Variable", values_to = "Count")
                            }

csc_data <- csc %>%
                select(UNITID, INSTNM, PELLCAT, ICLEVEL, CONTROL, STABBR) %>%
                mutate(CONTROL2 = case_when(CONTROL == "1" ~ "Public",
                                            CONTROL == "2" ~ "Private",
                                            CONTROL == "3" ~ "Private"),
                       ICLEVEL2 = case_when(ICLEVEL == "1" ~ "4-year",
                                            ICLEVEL == "2" ~ "2-year",
                                            ICLEVEL == "3" ~ "2-year"),
                       PELLCAT = case_when(PELLCAT == 0 ~ "Minority Pell",
                                           PELLCAT == 1 ~ "Majority Pell")) %>%
                filter(!is.na(PELLCAT))

csc_data %>%
    count(CONTROL2, PELLCAT) %>%
    mutate(NAME = c(seq(1,4,1))) %>%
    ggplot(aes(fill = NAME, values = n)) +
        geom_waffle() +
  scale_fill_manual(name = NULL,
                    values = c("#6c0075", "#d18589","#b805c9", "#c21d25"),
                    labels = c("Public Minority Pell", "Public Majority Pell", 
                               "Private Minority Pell","Private Majority Pell")) +
  theme_void()

csc_data %>%
    filter(CONTROL2 == "Public") %>% # Public post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#dbdbdb", show.legend = T, 
                    radius = unit(2, "pt"),
                    size = .1, n_rows = 25, flip = T) +
        facet_wrap(PELLCAT ~ .,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 50, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))

csc_data %>% 
    filter(CONTROL2 == 2) %>% # Private post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#dbdbdb", show.legend = T, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(PELLCAT ~ ., nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))
csc_data %>%
    filter(CONTROL2 == 2) %>% # Private post-secondary schools
    count(PELLCAT, ICLEVEL2) %>% # 0 is minority pell :: 1 is majority pell
    mutate(NAME = c(seq(1,4,1))) %>%
    ggplot(aes(fill = NAME, values = n)) +
        geom_waffle(color = "#dbdbdb", show.legend = T, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(PELLCAT ~ ., nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2",
                                     "#ffc629", "#b0b3b2"),
                          labels = c("4-year Minority", "2-year Minority",
                                     "4-year Majority", "2-year Majority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))
#################################################

fouryear <- csc_data %>%
    filter(ICLEVEL2 == 1) %>% # 4-year post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#3a3a3a", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(. ~ PELLCAT, nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))

twoyear <- csc_data %>%
    filter(ICLEVEL2 == 2) %>% # 4-year post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#3a3a3a", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(. ~ PELLCAT, nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))
(fouryear | twoyear)
ggsave("images/fouryearTHENtwoyear.png", width = 5120, height = 2160, units = "px")

######################################################

public <- csc_data %>%
    filter(CONTROL2 == 1) %>% # 4-year post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#3a3a3a", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(. ~ PELLCAT, nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))
public
ggsave("images/public.png", width = 5120, height = 2160, units = "px")

private <- csc_data %>%
    filter(CONTROL2 == 2) %>% # 4-year post-secondary schools
    count(PELLCAT) %>% # 0 is minority pell :: 1 is majority pell
    ggplot(aes(fill = PELLCAT, values = n)) +
        geom_waffle(color = "#3a3a3a", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(. ~ PELLCAT, nrow = 1, 
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0,0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2"),
                          labels = c("Minority", "Minority")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
        guides(fill = guide_legend(reverse = TRUE))
private
ggsave("images/private.png", width = 5120, height = 2160, units = "px")

######################################################
csc_data %>%
    count(PELLCAT, CONTROL2, ICLEVEL2) %>% # 0 is minority pell :: 1 is majority pell
    mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = NAME, values = n)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(PELLCAT + CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2",
                                     "#ffc629", "#b0b3b2",
                                     "#ffc629", "#b0b3b2",
                                     "#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat")) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/fouryearTHENtwoyearANDANDpublicTHENprivate.png", width = 5120, height = 2160, units = "px")
######################################################################################################




csc_data %>%
    select(PELLCAT) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = PELLCAT, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(PELLCAT ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/majorityVSminority.png", width = 5120, height = 2160, units = "px")
######################################################################################################

csc_data %>%
    select(PELLCAT, ICLEVEL2) %>%
    filter(PELLCAT == 0) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = ICLEVEL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(ICLEVEL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/minority_4yrVS2year.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    select(PELLCAT, ICLEVEL2) %>%
    filter(PELLCAT == 1) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = ICLEVEL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(ICLEVEL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/majority_4yrVS2year.png", width = 5120, height = 2160, units = "px")
######################################################################################################

csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == 0) %>%
    filter(ICLEVEL2 == 1) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/minority4yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == 1) %>%
    filter(ICLEVEL2 == 1) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/majority4yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == 0) %>%
    filter(ICLEVEL2 == 2) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/minority2yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == 1) %>%
    filter(ICLEVEL2 == 2) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/majority2yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")


csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == "Minority Pell") %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(ICLEVEL2 + CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/minority2yrAND4yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    filter(PELLCAT == "Majority Pell") %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    #mutate(NAME = as.character(seq(1, 8, 1))) %>%
    ggplot(aes(fill = CONTROL2, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(ICLEVEL2 + CONTROL2 ~ ., nrow = 1, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#ffc629", "#b0b3b2")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/majority2yrAND4yr_publicVSprivate.png", width = 5120, height = 2160, units = "px")


csc_data %>%
    select(PELLCAT, ICLEVEL2, CONTROL2) %>%
    count() %>% # 0 is minority pell :: 1 is majority pell
    mutate(NAME = as.character(seq(1, 8, 1))) %>%
    arrange(PELLCAT) %>%
    ggplot(aes(fill = PELLCAT, values = freq)) +
        geom_waffle(color = "#dbdbdb", show.legend = F, 
                    radius = unit(2, "pt"),
                    size = .25, n_rows = 30, flip = T) +
        facet_wrap(PELLCAT ~ ICLEVEL2 + CONTROL2, nrow = 2, ncol = 4, shrink = F,
                   strip.position = "bottom") +
        scale_x_discrete() + 
        scale_y_continuous(labels = function(x) x * 30, # make this multiplyer the same as n_rows
                            expand = c(0, 0)) +
        scale_fill_manual(name = NULL,
                          values = c("#b0b2b3", "#ffc629")
                          ) +
        coord_equal() +
        theme_minimal(base_family = "Roboto Condensed") +
        theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
              strip.text = element_text(family = "Montserrat"),
              axis.text.y = element_text(angle = 90)) +
        guides(fill = guide_legend(reverse = TRUE))
ggsave("images/waffle2.png", width = 5120, height = 2160, units = "px")













