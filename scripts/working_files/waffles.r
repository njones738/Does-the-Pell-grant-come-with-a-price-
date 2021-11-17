httpgd::hgd()
httpgd::hgd_browse()

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
    scale_y_continuous(labels = function(x) x * num_rows, # make this multiplyer the same as n_rows # nolint
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = on_brand_Palette[1:2]
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MINvsMAJwaffle.png", width = 5120, height = 2160, units = "px")
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
    scale_y_continuous(labels = function(x) x * num_rows, # make this multiplyer the same as n_rows # nolint
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = golds
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MIN2yrVS4yrwaffle.png", width = 5120, height = 2160, units = "px")

csc_data %>%
    filter(PELLCAT2 == "1 Majority Pell") %>%
    count(PELLCAT2, ICLEVEL2) %>%
    mutate(n = n / oneboxequals) %>%
    ggplot(aes(fill = ICLEVEL2, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 ~ ., strip.position = "bottom") +
    scale_y_continuous(labels = function(x) x * num_rows, # make this multiplyer the same as n_rows
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = greys
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MAJ2yrVS4yrwaffle.png", width = 5120, height = 2160, units = "px")

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
    scale_y_continuous(labels = function(x) x * num_rows, # make this multiplyer the same as n_rows
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = golds2
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MINpubVSprivwaffle.png", width = 5120, height = 2160, units = "px")


csc_data %>%
    filter(PELLCAT2 == "1 Majority Pell") %>%
    count(PELLCAT2, ICLEVEL2, CONTROL2) %>%
    mutate(n = n / oneboxequals,
           NAME = seq(1,4,1)) %>%
    ggplot(aes(fill = NAME, values = n)) +
    geom_waffle(n_rows = num_rows, flip = TRUE, show.legend = T,
                colour = "white", size = 1.5,
                ) +
    facet_wrap(ICLEVEL2 + CONTROL2 ~ ., strip.position = "bottom",
               nrow = 1, ncol = 4) +
    scale_y_continuous(labels = function(x) x * num_rows, # make this multiplyer the same as n_rows
                        expand = c(0,0)) +
    scale_fill_manual(
        name = NULL,
        values = greys2
    ) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle()
ggsave("images/MAJpubVSprivwaffle.png", width = 5120, height = 2160, units = "px")










