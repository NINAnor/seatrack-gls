summer_thresholds <- list(
    threshold_300 = list(
        models = c(
            "c330",
            "f100",
            "c250",
            "c65",
            "w65",
            "Intigeo-P55B1-7",
            "c65_super",
            "Intigeo-P35A11-7-SGA-NOT",
            "W65A9-SEA",
            "W30A9-SEA",
            "W30A9-SEA-NOT",
            "c65_NOT",
            "c331",
            "c108",
            "LAT",
            "LAT2800S",
        ),
        value = 300
    ),
    default = list(models = c(), value = 50)
)

main_thresholds <- list(
    threshold_11 = list(models = c(
        "c330",
        "f100",
        "c250",
        "c65",
        "w65",
        "Intigeo-P55B1-7",
        "c65_super",
        "Intigeo-P35A11-7-SGA-NOT",
        "W65A9-SEA",
        "W30A9-SEA",
        "W30A9-SEA-NOT",
        "c65_NOT",
        "c331",
        "c108"
    ), value = 11),
    threshold_9 = list(
        models = c(
            "mk3006",
            "mk3005",
            "mk15",
            "mk3",
            "mk4",
            "mk5",
            "mk7",
            "mk13",
            "mk14",
        ),
        value = 9
    ),
    threshold_150 = list(models = c("LAT2000", "LAT", "LAT2800S"), value = 150),
    default = list(models = c(), value = 1)
)

logger_light_thresholds <- list(summer = summer_thresholds, main = main_thresholds)


usethis::use_data(logger_light_thresholds, overwrite = TRUE)
