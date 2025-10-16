### må legge til funksjonen en måte å forstå median sun angle for de samme periodene.
assign_equinox_periods <- function(lats, dates, breedingloc_lat, sun) {
    equinox_table <- read.table(system.file("equinox_table", "equinox_table.txt", package = "seatrackGLS"), header = TRUE)

    DOY <- as.numeric(strftime(dates, format = "%j"))
    feb_march1_sun <- NA
    feb_march1_sun <- median(sun[DOY %in% c(41:45)])
    march_april1_sun <- NA
    march_april1_sun <- median(sun[DOY %in% c(113:117)])
    august_september1_sun <- NA
    august_september1_sun <- median(sun[DOY %in% c(227:231)])
    september_october1_sun <- NA
    september_october1_sun <- median(sun[DOY %in% c(300:304)])

    if (is.na(feb_march1_sun)) feb_march1_sun <- march_april1_sun
    if (is.na(feb_march1_sun)) feb_march1_sun <- sun[1]
    if (is.na(march_april1_sun)) march_april1_sun <- feb_march1_sun
    if (is.na(march_april1_sun)) march_april1_sun <- sun[1]
    if (is.na(august_september1_sun)) august_september1_sun <- september_october1_sun
    if (is.na(august_september1_sun)) august_september1_sun <- sun[1]
    if (is.na(september_october1_sun)) september_october1_sun <- august_september1_sun
    if (is.na(september_october1_sun)) september_october1_sun <- sun[1]

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - feb_march1_sun) == min(abs(unique(equinox_table$sun_angle) - feb_march1_sun)))]
    new_start_DOY <- min(equinox_table$start_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    feb_march2_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march2_sun)) feb_march2_sun <- feb_march1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - feb_march2_sun) == min(abs(unique(equinox_table$sun_angle) - feb_march2_sun)))]
    new_start_DOY <- min(equinox_table$start_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    feb_march_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march_sun)) feb_march_sun <- feb_march1_sun
    feb_march_lat <- NA
    feb_march_lat <- median(lats[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march_lat)) feb_march_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - march_april1_sun) == min(abs(unique(equinox_table$sun_angle) - march_april1_sun)))]
    new_start_DOY <- max(equinox_table$end_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    march_april2_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april2_sun)) march_april2_sun <- feb_march1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - march_april2_sun) == min(abs(unique(equinox_table$sun_angle) - march_april2_sun)))]
    new_start_DOY <- max(equinox_table$end_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    march_april_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april_sun)) march_april_sun <- march_april1_sun
    march_april_lat <- NA
    march_april_lat <- median(lats[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april_lat)) march_april_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - august_september1_sun) == min(abs(unique(equinox_table$sun_angle) - august_september1_sun)))]
    new_start_DOY <- min(equinox_table$start_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    august_september2_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september2_sun)) august_september2_sun <- august_september1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - august_september2_sun) == min(abs(unique(equinox_table$sun_angle) - august_september2_sun)))]
    new_start_DOY <- min(equinox_table$start_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    august_september_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september_sun)) august_september_sun <- august_september1_sun
    august_september_lat <- NA
    august_september_lat <- median(lats[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september_lat)) august_september_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - september_october1_sun) == min(abs(unique(equinox_table$sun_angle) - september_october1_sun)))]
    new_start_DOY <- max(equinox_table$end_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    september_october2_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october2_sun)) september_october2_sun <- september_october1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - september_october2_sun) == min(abs(unique(equinox_table$sun_angle) - september_october2_sun)))]
    new_start_DOY <- max(equinox_table$end_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    september_october_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october_sun)) september_october_sun <- september_october1_sun
    september_october_lat <- NA
    september_october_lat <- median(lats[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october_lat)) september_october_lat <- breedingloc_lat



    aut_start_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - august_september_lat))]
    aut_start_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - august_september_sun))]
    start_aut <- equinox_table$start_aut_eq[equinox_table$sun_angle %in% aut_start_sun & equinox_table$latitude %in% aut_start_lat]

    end_aut_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - september_october_lat))]
    end_aut_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - september_october_sun))]
    end_aut <- equinox_table$end_aut_eq[equinox_table$sun_angle %in% end_aut_sun & equinox_table$latitude %in% end_aut_lat]

    start_spring_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - feb_march_lat))]
    start_spring_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - feb_march_sun))]
    start_spring <- equinox_table$start_spring_eq[equinox_table$sun_angle %in% start_spring_sun & equinox_table$latitude %in% start_spring_lat]

    end_spring_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - march_april_lat))]
    end_spring_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - march_april_sun))]
    end_spring <- equinox_table$end_spring_eq[equinox_table$sun_angle %in% end_spring_sun & equinox_table$latitude %in% end_spring_lat]



    df <- as.data.frame(DOY)
    colnames(df) <- c("DOY")
    df$eqfilter <- 1
    df$eqfilter[df$DOY >= (start_aut + 1) & df$DOY <= (end_aut - 1)] <- 0
    df$eqfilter[df$DOY >= (start_spring + 1) & df$DOY <= (end_spring - 1)] <- 0

    output <- df$eqfilter
    return(output)
}
