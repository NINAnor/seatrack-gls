plot_seasonal_points <- function(df) {
    # Set up axis for entire length of track
    plot(NA, NA, xlim = c(min(df$date_time), max(df$date_time)), ylim = c(min(df$lat), max(df$lat)), xlab = "Date", ylab = "Lattitude")

    line_cols <- list(main = "black", winter = "blue", summer = "red")

    # Create segments when point_type category changes
    df$segment <- cumsum(c(1, diff(as.numeric(factor(df$point_type))) != 0))

    # Add lines by point_type
    for (point_type in unique(df$point_type)) {
        # Get data for this point type, split into segments where point_type is the same
        point_data <- df[df$point_type == point_type, ]

        segments <- split(point_data, point_data$segment)
        for (segment in segments) {
            lines(segment$date_time, segment$lat, col = line_cols[[point_type]], lwd = 2)
            points(segment$date_time, segment$lat, col = line_cols[[point_type]], pch = 16, cex = 0.5)
        }
    }

    # Add legend
    legend("topright", legend = names(line_cols), col = unlist(line_cols), lwd = 2)
}
