plot_seasonal_points <- function(df) {
    # Set up axis for entire length of track
    plot(NA, NA,
        xlim = c(min(df$date_time), max(df$date_time)), ylim = c(min(df$lat), max(df$lat) + 10),
        xlab = "Date", ylab = "Lattitude", cex.axis = 0.6, cex.lab = 0.7, xaxt = "n"
    )
    axis.POSIXct(1, at = seq(min(df$date_time), max(df$date_time), by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))

    line_cols <- list(main = "black", winter = "blue", summer = "red")

    # Create segments when point_type category changes
    df$segment <- cumsum(c(1, diff(as.numeric(factor(df$point_type))) != 0))

    # Add lines by point_type
    for (point_type in unique(df$point_type)) {
        # Get data for this point type, split into segments where point_type is the same
        point_data <- df[df$point_type == point_type, ]

        segments <- split(point_data, point_data$segment)
        for (segment_i in unique(point_data$segment)) {
            point_segment <- df[df$segment == segment_i, ]
            line_segment_index <- which(df$segment == segment_i)
            line_segment_index <- unique(c(max(line_segment_index[1] - 1, 1), line_segment_index)) # Add the previous point to the segment for line continuity
            line_segment <- df[line_segment_index, ]
            lines(line_segment$date_time, line_segment$lat, col = line_cols[[point_type]], lwd = 1.5)
            points(point_segment$date_time, point_segment$lat, col = line_cols[[point_type]], pch = 16, cex = 0.3)
        }
    }

    # Add legend
    legend("top", legend = names(line_cols), col = unlist(line_cols), lwd = 2, horiz = TRUE, pch = 19, cex = 0.3)
}
