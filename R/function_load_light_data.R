#' Load lightdata from files
#'
#' Loads light data from specified file paths. Will handle different file formats based on file extensions.
#'
#' @param filepaths A vector of file paths to logger data files.
#' @return A data frame containing the loaded light data.
#' @concept function
get_light_data <- function(filepaths, ...) {
    # Taken from Vegard's original script
    # Reworking some of this to use column headers would make it more readable.
    light_filepath <- filepaths[tolower(tools::file_ext(filepaths)) %in% c("lig", "lux", "csv")]
    if (length(light_filepath) == 0) {
        stop("No light data file found (expected .lig, .lux or csv file).")
    }
    file_extension <- tools::file_ext(light_filepath)
    if (all(tolower(file_extension) == "lig")) {
        all_light_data <- lapply(seq_along(light_filepath), function(i) {
            light_data <- read.table(light_filepath[i], sep = ",", skip = 1, header = FALSE, fill = TRUE)
            light_data$dtime <- light_data[, 2]
            light_data$lux <- light_data[, 4]
            return(light_data)
        })
    } else if (all(tolower(file_extension) == "lux")) {
        all_light_data <- lapply(seq_along(light_filepath), function(i) {
            light_data <- read.table(light_filepath[1], sep = "\t", header = FALSE, fill = TRUE, skip = 20)
            light_data$dtime <- light_data[, 1]
            light_data$lux <- light_data[, 2]
            light_data$V1 <- "ok"
            return(light_data)
        })
    } else if (all(tolower(file_extension) == "csv")) {
        initial_args <- list(...)
        all_light_data <- lapply(seq_along(light_filepath), function(i) {
            current_args <- initial_args
            current_args$file <- light_filepath[i]
            # Is delim specified?
            if (!"sep" %in% names(current_args)) {
                for (current_sep in c(",", ";")) {
                    if (ncol(read.csv(current_args$file, sep = current_sep, nrows = 1)) > 1) {
                        current_args$sep <- current_sep
                        break
                    }
                }
            }

            # Is header specified? If not, detect
            if (!"header" %in% names(current_args)) {
                file_peak <- read.csv(current_args$file, sep = current_args$sep, nrows = 1, header = FALSE)
                if (all(file_peak[1, ] == "char")) {
                    current_args$header <- TRUE
                } else {
                    current_args$header <- FALSE
                }
            }

            light_data <- do.call(read.csv, current_args)
            if(!current_args$header){
                names(light_data) <- c("V1", "dtime", "V3", "lux")
            }

            return(light_data)
        })
    }

    all_light_data <- do.call(rbind, all_light_data)
    all_light_data$lux <- as.numeric(gsub("\\,", ".", all_light_data$lux))
    dtime_formats <- lubridate::guess_formats(all_light_data$dtime[1], orders = c("dmyHMS", "dmy HM", "ymd HMS", "ymd HM"))
    # all_light_data$dtime <- lubridate::as_datetime(all_light_data$dtime, tz = "UTC", format = c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d/%m/%y %H:%" "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M"))

    all_light_data$dtime <- lubridate::as_datetime(all_light_data$dtime, tz = "UTC", format = unique(dtime_formats))
    all_light_data$date <- as.Date(all_light_data$dtime)
    # all_light_data$date <- date_conversion(all_light_data$date) # Not sure this function is really needed
    v_time_string <- paste("01.01.2000", format(all_light_data$dtime, "%H:%M:%S"), sep = " ")
    all_light_data$time <- strptime(v_time_string, "%d.%m.%Y %H:%M:%S", tz = "UTC")
    all_light_data$V1 <- tolower(all_light_data$V1)
    return(all_light_data)
}

#' Limit light data to calibration time windows
#'
#' @param light_data Data frame containing light data with a 'dtime' column.
#' @param logger_calibration_data Data frame containing calibration data with 'start_datetime' and 'end_datetime' columns.
#' @return A list of data frames, each containing light data limited to the corresponding calibration time window.
#' @keywords internal
limit_light_data <- function(light_data, logger_calibration_data) {
    limited_light_data <- lapply(seq_len(nrow(logger_calibration_data)), function(i) {
        start_datetime <- logger_calibration_data$start_datetime[i]
        end_datetime <- logger_calibration_data$end_datetime[i]

        return(light_data[light_data$dtime >= start_datetime & light_data$dtime <= end_datetime, ])
    })

    return(limited_light_data)
}
