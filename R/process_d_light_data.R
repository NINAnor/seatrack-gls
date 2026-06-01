#' Load and process a logger/year combination's light files
#'
#' Given a set of filepaths for light data files, calibration data, filter settings, colony info and extra metadata,
#' this function processes the light data to estimate positions and apply various filters depending on whether calibration mode is active or not.
#'
#' @param filepaths A vector of file paths to the light data files.
#' @param logger_calibration_data A data frame containing calibration data for the logger. If multiple calibration windows are provided, each will be processed in sequence.
#' @param filter_setting_list A list of filter settings for different species. Defaults to `seatrack_settings_list`.
#' @param logger_colony_info A data frame containing colony information for the logger.
#' @param all_light_data An optional data frame containing all light data for the logger. If not provided, light data will be loaded from the filepaths.
#' @param logger_extra_metadata A data frame containing extra metadata for the logger.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param plotting_dir An optional directory path to save plotting outputs. Defaults to NULL.
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to TRUE.
#' @param min_length Number indicating minimum length of light data. Anything below this will fail. Defaults to 40.
#' @param do_seasonal_calibration A logical indicating whether to perform seasonal calibration. Defaults to NULL, which uses the filter_settings_list.
#' @param stop_on_error A logical indicating whether to stop processing if an error occurs. Defaults to FALSE.
#'
#' @concept processing
#' @return If calibration_mode is FALSE, returns a list containing:
#'          - `twilight_estimates`: A data frame of twilight estimates.
#'          - `posdata_export`: A data frame of processed position data.
#'          - `filtering`: A data frame summarizing the filtering steps applied.
#' If calibration_mode is TRUE, returns data frame of default calibration outputs and exports calibration plots.
process_logger_light_data <- function(
    filepaths,
    logger_calibration_data,
    filter_setting_list,
    logger_colony_info,
    all_light_data = NULL,
    logger_extra_metadata = NULL,
    show_filter_plots = FALSE,
    plotting_dir = NULL,
    calibration_mode = TRUE,
    min_length = 40,
    do_seasonal_calibration = NULL,
    stop_on_error = FALSE) {
    # create dir for plotting
    if (!is.null(plotting_dir) && !dir.exists(plotting_dir)) {
        dir.create(plotting_dir, recursive = TRUE)
    }

    if (is.null(all_light_data)) {
        print("Load light data...")
        all_light_data <- tryCatch(
            {
                get_light_data(filepaths)
            },
            error = function(e) {
                print(paste("Error loading file:", e))
                return(NULL)
            }
        )
    }
    if (!is.null(all_light_data)) {
        print("Limit light data to calibration time windows...")
        light_data_split <- limit_light_data(all_light_data, logger_calibration_data)
    } else {
        light_data_split <- rep(list(data.frame()), nrow(logger_calibration_data))
    }


    all_results <- list()

    for (i in seq_along(light_data_split)) {
        light_data <- light_data_split[[i]]
        light_data_calibration <- logger_calibration_data[i, ]
        logger_filter <- filter_setting_list$get_settings_from_list(species = light_data_calibration$species[1], colony = light_data_calibration$colony[1], logger_id = light_data_calibration$logger_id[1], years_tracked = light_data_calibration$year_tracked[1])

        print(paste(light_data_calibration$logger_id[1], "- Processing calibration window", i, "of", nrow(logger_calibration_data), "-", light_data_calibration$year_tracked[1]))

        if (nrow(light_data) < min_length) {
            print(paste("Light data has only", nrow(light_data), "rows, skipping."))
            if (calibration_mode) {
                result <- light_data_calibration
                result <- add_default_cols(result)
                result$problem <- TRUE
            }else {
                result <- NULL
            }
        } else {
            result <- tryCatch(
                {
                    process_result <- apply_filters(
                        light_data = light_data,
                        light_data_calibration = light_data_calibration,
                        logger_filter = logger_filter,
                        logger_colony_info = logger_colony_info,
                        logger_extra_metadata = logger_extra_metadata,
                        show_filter_plots = show_filter_plots,
                        plotting_dir = plotting_dir,
                        calibration_mode = calibration_mode,
                        stop_on_error = stop_on_error,
                        do_seasonal_calibration = do_seasonal_calibration
                    )
                    if (calibration_mode && (is.null(process_result) || nrow(process_result) == 0)) {
                        stop("No results returned")
                    }
                    if (calibration_mode) {
                        process_result$problem <- FALSE
                    }
                    process_result
                },
                error = function(e) {
                    print(paste("Error in processing:", e))
                    if (stop_on_error) {
                        stop(e)
                    }
                    if (calibration_mode) {
                        process_result <- logger_calibration_data[i, ]
                        process_result <- add_default_cols(process_result)
                        process_result$problem <- TRUE
                        return(process_result)
                    } else {
                        return(NULL)
                    }
                }
            )
        }
        print(paste("Finished processing calibration window", i, "of", nrow(logger_calibration_data)))
        result <- result[!sapply(result, is.null)]
        all_results <- c(all_results, list(result))
    }

    # Handle results dependng on calibration mode
    if (calibration_mode == FALSE) {
        # Combine results
        combined_twilight_estimates <- do.call(rbind, lapply(all_results, function(x) x$twilight_estimates))
        combined_posdata_export <- do.call(rbind, lapply(all_results, function(x) x$posdata_export))
        if (!is.null(combined_posdata_export)) {
            combined_posdata_export$raw_data_file <- basename(filepaths[1])
        }

        filtering_list <- lapply(all_results, function(x) x$filtering)
        filtering_list <- filtering_list[!sapply(filtering_list, is.null)]
        if (length(filtering_list) == 0) {
            combined_filtering <- NULL
        } else {
            all_filtering_cols <- unique(unlist(lapply(filtering_list, names)))
            filtering_list <- lapply(filtering_list, function(df) {
                missing_cols <- setdiff(all_filtering_cols, names(df))
                if (length(missing_cols)) df[missing_cols] <- NA
                df[all_filtering_cols]
            })
            combined_filtering <- do.call(rbind, filtering_list)
        }

        return(list(
            twilight_estimates = combined_twilight_estimates,
            posdata_export = combined_posdata_export,
            filtering = combined_filtering
        ))
    } else {
        # In calibration mode, combine the new calibration dataframe
        combined_calibration <- do.call(rbind, all_results)
        return(combined_calibration)
    }
}
