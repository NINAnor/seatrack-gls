#' get_threshold
#'
#' Get the light threshold for a given logger model and mode (main or summer).
#'
#' @param model The logger model for which to get the light threshold.
#' @param mode The mode for which to get the light threshold, either "main" or "summer". Default is "main".
#' @return The light threshold value for the specified logger model and mode.
#' @keywords internal
get_threshold <- function(model, mode = c("main", "summer")) {
    mode <- match.arg(mode)
    thresholds <- logger_light_thresholds[[mode]]
    for (i in seq_along(thresholds)) {
        current_threshold <- thresholds[[i]]
        threshold_models <- current_threshold$models

        if (tolower(model) %in% tolower(threshold_models) || length(threshold_models) == 0) {
            return(current_threshold$value)
        }
    }
    stop("Error in getting threshold")
}
