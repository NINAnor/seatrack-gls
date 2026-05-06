#' Get sun angle
#' This function retrieves the sun angle sequence based on the specified type and model.
#' @param type A string indicating the type of sun angle sequence to retrieve. Options are `"general"`, `"summer"`, or `"winter"`.
#' @param model A string indicating the logger model. If the model is "LAT" or "LAT2800S", it retrieves the sun angles specific to those models. Default is an empty string.
#' @return A numeric vector containing the sun angle sequence corresponding to the specified type and model
#' @keywords internal
#' @export
get_sun_angle <- function(type = "general", model = "") {
    if (type == "main") {
        type <- "general"
    }
    if (tolower(model) %in% tolower(c("LAT", "LAT2800S"))) {
        angles <- seatrackRgls::sun_angles_LAT[[type]]
    } else {
        angles <- seatrackRgls::sun_angles[[type]]
    }

    return(angles)
}

#' Get default sun angle
#' This function retrieves the default sun angle based on the specified type and model.
#' @param type A string indicating the type of sun angle to retrieve. Options are `"general"`, `"summer"`, or `"winter"`.
#' @param model A string indicating the logger model. If the model is "LAT" or "LAT2800S", it retrieves the default sun angle specific to those models. Default is an empty string.
#' @return A numeric value representing the default sun angle corresponding to the specified type and model
#' @keywords internal
#' @export
get_default_sun_angle <- function(type = "main", model = "") {
    if (type == "main") {
        if (tolower(model) %in% tolower(c("LAT", "LAT2800S"))) {
            angle <- -8
        } else {
            angle <- -3.5
        }
    } else if (type == "summer") {
        angle <- 0
    } else if (type == "winter") {
        angle <- -5
    }

    return(angle)
}
