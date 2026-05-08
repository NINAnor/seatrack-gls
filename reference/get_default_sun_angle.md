# Get default sun angle This function retrieves the default sun angle based on the specified type and model.

Get default sun angle This function retrieves the default sun angle
based on the specified type and model.

## Usage

``` r
get_default_sun_angle(type = "main", model = "")
```

## Arguments

- type:

  A string indicating the type of sun angle to retrieve. Options are
  `"general"`, `"summer"`, or `"winter"`.

- model:

  A string indicating the logger model. If the model is "LAT" or
  "LAT2800S", it retrieves the default sun angle specific to those
  models. Default is an empty string.

## Value

A numeric value representing the default sun angle corresponding to the
specified type and model
