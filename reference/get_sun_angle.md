# Get sun angle This function retrieves the sun angle sequence based on the specified type and model.

Get sun angle This function retrieves the sun angle sequence based on
the specified type and model.

## Usage

``` r
get_sun_angle(type = "general", model = "")
```

## Arguments

- type:

  A string indicating the type of sun angle sequence to retrieve.
  Options are `"general"`, `"summer"`, or `"winter"`.

- model:

  A string indicating the logger model. If the model is "LAT" or
  "LAT2800S", it retrieves the sun angles specific to those models.
  Default is an empty string.

## Value

A numeric vector containing the sun angle sequence corresponding to the
specified type and model
