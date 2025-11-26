# Summarise coil inductance versus placement for design work

This function evaluates
[`loading_coil_inductance()`](https://yojimbodurant.github.io/loadedDipole/reference/loading_coil_inductance.md)
for a set of coil positions expressed as fractions of the dipole
half-length. It is useful when exploring design trade-offs between coil
placement and required inductance for a given frequency and total
length.

## Usage

``` r
coil_inductance_profile(
  frequency,
  total_length,
  fractions = seq(0.3, 0.7, by = 0.05),
  wire_diameter = 0.065,
  metric = FALSE
)
```

## Arguments

- frequency:

  Operating frequency in megahertz (MHz).

- total_length:

  Total physical length of the dipole (feet or metres depending on
  `metric`).

- fractions:

  Numeric vector of fractions between 0 and 1 giving the coil positions
  relative to the half-length (0 = at the feedpoint, 1 = at the end of
  the leg).

- wire_diameter:

  Diameter of the dipole conductor; see
  [`loading_coil_inductance()`](https://yojimbodurant.github.io/loadedDipole/reference/loading_coil_inductance.md).

- metric:

  Logical; if `TRUE`, lengths are interpreted as metres and converted
  internally.

## Value

A data frame with columns `fraction_of_half`, `coil_position` (in the
same units as `total_length`) and `inductance` (uH).

## Examples

``` r
# Inductance profile for a 60 ft dipole at 3.57 MHz
coil_inductance_profile(frequency = 3.57, total_length = 60,
                        fractions = seq(0.3, 0.7, by = 0.05))
#>   fraction_of_half coil_position inductance
#> 1             0.30           9.0   35.01492
#> 2             0.35          10.5   37.48939
#> 3             0.40          12.0   40.39365
#> 4             0.45          13.5   43.84239
#> 5             0.50          15.0   47.99443
#> 6             0.55          16.5   53.07632
#> 7             0.60          18.0   59.42280
#> 8             0.65          19.5   67.55064
#> 9             0.70          21.0   78.30285
```
