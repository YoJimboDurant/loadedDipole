# Sweep coil placement and inductance for a shortened dipole

This function computes the loading coil inductance required to resonate
a shortened dipole at a given frequency over a sequence of coil
positions. It can optionally estimate antenna efficiency for each coil
placement using the "simple", "segmented", or "both" efficiency models.
All internal calculations use imperial units (feet and inches), but the
`metric` argument allows users to specify input lengths in metres
instead.

## Usage

``` r
optimize_inductance_for_position(
  frequency,
  total_length,
  positions,
  wire_diameter = 0.065,
  efficiency = FALSE,
  efficiency_model = c("simple", "segmented", "both"),
  metric = FALSE
)
```

## Arguments

- frequency:

  Operating frequency in megahertz (MHz).

- total_length:

  Total end-to-end dipole length. Feet when `metric = FALSE`, metres
  otherwise.

- positions:

  Numeric vector of coil positions to evaluate. These are distances from
  the feedpoint to each loading coil.

- wire_diameter:

  Conductor diameter. Inches when `metric = FALSE`, metres otherwise.
  Default is 0.065 inch (approx. AWG 14).

- efficiency:

  Logical; if `TRUE`, efficiency is estimated using
  [`estimate_efficiency()`](https://yojimbodurant.github.io/loadedDipole/reference/estimate_efficiency.md)
  for each coil position.

- efficiency_model:

  One of `"simple"`, `"segmented"`, or `"both"`. Determines which
  efficiency model to use when `efficiency = TRUE`.

- metric:

  Logical; if `TRUE`, length inputs are interpreted as metres and
  internally converted to imperial units for the Hall inductance model.

## Value

A data frame with one row per coil position. Columns include:

- `coil_position`: coil placement in the same units as the user supplied
  (feet or metres).

- `inductance`: required inductance in microhenry (uH) calculated using
  the Hall short-dipole formula.

- `eff_simple`: estimated efficiency using the simple model (only
  returned when `efficiency = TRUE` and `efficiency_model` includes
  "simple").

- `eff_segmented`: estimated efficiency using the segmented model (only
  returned when `efficiency = TRUE` and `efficiency_model` includes
  "segmented").

Invalid coil positions or numerical failures return `NA` for inductance
and/or efficiency fields.

## Details

The function is hardened for safety: any invalid coil position,
numerical failure in the inductance calculation, or failure inside the
efficiency estimator will cause `NA` values to be inserted for that row
rather than stopping execution. This makes the function suitable for
sweeps and for use inside vignettes or CRAN examples.

## Examples

``` r
# Sweep coil positions from 10 to 20 ft for a 60 ft dipole at 3.57 MHz
optimize_inductance_for_position(
  frequency     = 3.57,
  total_length  = 60,
  positions     = seq(10, 20, by = 2),
  efficiency    = TRUE,
  efficiency_model = "both"
)
#>   coil_position inductance eff_simple eff_segmented
#> 1            10   36.62151  0.9521232     0.9645267
#> 2            12   40.39365  0.9508534     0.9619872
#> 3            14   45.13876  0.9495903     0.9594610
#> 4            16   51.26121  0.9483338     0.9569480
#> 5            18   59.42280  0.9470838     0.9544481
#> 6            20   70.78758  0.9458402     0.9519612
```
