# Recommend a loading-coil placement for a given dipole and inductance

This helper solves for the distance from the feedpoint to each loading
coil (per leg) that yields a specified coil inductance at the desired
operating frequency and total dipole length. The calculation inverts the
[`loading_coil_inductance()`](https://yojimbodurant.github.io/loadedDipole/reference/loading_coil_inductance.md)
relation using numerical root finding on the coil position. Results are
based on the Jerry Hall short-dipole model and therefore assume two
identical coils placed symmetrically on each leg of a centre-fed dipole.

## Usage

``` r
recommend_coil_position(
  frequency,
  total_length,
  inductance,
  wire_diameter = 0.065,
  metric = FALSE,
  target_fraction = NULL
)
```

## Arguments

- frequency:

  Operating frequency in megahertz (MHz).

- total_length:

  Total physical length of the dipole. Feet when `metric = FALSE`,
  metres otherwise.

- inductance:

  Target loading-coil inductance (per coil) in microhenries (uH).

- wire_diameter:

  Diameter of the dipole conductor. Defaults to 0.065 inches
  (approximately AWG 14) when `metric = FALSE`; supply metres when
  `metric = TRUE`.

- metric:

  Logical; if `TRUE`, length quantities are interpreted as metres and
  converted internally to imperial units. If `FALSE`, lengths are
  assumed to be in feet and diameters in inches.

- target_fraction:

  Optional numeric value between 0 and 1 giving a preferred fraction of
  the half-length for the coil position (0 = at the feedpoint, 1 = at
  the end of the leg). When supplied the search for the solution is
  biased around this fraction; when `NULL` a broad search over 10–90% of
  the half-length is used.

## Value

A list with elements `frequency`, `total_length`, `inductance`,
`coil_position` (distance from feedpoint to each coil in the same units
as `total_length`), `fraction_of_half` (position expressed as a fraction
of the half-length), and the `metric` flag.

## Examples

``` r
# For a 60 ft dipole at 3.57 MHz with 66 uH coils, find the recommended
# coil placement.
recommend_coil_position(frequency = 3.57, total_length = 60,
                        inductance = 66)
#> Loaded dipole recommendation:
#>   Frequency        : 3.5700 MHz
#>   Total length     : 60.0000 ft
#>   Coil inductance  : 66.0000 uH (per coil)
#>   Coil position    : 19.2429 ft from feedpoint
#>   Fraction of half : 0.641
```
