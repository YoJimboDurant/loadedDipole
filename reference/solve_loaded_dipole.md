# Solve for an unknown loaded dipole parameter

Given any two of the operating frequency, the required loading coil
inductance and the total physical length of the dipole, this function
solves for the remaining parameter and determines the optimum coil
placement. When \`frequency\` and \`total_length\` are supplied it
minimises the coil inductance with respect to the coil position. When
\`frequency\` and \`inductance\` are supplied it searches for a total
length that yields the specified inductance at the optimum coil
position. When \`total_length\` and \`inductance\` are supplied it
searches for a frequency that satisfies the inductance at the optimum
coil position. All calculations use the Jerry Hall formula for the coil
inductance【584486973026570†L36-L68】.

## Usage

``` r
solve_loaded_dipole(
  frequency = NA,
  inductance = NA,
  total_length = NA,
  wire_diameter = 0.065,
  metric = FALSE,
  search_length_range = NULL,
  search_frequency_range = NULL
)
```

## Arguments

- frequency:

  Operating frequency in megahertz (MHz). Use \`NA\` for the unknown
  parameter.

- inductance:

  Coil inductance per loading coil in microhenries (uH). Use \`NA\` for
  the unknown parameter.

- total_length:

  Total physical length of the dipole in feet (or metres when \`metric =
  TRUE\`). Use \`NA\` for the unknown parameter.

- wire_diameter:

  Diameter of the dipole conductor. The default of 0.065 inches
  corresponds to AWG 14. For metric units specify metres.

- metric:

  Logical; if \`TRUE\` all length inputs/outputs are in metres (and wire
  diameter in metres). If \`FALSE\` (default) feet and inches are used.

- search_length_range:

  Optional numeric vector of length two giving the lower and upper
  limits for total length when searching for a solution. When \`NULL\`
  and the total length is unknown, the function uses a range between 0.2
  × (full‑size dipole length) and the full‑size dipole length
  (\`468/frequency\`). The search is in the same units as
  \`total_length\`.

- search_frequency_range:

  Optional numeric vector of length two giving the lower and upper
  limits for frequency when searching for a solution (in MHz). When
  \`NULL\` and frequency is unknown, the range is c(0.5, 30) MHz.

## Value

A list containing the solved parameters: \`frequency\`, \`inductance\`,
\`total_length\`, \`coil_position\` (distance from feedpoint to coil),
and \`metric\` flag. When searching fails an informative error is
thrown.

## Examples

``` r
# Solve for the inductance and coil placement of a 60 ft dipole at 3.57 MHz
solve_loaded_dipole(frequency = 3.57, total_length = 60, inductance = NA)
#> Loaded dipole solution:
#>   Frequency      : 3.5700 MHz
#>   Total length   : 60.0000 ft
#>   Coil inductance: 27.9951 uH (per coil)
#>   Coil position  : 3.0001 ft from feedpoint

# Given a 60 ft dipole and 38.6 uH loading coils, find the required frequency
solve_loaded_dipole(frequency = NA, total_length = 60, inductance = 38.6)
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Warning: NaNs produced
#> Warning: NA/NaN replaced by maximum positive value
#> Loaded dipole solution:
#>   Frequency      : 3.1324 MHz
#>   Total length   : 60.0000 ft
#>   Coil inductance: 38.6000 uH (per coil)
#>   Coil position  : 3.0001 ft from feedpoint

# Given a 38.6 uH coil and 3.57 MHz frequency, compute the total length
solve_loaded_dipole(frequency = 3.57, inductance = 38.6, total_length = NA)
#> Loaded dipole solution:
#>   Frequency      : 3.5700 MHz
#>   Total length   : 46.6402 ft
#>   Coil inductance: 38.6000 uH (per coil)
#>   Coil position  : 2.3321 ft from feedpoint
```
