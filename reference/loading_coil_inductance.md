# Calculate loading coil inductance for a shortened dipole

This function implements the Jerry Hall (QST Sep 1974) formula for the
inductance of a loading coil used to shorten a half‑wave dipole antenna.
It returns the inductance of each coil in micro‑henries given the
operating frequency, the total physical length of the dipole, the
distance from the feedpoint to each coil, and the conductor diameter.
The formula relies on logarithmic terms and is defined for distances
measured in imperial units (feet and inches). When \`metric = TRUE\` the
inputs are converted from metres (for lengths) and metres (for wire
diameter) to feet and inches internally. For further details on the
equation and variable definitions see Hall's article and subsequent
summaries【584486973026570†L36-L68】.

## Usage

``` r
loading_coil_inductance(
  frequency,
  total_length,
  coil_position,
  wire_diameter = 0.065,
  metric = FALSE
)
```

## Arguments

- frequency:

  Operating frequency in megahertz (MHz).

- total_length:

  Total physical length of the dipole. If \`metric\` is \`TRUE\` this
  value is interpreted as metres; otherwise it is taken as feet. The
  length refers to the complete end‑to‑end length of the dipole.

- coil_position:

  Distance from the feedpoint to each loading coil. If \`metric\` is
  \`TRUE\` this is in metres; otherwise in feet. Two coils are assumed
  symmetrically placed on each side of the feedpoint.

- wire_diameter:

  Diameter of the dipole conductor. If \`metric\` is \`TRUE\` this is in
  metres; otherwise it is in inches. If unspecified the default is 0.065
  inches (approx. AWG 14 wire).

- metric:

  Logical; if \`TRUE\`, inputs are metric (metres for lengths and metres
  for wire diameter). If \`FALSE\`, inputs are imperial (feet for
  lengths and inches for wire diameter).

## Value

A numeric value giving the required loading coil inductance (per coil)
in microhenries (uH). When the function is called with a vector of coil
positions it will return a vector of inductances.

## Examples

``` r
# Compute inductance for a 20 m total length dipole with coils 5 m from the centre
loading_coil_inductance(10.1, total_length = 20, coil_position = 5,
                         wire_diameter = 0.0015, metric = TRUE)
#> [1] -10.20186

# Same calculation using imperial units (A=65.6 ft, B=16.4 ft, D=0.059 in)
loading_coil_inductance(10.1, total_length = 65.6, coil_position = 16.4,
                         wire_diameter = 0.059, metric = FALSE)
#> [1] -10.19189
```
