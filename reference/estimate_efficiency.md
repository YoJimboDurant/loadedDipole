# Estimate efficiency of a loaded shortened dipole

This hardened version handles all failure conditions gracefully, always
returning numeric NA values when estimates cannot be computed.

## Usage

``` r
estimate_efficiency(
  frequency,
  total_length,
  coil_inductance,
  coil_position,
  wire_diameter = 0.065,
  model = c("simple", "segmented", "both"),
  metric = FALSE
)
```

## Arguments

- frequency:

  MHz

- total_length:

  ft (or m if metric)

- coil_inductance:

  uH (per coil)

- coil_position:

  ft from feedpoint (or m)

- wire_diameter:

  inches (or m)

- model:

  "simple", "segmented", or "both"

- metric:

  logical

## Value

A list with efficiency estimates; invalid inputs return NA safely.
