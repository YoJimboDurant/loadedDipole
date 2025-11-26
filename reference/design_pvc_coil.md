# Design a single-layer air-core solenoid coil on PVC

This function computes the winding geometry, required number of turns,
winding length, and wire length for a target inductance using Wheeler's
formula. It also supports specifying wire diameter and optional turn
spacing, making it suitable for practical HF loading coil construction.

## Usage

``` r
design_pvc_coil(
  inductance,
  pvc_size = "1-1/2",
  wire_diameter = 0.064,
  turn_spacing = NULL,
  schedule = "40"
)
```

## Arguments

- inductance:

  Target inductance in microhenry (uH).

- pvc_size:

  Nominal Schedule 40 PVC size ("1/2", "3/4", "1", "1-1/4", "1-1/2",
  "2").

- wire_diameter:

  Wire diameter in inches (e.g., 0.064 for AWG 14).

- turn_spacing:

  Optional spacing between turns in inches. If NULL, assumes tight
  winding (spacing = wire diameter).

- schedule:

  Pipe schedule (only "40" supported).

## Value

A list containing:

- turns:

  Estimated number of turns

- winding_length_in:

  Total winding length in inches

- diameter_in:

  Coil diameter in inches

- wire_length_ft:

  Estimated wire length in feet

- summary_table:

  Data frame of N ± 2 turns with inductance values

## Details

Wheeler (single-layer) formula: L (uH) = (d^2 \* N^2) / (18 d + 40 l)

## Examples

``` r
design_pvc_coil(
  inductance = 33.5,
  pvc_size = "1-1/2",
  wire_diameter = 0.064,
  turn_spacing = 0.070
)
#> $turns
#> [1] 35.04055
#> 
#> $winding_length_in
#> [1] 2.452839
#> 
#> $diameter_in
#> [1] 1.9
#> 
#> $wire_length_ft
#> [1] 17.42983
#> 
#> $summary_table
#>   turns inductance_uH winding_length_in
#> 1    33      31.05284              2.31
#> 2    34      32.25008              2.38
#> 3    35      33.45121              2.45
#> 4    36      34.65600              2.52
#> 5    37      35.86422              2.59
#> 
```
