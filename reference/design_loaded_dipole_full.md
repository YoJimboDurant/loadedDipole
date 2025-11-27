# Full practical design workflow for an optimized loaded dipole

This function builds on \`design_loaded_dipole()\` by: 1. Finding the
optimal coil position using efficiency + inductance tradeoff 2.
Designing the physical loading coil on PVC 3. Estimating efficiency 4.
Producing a builder-friendly summary table 5. Drawing a simple schematic

## Usage

``` r
design_loaded_dipole_full(
  frequency,
  total_length,
  L_max,
  pvc_size = "1-1/2",
  wire_diameter = 0.065,
  turn_spacing = NULL,
  positions = seq(8, 22, by = 0.5),
  metric = FALSE,
  make_plot = TRUE
)
```

## Arguments

- frequency:

  Operating frequency in MHz.

- total_length:

  Total physical dipole length (ft or m).

- L_max:

  Maximum acceptable inductance (uH) per coil.

- pvc_size:

  PVC form (e.g., "1-1/2").

- wire_diameter:

  Wire diameter (inches or m).

- turn_spacing:

  Turn spacing in inches (NULL = tight winding).

- positions:

  Coil positions to evaluate (default: 8–22 ft or metric-converted).

- metric:

  TRUE = metres, FALSE = feet.

- make_plot:

  Draw schematic?

## Value

A list containing:

- optimizer:

  Output of design_loaded_dipole()

- best:

  Chosen design row

- coil:

  PVC coil geometry from design_pvc_coil()

- efficiency:

  Efficiency estimates from estimate_efficiency()

- build_table:

  Printable build-sheet table
