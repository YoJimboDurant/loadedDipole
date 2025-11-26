# Plot the relationship between coil placement and the resonant quantity

This helper creates a ggplot object showing how either the coil
inductance varies with coil placement (when frequency and total_length
are known) or how the resonant frequency varies with coil placement
(when inductance and total_length are known). The function uses
numerical root finding to compute the dependent variable and may be
slower for fine resolution. Only one of \`frequency\` or \`inductance\`
should be supplied. The total length must be specified. The wire
diameter can be adjusted from its default.

## Usage

``` r
plot_inductor_placement(
  frequency = NULL,
  inductance = NULL,
  total_length,
  wire_diameter = 0.065,
  metric = FALSE,
  num_points = 100
)
```

## Arguments

- frequency:

  Operating frequency in MHz (optional). When provided the plot shows
  coil inductance as a function of coil placement. When \`NULL\` and
  \`inductance\` is supplied the plot shows the frequency needed to
  resonate the antenna for different coil positions.

- inductance:

  Coil inductance in microhenries (optional). When provided and
  \`frequency\` is \`NULL\` the plot shows the required resonant
  frequency for varying coil positions.

- total_length:

  Total physical length of the dipole (feet or metres depending on
  \`metric\`). This argument is required.

- wire_diameter:

  Diameter of the dipole conductor. Default is 0.065 inches. For metric
  units supply metres.

- metric:

  Logical; if \`TRUE\` length inputs and outputs use metres. Otherwise
  use feet and inches.

- num_points:

  Number of points to sample along the coil placement interval. Default
  is 100. Higher values give smoother curves but take longer to compute,
  especially when solving for frequency.

## Value

A \`ggplot2\` object. Use \`print()\` to display or add layers.

## Examples

``` r
# Plot inductance versus coil placement for a 60 ft dipole at 3.57 MHz
plot_inductor_placement(frequency = 3.57, total_length = 60)


# Plot frequency versus coil placement for a 60 ft dipole with 38.6 uH coils
plot_inductor_placement(inductance = 38.6, total_length = 60)
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: Removed 77 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
