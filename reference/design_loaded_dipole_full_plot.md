# Internal plotting helper for loaded dipole schematics

Not exported. Used only by design_loaded_dipole_full() and
render_loaded_dipole_md().

Draws a simple (not-to-scale) schematic of a loaded dipole based on the
`build_table` produced by
[`design_loaded_dipole_full()`](https://yojimbodurant.github.io/loadedDipole/reference/design_loaded_dipole_full.md).

## Usage

``` r
design_loaded_dipole_full_plot(design)

design_loaded_dipole_full_plot(design)
```

## Arguments

- design:

  A design object returned by
  [`design_loaded_dipole_full()`](https://yojimbodurant.github.io/loadedDipole/reference/design_loaded_dipole_full.md).

## Value

Called for its side effect of drawing a base R plot.
