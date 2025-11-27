# Render a markdown build report for a loaded dipole design

This function creates a self-contained Markdown report for a design
created by
[`design_loaded_dipole_full()`](https://yojimbodurant.github.io/loadedDipole/reference/design_loaded_dipole_full.md).
The report includes:

- A formatted build table

- An embedded (Base64) schematic figure (optional)

- An embedded (Base64) QR code linking to the package repository

- A small embedded CSS block so the HTML produced via pandoc looks
  reasonable without external files.

## Usage

``` r
render_loaded_dipole_md(design, file, include_plot = TRUE)
```

## Arguments

- design:

  Result from
  [`design_loaded_dipole_full()`](https://yojimbodurant.github.io/loadedDipole/reference/design_loaded_dipole_full.md).

- file:

  Output `.md` filename.

- include_plot:

  Logical; if `TRUE`, include an embedded schematic.

## Value

Invisibly returns the markdown file path.
