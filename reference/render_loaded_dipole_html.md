# Render a full HTML build report for a loaded dipole design

This convenience wrapper calls
[`render_loaded_dipole_md()`](https://yojimbodurant.github.io/loadedDipole/reference/render_loaded_dipole_md.md)
to build a temporary Markdown file and then uses
[`rmarkdown::pandoc_convert()`](https://pkgs.rstudio.com/rmarkdown/reference/pandoc_convert.html)
to produce a standalone HTML document. All styling and images are
embedded so the HTML file is fully portable.

## Usage

``` r
render_loaded_dipole_html(
  design,
  file = "loaded_dipole_report.html",
  include_plot = TRUE
)
```

## Arguments

- design:

  Result from
  [`design_loaded_dipole_full()`](https://yojimbodurant.github.io/loadedDipole/reference/design_loaded_dipole_full.md).

- file:

  Output HTML file path. Relative paths are resolved against
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- include_plot:

  Logical; if `TRUE`, include the schematic figure.

## Value

Invisibly returns the HTML file path.
