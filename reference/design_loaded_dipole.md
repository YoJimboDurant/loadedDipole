# High-level design function for a shortened loaded dipole

This function automates the entire design workflow for a loaded dipole
when only the total length and frequency are known, and when the builder
is willing to use any inductance up to `L_max`. It:

## Usage

``` r
design_loaded_dipole(
  frequency,
  total_length,
  L_max,
  wire_diameter = 0.065,
  positions = seq(8, 22, by = 0.5),
  metric = FALSE
)
```

## Arguments

- frequency:

  Operating frequency in MHz.

- total_length:

  The total dipole length (ft unless metric = TRUE).

- L_max:

  Maximum allowed inductance per coil in microhenries.

- wire_diameter:

  Conductor diameter (inches or metres).

- positions:

  Optional vector of coil positions to test (default: 8-22 ft).

- metric:

  Logical; TRUE = metres, FALSE = feet.

## Value

A list containing:

- designs:

  A data frame of all feasible coil positions with inductance and
  efficiency

- best:

  The chosen optimal design

## Details

1\. Sweeps coil positions 2. Computes the required inductance for
resonance 3. Filters out those requiring inductance greater than `L_max`
4. Computes efficiency (segmented model) 5. Picks the \*optimal\*
design: - within 0.1 dB of maximum efficiency, and - using the smallest
inductance

## Examples

``` r
# Design for a 60 ft dipole at 3.57 MHz with at most 66 uH coils
design_loaded_dipole(frequency = 3.57, total_length = 60, L_max = 66)
#> $designs
#>    coil_position inductance efficiency   loss_dB
#> 1            8.0   33.56123  0.9670797 0.1453773
#> 2            8.5   34.27028  0.9664402 0.1482501
#> 3            9.0   35.01492  0.9658015 0.1511211
#> 4            9.5   35.79772  0.9651637 0.1539901
#> 5           10.0   36.62151  0.9645267 0.1568573
#> 6           10.5   37.48939  0.9638906 0.1597226
#> 7           11.0   38.40478  0.9632553 0.1625859
#> 8           11.5   39.37147  0.9626208 0.1654475
#> 9           12.0   40.39365  0.9619872 0.1683071
#> 10          12.5   41.47597  0.9613544 0.1711648
#> 11          13.0   42.62362  0.9607224 0.1740207
#> 12          13.5   43.84239  0.9600913 0.1768747
#> 13          14.0   45.13876  0.9594610 0.1797268
#> 14          14.5   46.52003  0.9588315 0.1825771
#> 15          15.0   47.99443  0.9582028 0.1854255
#> 16          15.5   49.57129  0.9575750 0.1882720
#> 17          16.0   51.26121  0.9569480 0.1911167
#> 18          16.5   53.07632  0.9563218 0.1939595
#> 19          17.0   55.03050  0.9556964 0.1968005
#> 20          17.5   57.13979  0.9550718 0.1996396
#> 21          18.0   59.42280  0.9544481 0.2024769
#> 22          18.5   61.90120  0.9538251 0.2053123
#> 23          19.0   64.60045  0.9532030 0.2081460
#> 
#> $best
#>   coil_position inductance efficiency   loss_dB
#> 1             8   33.56123  0.9670797 0.1453773
#> 
```
