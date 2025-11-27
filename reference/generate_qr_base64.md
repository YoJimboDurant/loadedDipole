# Generate a Base64-encoded QR Code PNG

Internal helper used by the report renderer to embed a QR link to the
project repository.

## Usage

``` r
generate_qr_base64(text, size = 400)
```

## Arguments

- text:

  Text or URL to encode into the QR code.

- size:

  Pixel size of the QR code image (not critical).

## Value

A Base64 string suitable for use inside an HTML
`<img src="data:image/png;base64,...">` tag.
