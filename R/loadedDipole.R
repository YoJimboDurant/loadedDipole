#' Calculate loading coil inductance for a shortened dipole
#'
#' This function implements the Jerry Hall (QST Sep 1974) formula for the
#' inductance of a loading coil used to shorten a half-wave dipole antenna.
#' It returns the inductance of each coil in micro-henries given the operating
#' frequency, the total physical length of the dipole, the distance from the
#' feedpoint to each coil, and the conductor diameter.  The formula relies on
#' logarithmic terms and is defined for distances measured in imperial units
#' (feet and inches).  When `metric = TRUE` the inputs are converted from
#' metres (for lengths) and metres (for wire diameter) to feet and inches
#' internally.  For further details on the equation and variable definitions
#' see Hall's article and subsequent summaries.
#'
#' @param frequency Operating frequency in megahertz (MHz).
#' @param total_length Total physical length of the dipole.  If `metric` is
#'   `TRUE` this value is interpreted as metres; otherwise it is taken as
#'   feet.  The length refers to the complete end-to-end length of the dipole.
#' @param coil_position Distance from the feedpoint to each loading coil.  If
#'   `metric` is `TRUE` this is in metres; otherwise in feet.  Two coils are
#'   assumed symmetrically placed on each side of the feedpoint.
#' @param wire_diameter Diameter of the dipole conductor.  If `metric` is
#'   `TRUE` this is in metres; otherwise it is in inches.  If unspecified the
#'   default is 0.065 inches (approx. AWG 14 wire).
#' @param metric Logical; if `TRUE`, inputs are metric (metres for lengths and
#'   metres for wire diameter).  If `FALSE`, inputs are imperial (feet for
#'   lengths and inches for wire diameter).
#' @return A numeric value giving the required loading coil inductance (per
#'   coil) in microhenries (uH).  When the function is called with a vector of
#'   coil positions it will return a vector of inductances.
#' @examples
#' # Compute inductance for a 20 m total length dipole with coils 5 m from the centre
#' loading_coil_inductance(10.1, total_length = 20, coil_position = 5,
#'                          wire_diameter = 0.0015, metric = TRUE)
#'
#' # Same calculation using imperial units (A=65.6 ft, B=16.4 ft, D=0.059 in)
#' loading_coil_inductance(10.1, total_length = 65.6, coil_position = 16.4,
#'                          wire_diameter = 0.059, metric = FALSE)
#' @export                          
loading_coil_inductance <- function(frequency, total_length, coil_position,
                                    wire_diameter = 0.065, metric = FALSE) {
  if (length(frequency) != 1) {
    stop("frequency must be a single numeric value")
  }
  # Convert metric inputs to imperial units
  if (metric) {
    inch <- 0.0254
    foot <- 12 * inch
    total_length <- total_length / foot
    coil_position <- coil_position / foot
    wire_diameter <- wire_diameter / inch
  }
  f <- frequency
  A <- total_length
  B <- coil_position
  D <- wire_diameter
  # Ensure B is within (0, A/2)
  if (any(B <= 0) || any(B >= A/2)) {
    stop("coil_position must be greater than 0 and less than total_length/2")
  }
  # Vectorise over B if necessary
  calc_H <- function(Bi) {
    T00 <- (234 / f) - Bi
    T01 <- log(24 * T00 / D) - 1
    T02 <- (1 - f * Bi / 234)^2 - 1
    T03 <- (A / 2) - Bi
    T04 <- ((f * A / 2 - f * Bi) / 234)^2 - 1
    T05 <- log(24 * T03 / D) - 1
    T06 <- 1e6 / (68 * pi^2 * f^2)
    H  <- T06 * ((T01 * T02) / T00 - (T04 * T05) / T03)
    return(H)
  }
  vapply(coil_position, calc_H, numeric(1))
}

# internal helper: find optimum coil position for minimal coil inductance
.find_optimal_B <- function(frequency, total_length, wire_diameter = 0.065,
                            metric = FALSE) {
  # Convert to imperial units if needed
  if (metric) {
    inch <- 0.0254
    foot <- 12 * inch
    total_length <- total_length / foot
    wire_diameter <- wire_diameter / inch
  }
  f <- frequency
  A <- total_length
  D <- wire_diameter
  # Define objective function for optimize
  obj <- function(B) {
    # prevent invalid B values
    if (B <= 0 || B >= A/2) return(Inf)
    loading_coil_inductance(f, total_length = A, coil_position = B, wire_diameter = D, metric = FALSE)
  }
  # Search B in a sensible range: from a small fraction of A to just below half the length
  # Constrain the search for B to a realistic region between 10 % and 45 %
  # of the half-length.  Coils placed extremely close to the feedpoint minimise
  # inductance but are impractical.
  half_length <- A / 2
  lower <- half_length * 0.10
  upper <- half_length * 0.45
  if (lower >= upper) {
    lower <- max(1e-3, 0.001 * A)
    upper <- half_length * 0.9
  }
  res <- optimize(obj, lower = lower, upper = upper)
  list(B_opt = res$minimum, H_opt = res$objective)
}

#' Solve for an unknown loaded dipole parameter
#'
#' Given any two of the operating frequency, the required loading coil inductance
#' and the total physical length of the dipole, this function solves for the
#' remaining parameter and determines the optimum coil placement.  When
#' `frequency` and `total_length` are supplied it minimises the coil inductance
#' with respect to the coil position.  When `frequency` and `inductance` are
#' supplied it searches for a total length that yields the specified inductance
#' at the optimum coil position.  When `total_length` and `inductance` are
#' supplied it searches for a frequency that satisfies the inductance at the
#' optimum coil position.  All calculations use the Jerry Hall formula for the
#' coil inductance.
#'
#' @param frequency Operating frequency in megahertz (MHz).  Use `NA` for the
#'   unknown parameter.
#' @param inductance Coil inductance per loading coil in microhenries (uH).
#'   Use `NA` for the unknown parameter.
#' @param total_length Total physical length of the dipole in feet (or metres
#'   when `metric = TRUE`).  Use `NA` for the unknown parameter.
#' @param wire_diameter Diameter of the dipole conductor.  The default of
#'   0.065 inches corresponds to AWG 14.  For metric units specify metres.
#' @param metric Logical; if `TRUE` all length inputs/outputs are in metres
#'   (and wire diameter in metres).  If `FALSE` (default) feet and inches are
#'   used.
#' @param search_length_range Optional numeric vector of length two giving the
#'   lower and upper limits for total length when searching for a solution.
#'   When `NULL` and the total length is unknown, the function uses a range
#'   between 0.2 X (full-size dipole length) and the full-size dipole length
#'   (`468/frequency`).  The search is in the same units as `total_length`.
#' @param search_frequency_range Optional numeric vector of length two giving the
#'   lower and upper limits for frequency when searching for a solution (in MHz).
#'   When `NULL` and frequency is unknown, the range is c(0.5, 30) MHz.
#' @return A list containing the solved parameters: `frequency`, `inductance`,
#'   `total_length`, `coil_position` (distance from feedpoint to coil), and
#'   `metric` flag.  When searching fails an informative error is thrown.
#' @examples
#' # Solve for the inductance and coil placement of a 60 ft dipole at 3.57 MHz
#' solve_loaded_dipole(frequency = 3.57, total_length = 60, inductance = NA)
#'
#' # Given a 60 ft dipole and 38.6 uH loading coils, find the required frequency
#' solve_loaded_dipole(frequency = NA, total_length = 60, inductance = 38.6)
#'
#' # Given a 38.6 uH coil and 3.57 MHz frequency, compute the total length
#' solve_loaded_dipole(frequency = 3.57, inductance = 38.6, total_length = NA)
#' @export
solve_loaded_dipole <- function(frequency = NA, inductance = NA, total_length = NA,
                                wire_diameter = 0.065, metric = FALSE,
                                search_length_range = NULL,
                                search_frequency_range = NULL) {
  # Count how many variables are provided
  provided <- !is.na(c(frequency, inductance, total_length))
  if (sum(provided) != 2) {
    stop("Exactly two of frequency, inductance, and total_length must be supplied")
  }
  # Helper to convert between metric and imperial
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) {
      return(value / foot)
    } else {
      return(value / inch)
    }
  }
  to_metric <- function(value, is_length = TRUE) {
    if (metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) {
      return(value * foot)
    } else {
      return(value * inch)
    }
  }
  # Convert known quantities to imperial
  f_in <- if (!is.na(frequency)) frequency else NA
  L_in <- if (!is.na(inductance)) inductance else NA
  A_in <- if (!is.na(total_length)) total_length else NA
  # All length parameters to imperial for calculations
  if (!is.na(A_in)) A_imperial <- to_imperial(A_in, TRUE)
  D_imperial <- to_imperial(wire_diameter, FALSE)
  # Case 1: frequency and total_length provided -> solve inductance & coil position
  if (!is.na(f_in) && !is.na(A_in) && is.na(L_in)) {
    # find optimum coil position and inductance
    opt <- .find_optimal_B(frequency = f_in, total_length = A_in,
                           wire_diameter = wire_diameter, metric = metric)
    B_imperial <- opt$B_opt
    H <- opt$H_opt
    # Convert to metric if needed
    result <- list(
      frequency = f_in,
      inductance = H,
      total_length = A_in,
      coil_position = if (metric) to_metric(B_imperial, TRUE) else B_imperial,
      metric = metric
    )
    class(result) <- c("loadedDipoleSolution", class(result))
    return(result)
  }
  # Case 2: frequency and inductance provided -> solve total_length & coil position
  if (!is.na(f_in) && !is.na(L_in) && is.na(A_in)) {
    # Determine full-size length for search
    full_size <- 468 / f_in
    if (is.null(search_length_range)) {
      # search between 20% of full-size and full-size
      lower <- 0.2 * full_size
      upper <- full_size
    } else {
      lower <- search_length_range[1]
      upper <- search_length_range[2]
    }
    # convert to imperial
    lower_imp <- to_imperial(lower, TRUE)
    upper_imp <- to_imperial(upper, TRUE)
    # Define function of length: difference between H_opt(length) and target inductance
    f_root <- function(A_imp) {
      # For a given length in imperial, find optimum coil position & inductance
      opt <- .find_optimal_B(frequency = f_in, total_length = A_imp,
                             wire_diameter = D_imperial, metric = FALSE)
      return(opt$H_opt - L_in)
    }
    # Ensure the sign changes over the interval; adjust if necessary
    f_lower <- f_root(lower_imp)
    f_upper <- f_root(upper_imp)
    if (is.nan(f_lower) || is.nan(f_upper)) {
      stop("Invalid search range for total_length; please adjust search_length_range")
    }
    # If both ends are same sign, try expanding
    if (f_lower * f_upper > 0) {
      stop("Could not bracket a solution for total_length; try adjusting search_length_range")
    }
    sol <- uniroot(function(x) f_root(x), lower = lower_imp, upper = upper_imp)
    A_imp_solution <- sol$root
    # compute optimum B and H for found A
    opt <- .find_optimal_B(frequency = f_in, total_length = A_imp_solution,
                           wire_diameter = D_imperial, metric = FALSE)
    B_imp <- opt$B_opt
    # convert to metric outputs
    A_out <- if (metric) to_metric(A_imp_solution, TRUE) else A_imp_solution
    B_out <- if (metric) to_metric(B_imp, TRUE) else B_imp
    result <- list(
      frequency = f_in,
      inductance = L_in,
      total_length = A_out,
      coil_position = B_out,
      metric = metric
    )
    class(result) <- c("loadedDipoleSolution", class(result))
    return(result)
  }
  # Case 3: total_length and inductance provided -> solve frequency & coil position
  if (!is.na(A_in) && !is.na(L_in) && is.na(f_in)) {
    # Convert total_length to imperial for calculations
    A_imp <- to_imperial(A_in, TRUE)
    # Determine search range for frequency
    if (is.null(search_frequency_range)) {
      lower_f <- 0.5
      upper_f <- 30
    } else {
      lower_f <- search_frequency_range[1]
      upper_f <- search_frequency_range[2]
    }
    # Define root function: difference between H_opt(frequency) and L_in
    f_root_freq <- function(f_test) {
      opt <- .find_optimal_B(frequency = f_test, total_length = A_imp,
                             wire_diameter = D_imperial, metric = FALSE)
      return(opt$H_opt - L_in)
    }
    f_lower <- f_root_freq(lower_f)
    f_upper <- f_root_freq(upper_f)
    if (is.nan(f_lower) || is.nan(f_upper)) {
      stop("Invalid search range for frequency; please adjust search_frequency_range")
    }
    if (f_lower * f_upper > 0) {
      stop("Could not bracket a solution for frequency; try adjusting search_frequency_range")
    }
    sol <- uniroot(function(x) f_root_freq(x), lower = lower_f, upper = upper_f)
    f_solution <- sol$root
    # compute optimum B and H for found frequency
    opt <- .find_optimal_B(frequency = f_solution, total_length = A_imp,
                           wire_diameter = D_imperial, metric = FALSE)
    B_imp <- opt$B_opt
    # convert B to metric if needed
    B_out <- if (metric) to_metric(B_imp, TRUE) else B_imp
    result <- list(
      frequency = f_solution,
      inductance = L_in,
      total_length = A_in,
      coil_position = B_out,
      metric = metric
    )
    class(result) <- c("loadedDipoleSolution", class(result))
    return(result)
  }
  stop("Unexpected parameter combination")
}

#' Plot the relationship between coil placement and the resonant quantity
#'
#' This helper creates a ggplot object showing how either the coil inductance
#' varies with coil placement (when frequency and total_length are known) or
#' how the resonant frequency varies with coil placement (when inductance and
#' total_length are known).  The function uses numerical root finding to
#' compute the dependent variable and may be slower for fine resolution.  Only
#' one of `frequency` or `inductance` should be supplied.  The total length
#' must be specified.  The wire diameter can be adjusted from its default.
#'
#' @param frequency Operating frequency in MHz (optional).  When provided the
#'   plot shows coil inductance as a function of coil placement.  When
#'   `NULL` and `inductance` is supplied the plot shows the frequency needed
#'   to resonate the antenna for different coil positions.
#' @param inductance Coil inductance in microhenries (optional).  When
#'   provided and `frequency` is `NULL` the plot shows the required
#'   resonant frequency for varying coil positions.
#' @param total_length Total physical length of the dipole (feet or metres
#'   depending on `metric`).  This argument is required.
#' @param wire_diameter Diameter of the dipole conductor.  Default is
#'   0.065 inches.  For metric units supply metres.
#' @param metric Logical; if `TRUE` length inputs and outputs use metres.
#'   Otherwise use feet and inches.
#' @param num_points Number of points to sample along the coil placement
#'   interval.  Default is 100.  Higher values give smoother curves but take
#'   longer to compute, especially when solving for frequency.
#' @return A `ggplot2` object.  Use `print()` to display or add layers.
#' @examples
#' # Plot inductance versus coil placement for a 60 ft dipole at 3.57 MHz
#' plot_inductor_placement(frequency = 3.57, total_length = 60)
#'
#' # Plot frequency versus coil placement for a 60 ft dipole with 38.6 uH coils
#' plot_inductor_placement(inductance = 38.6, total_length = 60)
#' @export

plot_inductor_placement <- function(
  frequency = NULL,
  inductance = NULL,
  total_length,
  wire_diameter = 0.065,
  metric = FALSE,
  num_points = 100
) {

  # --- require ggplot2 if needed ---------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'plot_inductor_placement()' function requires ggplot2.\n",
      "Please install it with: install.packages('ggplot2')"
    )
  }

  # --- validate inputs --------------------------------------------------------
  if (is.null(total_length))
    stop("total_length must be supplied")

  if (!xor(is.null(frequency), is.null(inductance))) {
    stop("Supply either frequency or inductance, but not both")
  }

  # --- unit conversion helpers ------------------------------------------------
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }

  to_metric <- function(value, is_length = TRUE) {
    if (metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value * foot else value * inch
  }

  # --- convert to imperial (Hall model is imperial) --------------------------
  A_imp <- to_imperial(total_length, TRUE)
  D_imp <- to_imperial(wire_diameter, FALSE)
  half_len_imp <- A_imp / 2

  # --- attempt to compute recommended coil placement -------------------------
  rec <- try(
    recommend_coil_position(
      frequency      = frequency,
      total_length   = total_length,
      inductance     = inductance,
      wire_diameter  = wire_diameter,
      metric         = metric
    ),
    silent = TRUE
  )

  # --- choose coil-position range (Option 3) ---------------------------------
  if (!inherits(rec, "try-error") &&
      !is.null(rec$coil_position) &&
      is.finite(rec$coil_position)) {

    B0 <- to_imperial(rec$coil_position, TRUE)  # recommended position in ft

    # expand plus/minus 25% of half-length around B0
    B_min <- max(0.05 * half_len_imp, B0 - 0.25 * half_len_imp)
    B_max <- min(half_len_imp,         B0 + 0.25 * half_len_imp)

  } else {
    # fallback if solver fails
    B_min <- 0.10 * half_len_imp
    B_max <- 0.80 * half_len_imp
  }

  B_imp_seq <- seq(B_min, B_max, length.out = num_points)

  # ============================================================================  
  # CASE 1: known frequency -> compute inductance vs placement
  # ============================================================================  
  if (!is.null(frequency)) {

    L_vals <- loading_coil_inductance(
      frequency,
      total_length  = A_imp,
      coil_position = B_imp_seq,
      wire_diameter = D_imp,
      metric        = FALSE
    )

    df <- data.frame(
      coil_position = if (metric) to_metric(B_imp_seq, TRUE) else B_imp_seq,
      inductance    = L_vals
    )

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = coil_position, y = inductance)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = if (metric) "Coil position (m)" else "Coil position (ft)",
        y = "Inductance (uH)",
        title = "Coil inductance vs. coil placement",
        subtitle = paste0(
          "f = ", frequency, " MHz, Length = ",
          total_length, if (metric) " m" else " ft"
        )
      )

    if (!inherits(rec, "try-error") && is.finite(rec$coil_position)) {
      xpos <- rec$coil_position
      xpos_plot <- xpos

      p <- p +
        ggplot2::geom_vline(xintercept = xpos_plot, linetype = "dashed") +
        ggplot2::annotate(
          "text",
          x = xpos_plot,
          y = max(df$inductance, na.rm = TRUE),
          label = sprintf("Recommended: %.2f ft", xpos_plot),
          vjust = -0.5,
          size = 3
        )
    }

    return(p)
  }

  # ============================================================================  
  # CASE 2: known inductance -> compute resonant frequency vs placement
  # ============================================================================  

  safe_root <- function(B_imp) {

    root_fun <- function(f_test) {
      val <- try(
        loading_coil_inductance(
          f_test,
          total_length  = A_imp,
          coil_position = B_imp,
          wire_diameter = D_imp,
          metric        = FALSE
        ),
        silent = TRUE
      )

      if (inherits(val, "try-error") || is.nan(val))
        return(NA_real_)

      val - inductance
    }

    lower <- 0.5
    upper <- 30
    f_lower <- root_fun(lower)
    f_upper <- root_fun(upper)

    attempts <- 0
    while (!is.na(f_lower) && !is.na(f_upper) &&
           (f_lower * f_upper > 0) &&
           attempts < 5) {
      lower <- max(0.1, lower / 2)
      upper <- upper * 2
      f_lower <- root_fun(lower)
      f_upper <- root_fun(upper)
      attempts <- attempts + 1
    }

    if (is.na(f_lower) || is.na(f_upper) || (f_lower * f_upper > 0))
      return(NA_real_)

    out <- try(
      stats::uniroot(root_fun, lower = lower, upper = upper),
      silent = TRUE
    )

    if (inherits(out, "try-error"))
      return(NA_real_)

    out$root
  }

  freqs <- vapply(B_imp_seq, safe_root, numeric(1))

  df <- data.frame(
    coil_position = if (metric) to_metric(B_imp_seq, TRUE) else B_imp_seq,
    frequency     = freqs
  )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = coil_position, y = frequency)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x  = if (metric) "Coil position (m)" else "Coil position (ft)",
      y  = "Frequency (MHz)",
      title = "Resonant frequency vs. coil placement",
      subtitle = paste0(
        "L = ", inductance, " uH, Length = ",
        total_length, if (metric) " m" else " ft"
      )
    )

  if (!inherits(rec, "try-error") && is.finite(rec$coil_position)) {
    xpos <- rec$coil_position
    xpos_plot <- xpos

    p <- p +
      ggplot2::geom_vline(xintercept = xpos_plot, linetype = "dashed") +
      ggplot2::annotate(
        "text",
        x     = xpos_plot,
        y     = max(df$frequency, na.rm = TRUE),
        label = sprintf("Recommended: %.2f ft", xpos_plot),
        vjust = -0.5,
        size  = 3
      )
  }

  p
}
#' Print method for loaded dipole solutions
#'
#' @param x An object of class `loadedDipoleSolution` returned by
#'   `solve_loaded_dipole`.
#' @param ... Additional arguments (ignored).
#' @export
print.loadedDipoleSolution <- function(x, ...) {
  units_length <- if (x$metric) "m" else "ft"
  cat(sprintf("Loaded dipole solution:\n"))
  cat(sprintf("  Frequency      : %.4f MHz\n", x$frequency))
  cat(sprintf("  Total length   : %.4f %s\n", x$total_length, units_length))
  cat(sprintf("  Coil inductance: %.4f uH (per coil)\n", x$inductance))
  cat(sprintf("  Coil position  : %.4f %s from feedpoint\n", x$coil_position, units_length))
  invisible(x)
}
#' Recommend a loading-coil placement for a given dipole and inductance
#'
#' This helper solves for the distance from the feedpoint to each loading coil
#' (per leg) that yields a specified coil inductance at the desired operating
#' frequency and total dipole length. The calculation inverts the
#' \code{loading_coil_inductance()} relation using numerical root finding on the
#' coil position. Results are based on the Jerry Hall short-dipole model and
#' therefore assume two identical coils placed symmetrically on each leg of a
#' centre-fed dipole.
#'
#' @param frequency Operating frequency in megahertz (MHz).
#' @param total_length Total physical length of the dipole. Feet when
#'   \code{metric = FALSE}, metres otherwise.
#' @param inductance Target loading-coil inductance (per coil) in microhenries
#'   (uH).
#' @param wire_diameter Diameter of the dipole conductor. Defaults to
#'   0.065 inches (approximately AWG 14) when \code{metric = FALSE}; supply
#'   metres when \code{metric = TRUE}.
#' @param metric Logical; if \code{TRUE}, length quantities are interpreted as
#'   metres and converted internally to imperial units. If \code{FALSE},
#'   lengths are assumed to be in feet and diameters in inches.
#' @param target_fraction Optional numeric value between 0 and 1 giving a
#'   preferred fraction of the half-length for the coil position (0 = at the
#'   feedpoint, 1 = at the end of the leg). When supplied the search for the
#'   solution is biased around this fraction; when \code{NULL} a broad search
#'   over 10--90\% of the half-length is used.
#' @return A list with elements \code{frequency}, \code{total_length},
#'   \code{inductance}, \code{coil_position} (distance from feedpoint to each
#'   coil in the same units as \code{total_length}), \code{fraction_of_half}
#'   (position expressed as a fraction of the half-length), and the
#'   \code{metric} flag.
#' @examples
#' # For a 60 ft dipole at 3.57 MHz with 66 uH coils, find the recommended
#' # coil placement.
#' recommend_coil_position(frequency = 3.57, total_length = 60,
#'                         inductance = 66)
#' @export
recommend_coil_position <- function(frequency, total_length, inductance,
                                    wire_diameter = 0.065, metric = FALSE,
                                    target_fraction = NULL) {
  if (is.na(frequency) || is.na(total_length) || is.na(inductance)) {
    stop("frequency, total_length and inductance must all be supplied")
  }
  # unit conversion helpers
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }
  to_metric <- function(value, is_length = TRUE) {
    if (metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value * foot else value * inch
  }
  A_imp <- to_imperial(total_length, TRUE)
  D_imp <- to_imperial(wire_diameter, FALSE)
  half_len <- A_imp / 2
  # default search window: 10% to 90% of half-length
  if (is.null(target_fraction)) {
    frac_lower <- 0.10
    frac_upper <- 0.90
  } else {
    if (target_fraction <= 0 || target_fraction >= 1) {
      stop("target_fraction must lie between 0 and 1")
    }
    # clamp a window of  plus/minus 0.25 around the target fraction
    frac_lower <- max(0.05, target_fraction - 0.25)
    frac_upper <- min(0.95, target_fraction + 0.25)
  }
  B_lower <- frac_lower * half_len
  B_upper <- frac_upper * half_len
  # root function in B: difference between computed and target inductance
  root_fun <- function(B) {
    loading_coil_inductance(frequency, total_length = A_imp,
                            coil_position = B, wire_diameter = D_imp,
                            metric = FALSE) - inductance
  }
  f_lower <- root_fun(B_lower)
  f_upper <- root_fun(B_upper)
  # try to expand if no sign change
  attempts <- 0
  while (f_lower * f_upper > 0 && attempts < 5) {
    frac_lower <- max(0.01, frac_lower / 2)
    frac_upper <- min(0.99, 1 - (1 - frac_upper) / 2)
    B_lower <- frac_lower * half_len
    B_upper <- frac_upper * half_len
    f_lower <- root_fun(B_lower)
    f_upper <- root_fun(B_upper)
    attempts <- attempts + 1
  }
  if (f_lower * f_upper > 0 || any(is.nan(c(f_lower, f_upper)))) {
    stop("Could not bracket a solution for the coil position; try adjusting total_length, inductance or target_fraction")
  }
  sol <- uniroot(root_fun, lower = B_lower, upper = B_upper)
  B_opt <- sol$root
  frac_opt <- B_opt / half_len
  result <- list(
    frequency = frequency,
    total_length = total_length,
    inductance = inductance,
    coil_position = if (metric) to_metric(B_opt, TRUE) else B_opt,
    fraction_of_half = frac_opt,
    metric = metric
  )
  class(result) <- c("loadedDipoleRecommendation", class(result))
  result
}

#' Summarise coil inductance versus placement for design work
#'
#' This function evaluates \code{loading_coil_inductance()} for a set of coil
#' positions expressed as fractions of the dipole half-length. It is useful
#' when exploring design trade-offs between coil placement and required
#' inductance for a given frequency and total length.
#'
#' @param frequency Operating frequency in megahertz (MHz).
#' @param total_length Total physical length of the dipole (feet or metres
#'   depending on \code{metric}).
#' @param fractions Numeric vector of fractions between 0 and 1 giving the
#'   coil positions relative to the half-length (0 = at the feedpoint,
#'   1 = at the end of the leg).
#' @param wire_diameter Diameter of the dipole conductor; see
#'   \code{loading_coil_inductance()}.
#' @param metric Logical; if \code{TRUE}, lengths are interpreted as metres
#'   and converted internally.
#' @return A data frame with columns \code{fraction_of_half},
#'   \code{coil_position} (in the same units as \code{total_length}) and
#'   \code{inductance} (uH).
#' @examples
#' # Inductance profile for a 60 ft dipole at 3.57 MHz
#' coil_inductance_profile(frequency = 3.57, total_length = 60,
#'                         fractions = seq(0.3, 0.7, by = 0.05))
#' @export
coil_inductance_profile <- function(frequency, total_length,
                                    fractions = seq(0.3, 0.7, by = 0.05),
                                    wire_diameter = 0.065, metric = FALSE) {
  if (any(fractions <= 0 | fractions >= 1)) {
    stop("fractions must lie strictly between 0 and 1")
  }
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }
  to_metric <- function(value, is_length = TRUE) {
    if (metric) return(value)
    inch <- 0.0254
    foot <- 12 * inch
    if (is_length) value * foot else value * inch
  }
  A_imp <- to_imperial(total_length, TRUE)
  D_imp <- to_imperial(wire_diameter, FALSE)
  half_len <- A_imp / 2
  B_imp <- fractions * half_len
  L_vals <- loading_coil_inductance(frequency, total_length = A_imp,
                                    coil_position = B_imp,
                                    wire_diameter = D_imp, metric = FALSE)
  data.frame(
    fraction_of_half = fractions,
    coil_position = if (metric) to_metric(B_imp, TRUE) else B_imp,
    inductance = L_vals
  )
}

#' Print method for loaded dipole recommendations
#'
#' @param x An object returned by \code{recommend_coil_position()}.
#' @param ... Additional arguments (ignored).
#' @export
print.loadedDipoleRecommendation <- function(x, ...) {
  units_length <- if (x$metric) "m" else "ft"
  cat("Loaded dipole recommendation:\n")
  cat(sprintf("  Frequency        : %.4f MHz\n", x$frequency))
  cat(sprintf("  Total length     : %.4f %s\n", x$total_length, units_length))
  cat(sprintf("  Coil inductance  : %.4f uH (per coil)\n", x$inductance))
  cat(sprintf("  Coil position    : %.4f %s from feedpoint\n", x$coil_position, units_length))
  cat(sprintf("  Fraction of half : %.3f\n", x$fraction_of_half))
  invisible(x)
}

#' Estimate efficiency of a loaded shortened dipole
#'
#' This hardened version handles all failure conditions gracefully, always
#' returning numeric NA values when estimates cannot be computed.
#'
#' @param frequency MHz
#' @param total_length ft (or m if metric)
#' @param coil_inductance uH (per coil)
#' @param coil_position ft from feedpoint (or m)
#' @param wire_diameter inches (or m)
#' @param model "simple", "segmented", or "both"
#' @param metric logical
#' @return A list with efficiency estimates; invalid inputs return NA safely.
#' @export
estimate_efficiency <- function(
    frequency,
    total_length,
    coil_inductance,
    coil_position,
    wire_diameter = 0.065,
    model = c("simple", "segmented", "both"),
    metric = FALSE
) {
  
  model <- match.arg(model)
  
  # ---------- Utility ----------
  safe <- function(x) {
    if (is.null(x) || length(x) == 0 || is.nan(x) || is.infinite(x))
      return(NA_real_)
    x
  }
  
  # ---------- Unit conversion ----------
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254; foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }
  
  f  <- safe(frequency)
  A  <- safe(to_imperial(total_length, TRUE))
  B  <- safe(to_imperial(coil_position, TRUE))
  L  <- safe(coil_inductance)
  D  <- safe(to_imperial(wire_diameter, FALSE))
  
  # ---------- Validate physical domain ----------
  if (any(is.na(c(f, A, B, L, D)))) {
    return(list(
      efficiency_simple = NA_real_,
      efficiency_segmented = NA_real_,
      efficiency = NA_real_
    ))
  }
  
  if (A <= 0 || B <= 0 || B >= A/2 || L <= 0) {
    return(list(
      efficiency_simple = NA_real_,
      efficiency_segmented = NA_real_,
      efficiency = NA_real_
    ))
  }
  
  # ---------- Constants ----------
  lambda_ft <- 984 / f
  half_len  <- A / 2
  
  # Radiation resistance roughly:
  Rr <- 80 * pi^2 * (half_len / lambda_ft)^2
  
  if (Rr <= 0 || is.na(Rr)) {
    return(list(
      efficiency_simple = NA_real_,
      efficiency_segmented = NA_real_,
      efficiency = NA_real_
    ))
  }
  
  # ---------- Coil reactance ----------
  XL <- 2 * pi * f * L * 1e-6   # Ohms
  if (is.na(XL) || XL < 0) XL <- NA_real_
  
  # ---------- Coil Q model ----------
  # Avoid division by zero
  Q <- 150   # reasonable assumption for HF air core
  if (Q <= 0 || is.na(Q)) Q <- 150
  
  Rcoil <- safe(XL / Q)
  if (is.na(Rcoil)) Rcoil <- NA_real_
  
  # ---------- Wire loss estimation ----------
  # total wire (inner + outer)
  # if anything goes negative => return NA
  inner_len   <- B
  outer_len   <- half_len - B
  
  if (inner_len <= 0 || outer_len <= 0) {
    return(list(
      efficiency_simple = NA_real_,
      efficiency_segmented = NA_real_,
      efficiency = NA_real_
    ))
  }
  
  # AC resistance proportional to length
  Rwire_simple <- safe((inner_len + outer_len) * 0.02)
  
  # segmented model: lower current on outer segments
  I_ratio <- 0.6
  Rwire_segmented <- safe(inner_len * 0.02 + outer_len * 0.02 * I_ratio^2)
  
  # ---------- Total loss ----------
  Rloss_simple    <- safe(Rcoil + Rwire_simple)
  Rloss_segmented <- safe(Rcoil + Rwire_segmented)
  
  # ---------- Compute efficiencies ----------
  eff_simple <- safe(Rr / (Rr + Rloss_simple))
  eff_segmented <- safe(Rr / (Rr + Rloss_segmented))
  
  # ---------- Final output ----------
  list(
    efficiency_simple   = eff_simple,
    efficiency_segmented = eff_segmented,
    efficiency =
      if (model == "simple") eff_simple else
        if (model == "segmented") eff_segmented else
          mean(c(eff_simple, eff_segmented), na.rm = TRUE)
  )
}

#' Sweep coil placement and inductance for a shortened dipole
#'
#' This function computes the loading coil inductance required to resonate a
#' shortened dipole at a given frequency over a sequence of coil positions.
#' It can optionally estimate antenna efficiency for each coil placement using
#' the "simple", "segmented", or "both" efficiency models. All internal
#' calculations use imperial units (feet and inches), but the \code{metric}
#' argument allows users to specify input lengths in metres instead.
#'
#' The function is hardened for safety: any invalid coil position, numerical
#' failure in the inductance calculation, or failure inside the efficiency
#' estimator will cause \code{NA} values to be inserted for that row rather
#' than stopping execution. This makes the function suitable for sweeps and
#' for use inside vignettes or CRAN examples.
#'
#' @param frequency Operating frequency in megahertz (MHz).
#' @param total_length Total end-to-end dipole length. Feet when
#'   \code{metric = FALSE}, metres otherwise.
#' @param positions Numeric vector of coil positions to evaluate. These are
#'   distances from the feedpoint to each loading coil.
#' @param wire_diameter Conductor diameter. Inches when \code{metric = FALSE},
#'   metres otherwise. Default is 0.065 inch (approx. AWG 14).
#' @param efficiency Logical; if \code{TRUE}, efficiency is estimated using
#'   \code{estimate_efficiency()} for each coil position.
#' @param efficiency_model One of \code{"simple"}, \code{"segmented"}, or
#'   \code{"both"}. Determines which efficiency model to use when
#'   \code{efficiency = TRUE}.
#' @param metric Logical; if \code{TRUE}, length inputs are interpreted as
#'   metres and internally converted to imperial units for the Hall
#'   inductance model.
#'
#' @return A data frame with one row per coil position. Columns include:
#'   \itemize{
#'     \item \code{coil_position}: coil placement in the same units as the
#'       user supplied (feet or metres).
#'     \item \code{inductance}: required inductance in microhenry (uH)
#'       calculated using the Hall short-dipole formula.
#'     \item \code{eff_simple}: estimated efficiency using the simple model
#'       (only returned when \code{efficiency = TRUE} and
#'       \code{efficiency_model} includes "simple").
#'     \item \code{eff_segmented}: estimated efficiency using the segmented
#'       model (only returned when \code{efficiency = TRUE} and
#'       \code{efficiency_model} includes "segmented").
#'   }
#'
#'   Invalid coil positions or numerical failures return \code{NA} for
#'   inductance and/or efficiency fields.
#'
#' @examples
#'
#' # Sweep coil positions from 10 to 20 ft for a 60 ft dipole at 3.57 MHz
#' optimize_inductance_for_position(
#'   frequency     = 3.57,
#'   total_length  = 60,
#'   positions     = seq(10, 20, by = 2),
#'   efficiency    = TRUE,
#'   efficiency_model = "both"
#' )
#'
#' @export
optimize_inductance_for_position <- function(
    frequency,
    total_length,
    positions,
    wire_diameter = 0.065,
    efficiency = FALSE,
    efficiency_model = c("simple", "segmented", "both"),
    metric = FALSE
) {
  efficiency_model <- match.arg(efficiency_model)
  
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254; foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }
  
  A_imp <- to_imperial(total_length, TRUE)
  D_imp <- to_imperial(wire_diameter, FALSE)
  
  # Convert positions
  pos_imp <- to_imperial(positions, TRUE)
  
  n <- length(pos_imp)
  L_vals <- rep(NA_real_, n)
  
  eff_simple   <- rep(NA_real_, n)
  eff_segmented <- rep(NA_real_, n)
  
  # Sweep positions safely
  for (i in seq_len(n)) {
    
    # --- Compute inductance safely ---
    L_try <- try(
      loading_coil_inductance(
        frequency     = frequency,
        total_length  = A_imp,
        coil_position = pos_imp[i],
        wire_diameter = D_imp,
        metric        = FALSE
      ),
      silent = TRUE
    )
    
    if (inherits(L_try, "try-error") || is.nan(L_try)) {
      L_vals[i] <- NA_real_
      next
    }
    
    L_vals[i] <- L_try
    
    if (efficiency) {
      e <- try(
        estimate_efficiency(
          frequency       = frequency,
          total_length    = total_length,
          coil_inductance = L_try,
          coil_position   = positions[i],
          wire_diameter   = wire_diameter,
          metric          = metric,
          model           = efficiency_model
        ),
        silent = TRUE
      )
      
      if (!inherits(e, "try-error")) {
        if ("efficiency_simple" %in% names(e))
          eff_simple[i] <- e$efficiency_simple
        
        if ("efficiency_segmented" %in% names(e))
          eff_segmented[i] <- e$efficiency_segmented
        
        if ("efficiency" %in% names(e))
          eff_simple[i] <- e$efficiency
      }
    }
  }
  
  # Build clean results table
  out <- data.frame(
    coil_position = positions,
    inductance    = L_vals
  )
  
  if (efficiency) {
    if (efficiency_model %in% c("simple", "both"))
      out$eff_simple <- eff_simple
    
    if (efficiency_model %in% c("segmented", "both"))
      out$eff_segmented <- eff_segmented
  }
  
  out
}

#' High-level design function for a shortened loaded dipole
#'
#' This function automates the entire design workflow for a loaded dipole
#' when only the total length and frequency are known, and when the builder
#' is willing to use any inductance up to \code{L_max}. It:
#'
#' 1. Sweeps coil positions
#' 2. Computes the required inductance for resonance
#' 3. Filters out those requiring inductance greater than \code{L_max}
#' 4. Computes efficiency (segmented model)
#' 5. Picks the *optimal* design:
#'    - within 0.1 dB of maximum efficiency, and
#'    - using the smallest inductance
#'
#' @param frequency Operating frequency in MHz.
#' @param total_length The total dipole length (ft unless metric = TRUE).
#' @param L_max Maximum allowed inductance per coil in microhenries.
#' @param wire_diameter Conductor diameter (inches or metres).
#' @param positions Optional vector of coil positions to test (default: 8-22 ft).
#' @param metric Logical; TRUE = metres, FALSE = feet.
#' @return A list containing:
#'   \item{designs}{A data frame of all feasible coil positions with inductance and efficiency}
#'   \item{best}{The chosen optimal design}
#' @examples
#' # Design for a 60 ft dipole at 3.57 MHz with at most 66 uH coils
#' design_loaded_dipole(frequency = 3.57, total_length = 60, L_max = 66)
#' @export
design_loaded_dipole <- function(
    frequency,
    total_length,
    L_max,
    wire_diameter = 0.065,
    positions = seq(8, 22, by = 0.5),
    metric = FALSE
) {
  
  # Unit conversions
  to_imperial <- function(value, is_length = TRUE) {
    if (!metric) return(value)
    inch <- 0.0254; foot <- 12 * inch
    if (is_length) value / foot else value / inch
  }
  
  A_imp <- to_imperial(total_length, TRUE)
  D_imp <- to_imperial(wire_diameter, FALSE)
  pos_imp <- to_imperial(positions, TRUE)
  
  # Required inductance sweep
  L_vals <- loading_coil_inductance(
    frequency     = frequency,
    total_length  = A_imp,
    coil_position = pos_imp,
    wire_diameter = D_imp,
    metric        = FALSE
  )
  
  df <- data.frame(
    coil_position = positions,
    inductance = L_vals
  )
  
  # Keep only feasible inductances
  df <- subset(df, inductance <= L_max)
  
  if (nrow(df) == 0) {
    stop("No feasible designs: all positions require inductance > L_max.")
  }
  
  # Compute efficiency for each design (segmented model)
  eff_vals <- sapply(seq_len(nrow(df)), function(i) {
    B <- df$coil_position[i]
    L <- df$inductance[i]
    
    e <- estimate_efficiency(
      frequency       = frequency,
      total_length    = total_length,
      coil_inductance = L,
      coil_position   = B,
      wire_diameter   = wire_diameter,
      model           = "segmented",
      metric          = metric
    )
    
    return(e$efficiency)
  })
  
  df$efficiency <- eff_vals
  df$loss_dB    <- -10 * log10(df$efficiency)
  
  # Find the optimal design
  min_loss <- min(df$loss_dB, na.rm = TRUE)
  threshold <- min_loss + 0.1   # allow 0.1 dB worse than optimum
  
  near_opt <- subset(df, loss_dB <= threshold)
  
  best <- near_opt[which.min(near_opt$inductance), ]
  
  return(list(
    designs = df,
    best = best
  ))
}

#' Design a single-layer air-core solenoid coil on PVC
#'
#' This function computes the winding geometry, required number of turns,
#' winding length, and wire length for a target inductance using Wheeler's
#' formula. It also supports specifying wire diameter and optional turn
#' spacing, making it suitable for practical HF loading coil construction.
#'
#' Wheeler (single-layer) formula:
#'   L (uH) = (d^2 * N^2) / (18 d + 40 l)
#'
#' @param inductance Target inductance in microhenry (uH).
#' @param pvc_size Nominal Schedule 40 PVC size ("1/2", "3/4", "1", "1-1/4", 
#'   "1-1/2", "2").
#' @param wire_diameter Wire diameter in inches (e.g., 0.064 for AWG 14).
#' @param turn_spacing Optional spacing between turns in inches. If NULL,
#'   assumes tight winding (spacing = wire diameter).
#' @param schedule Pipe schedule (only "40" supported).
#' @return A list containing:
#'   \item{turns}{Estimated number of turns}
#'   \item{winding_length_in}{Total winding length in inches}
#'   \item{diameter_in}{Coil diameter in inches}
#'   \item{wire_length_ft}{Estimated wire length in feet}
#'   \item{summary_table}{Data frame of N plus/minus 2 turns with inductance values}
#' @examples
#' design_pvc_coil(
#'   inductance = 33.5,
#'   pvc_size = "1-1/2",
#'   wire_diameter = 0.064,
#'   turn_spacing = 0.070
#' )
#' @export
design_pvc_coil <- function(
    inductance,
    pvc_size       = "1-1/2",
    wire_diameter  = 0.064,
    turn_spacing   = NULL,
    schedule       = "40"
) {
  pvc_od_in <- c(
    "1/2"   = 0.840,
    "3/4"   = 1.050,
    "1"     = 1.315,
    "1-1/4" = 1.660,
    "1-1/2" = 1.900,
    "2"     = 2.375
  )
  
  if (!(pvc_size %in% names(pvc_od_in)))
    stop("Unsupported PVC size")
  
  d <- pvc_od_in[[pvc_size]]
  
  spacing <- if (is.null(turn_spacing)) wire_diameter else turn_spacing
  
  # Solve Wheeler for N:
  # N = sqrt( L (18 d + 40 l) / d^2 ) but l = N * spacing
  # => N appears in two places; solve numerically
  
  wheeler_residual <- function(N) {
    l <- N * spacing
    L_calc <- (d^2 * N^2) / (18 * d + 40 * l)
    L_calc - inductance
  }
  
  N_est <- uniroot(wheeler_residual, c(5, 200))$root
  
  # Compute final geometry
  winding_length <- N_est * spacing
  wire_length_ft <- (N_est * pi * d) / 12
  
  # Turn sweep table N-2 ... N+2
  N_range <- round(N_est) + (-2:2)
  N_range <- N_range[N_range > 1]
  
  l_vals <- N_range * spacing
  L_vals <- (d^2 * N_range^2) / (18 * d + 40 * l_vals)
  
  summary_tab <- data.frame(
    turns = N_range,
    inductance_uH = L_vals,
    winding_length_in = l_vals
  )
  
  list(
    turns = N_est,
    winding_length_in = winding_length,
    diameter_in = d,
    wire_length_ft = wire_length_ft,
    summary_table = summary_tab
  )
}


#' Plot inductance vs turns for a given PVC coil form
#'
#' @param pvc_size PVC nominal size.
#' @param wire_diameter Wire diameter in inches.
#' @param turn_spacing Turn spacing in inches.
#' @param N_range Range of turns to evaluate.
#' @export
plot_coil_turns <- function(
    pvc_size       = "1-1/2",
    wire_diameter  = 0.064,
    turn_spacing   = NULL,
    N_range        = 5:150
) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 needed")
  
  pvc_od <- c(
    "1/2"=0.840, "3/4"=1.050, "1"=1.315, "1-1/4"=1.660, 
    "1-1/2"=1.900, "2"=2.375
  )
  d <- pvc_od[[pvc_size]]
  spacing <- if (is.null(turn_spacing)) wire_diameter else turn_spacing
  
  l <- N_range * spacing
  L <- (d^2 * N_range^2) / (18*d + 40*l)
  
  df <- data.frame(
    N = N_range,
    inductance = L
  )
  
  ggplot2::ggplot(df, ggplot2::aes(N, inductance)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title="Inductance vs Turns for PVC-loading coil",
      x="Turns", y="Inductance (uH)"
    )
}

#' Full practical design workflow for an optimized loaded dipole
#'
#' This function builds on `design_loaded_dipole()` by:
#'   1. Finding the optimal coil position using efficiency + inductance tradeoff
#'   2. Designing the physical loading coil on PVC
#'   3. Estimating efficiency
#'   4. Producing a builder-friendly summary table
#'   5. Drawing a simple schematic
#'
#' @param frequency Operating frequency in MHz.
#' @param total_length Total physical dipole length (ft or m).
#' @param L_max Maximum acceptable inductance (uH) per coil.
#' @param pvc_size PVC form (e.g., "1-1/2").
#' @param wire_diameter Wire diameter (inches or m).
#' @param turn_spacing Turn spacing in inches (NULL = tight winding).
#' @param positions Coil positions to evaluate (default: 8 to 22 ft or metric-converted).
#' @param metric TRUE = metres, FALSE = feet.
#' @param make_plot Draw schematic?
#'
#' @return A list containing:
#'   \item{optimizer}{Output of design_loaded_dipole()}
#'   \item{best}{Chosen design row}
#'   \item{coil}{PVC coil geometry from design_pvc_coil()}
#'   \item{efficiency}{Efficiency estimates from estimate_efficiency()}
#'   \item{build_table}{Printable build-sheet table}
#' @export
design_loaded_dipole_full <- function(
    frequency,
    total_length,
    L_max,
    pvc_size = "1-1/2",
    wire_diameter = 0.065,
    turn_spacing = NULL,
    positions = seq(8, 22, by = 0.5),
    metric = FALSE,
    make_plot = TRUE
) {
  # -----------------------------------------------------
  # 1. Electrical Optimization (your real engine)
  # -----------------------------------------------------
  opt <- design_loaded_dipole(
    frequency     = frequency,
    total_length  = total_length,
    L_max         = L_max,
    wire_diameter = wire_diameter,
    positions     = positions,
    metric        = metric
  )
  
  best <- opt$best
  B    <- best$coil_position
  L_uH <- best$inductance
  
  # -----------------------------------------------------
  # 2. Coil Physical Design
  # -----------------------------------------------------
  coil <- design_pvc_coil(
    inductance     = L_uH,
    pvc_size       = pvc_size,
    wire_diameter  = wire_diameter,
    turn_spacing   = turn_spacing
  )
  
  # -----------------------------------------------------
  # 3. Efficiency estimation (using segmented model)
  # -----------------------------------------------------
  eff <- estimate_efficiency(
    frequency       = frequency,
    total_length    = total_length,
    coil_inductance = L_uH,
    coil_position   = B,
    wire_diameter   = wire_diameter,
    metric          = metric,
    model           = "both"
  )
  
  # -----------------------------------------------------
  # 4. Build-sheet table
  # -----------------------------------------------------
  units <- if (metric) "m" else "ft"
  
  build_table <- data.frame(
    Item = c(
      "Operating frequency",
      "Total dipole length",
      "Optimal coil position",
      "Required inductance (uH)",
      "PVC coil form",
      "Coil diameter (in)",
      "Turns",
      "Winding length (in)",
      "Wire length (ft)",
      "Efficiency (simple)",
      "Efficiency (segmented)",
      "Overall efficiency"
    ),
    Value = c(
      sprintf("%.3f MHz", frequency),
      sprintf("%.2f %s", total_length, units),
      sprintf("%.2f %s", B, units),
      sprintf("%.2f uH", L_uH),
      pvc_size,
      sprintf("%.3f", coil$diameter_in),
      sprintf("%.1f turns", coil$turns),
      sprintf("%.2f in", coil$winding_length_in),
      sprintf("%.2f ft", coil$wire_length_ft),
      sprintf("%.3f", eff$efficiency_simple),
      sprintf("%.3f", eff$efficiency_segmented),
      sprintf("%.3f", eff$efficiency)
    )
  )
  
  # -----------------------------------------------------
  # 5. Schematic (simple, printable)
  # -----------------------------------------------------
  if (make_plot) {
    plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="",
         main="Optimized Loaded Dipole Schematic (Not to Scale)")
    
    feed_x <- 0.1
    feed_y <- 0.5
    
    # Draw dipole wire
    segments(feed_x, feed_y, 0.9, feed_y)
    
    # Coil marker
    frac <- B / total_length
    coil_x <- feed_x + frac * (0.9 - feed_x)
    segments(coil_x - 0.02, feed_y - 0.05,
             coil_x + 0.02, feed_y + 0.05, lwd = 2)
    
    text(feed_x, feed_y - 0.1, "Feedpoint (coax + choke)", cex = 0.8)
    text(coil_x, feed_y + 0.12,
         sprintf("Loading coil\n%.1f uH at %.2f %s",
                 L_uH, B, units),
         cex = 0.7)
  }
  
  # -----------------------------------------------------
  # Return all objects
  # -----------------------------------------------------
  list(
    optimizer   = opt,
    best        = best,
    coil        = coil,
    efficiency  = eff,
    build_table = build_table,
    metric = metric,
    total_length = total_length,
    frequency = frequency
  )
}


#' Internal plotting helper for loaded dipole schematics
#'
#' Not exported. Used only by design_loaded_dipole_full() and
#' render_loaded_dipole_md().
#'
#' @param design A design object returned by design_loaded_dipole_full()
#' @keywords internal
#' 
design_loaded_dipole_full_plot <- function(design) {
  
  bt <- design$build_table
  
  get_num <- function(label) {
    v <- bt$Value[bt$Item == label]
    if (!length(v)) return(NA_real_)
    as.numeric(sub("[^0-9.].*$", "", v))
  }
  
  total_len  <- get_num("Total dipole length")
  coil_pos   <- get_num("Optimal coil position")
  inductance <- get_num("Required inductance (uH)")
  freq_mhz   <- get_num("Operating frequency")
  
  half_len <- total_len / 2
  units <- if (isTRUE(design$metric)) "m" else "ft"
  
  # Expanded vertical space
  plot(
    NA, NA,
    xlim = c(-half_len - 5, half_len + 5),
    ylim = c(-6, 7),
    xlab = "", ylab = "",
    axes = FALSE,
    asp = 1,
    main = sprintf("Loaded Dipole Schematic (%.3f MHz)", freq_mhz),
    cex.main = 2.5
  )
  
  # Main wire
  segments(-half_len, 0, half_len, 0, lwd = 8)
  
  # Feedpoint
  points(0, 0, pch = 19, col = "blue", cex = 2.5)
  text(0, 1.2, "Feedpoint", col = "blue", cex = 2)
  
  # Coil positions
  coil_x <- c(-coil_pos, coil_pos)
  points(coil_x, c(0,0), pch = 21, bg = "white", col = "red", cex = 3)
  
  text(
    coil_x,
    rep(-2.0, 2),
    sprintf("Coil @ %.1f %s", coil_pos, units),
    col = "red",
    cex = 2
  )
  
  # Move inductance DOWN
  text(
    0, -5.0,
    sprintf("Per-coil inductance: %.2f microH", inductance),
    col = "red",
    cex = 2.3,
    font = 2
  )
  
  # End labels (unchanged)
  text(-half_len, 4.0, sprintf("End (%.1f %s)", -half_len, units), cex = 2)
  text( half_len, 4.0, sprintf("End (%.1f %s)",  half_len, units), cex = 2)
  
  # Distance arrows
  arrow_y <- 5.0
  
  arrows(0, arrow_y, coil_pos, arrow_y, length = 0.12, lwd = 2)
  arrows(0, arrow_y, -coil_pos, arrow_y, length = 0.12, lwd = 2)
  
  # Move coil distance text UP
  text(
    0, arrow_y + 1.5,   # was +0.5, now +1.5
    sprintf("Coils %.1f %s from center", coil_pos, units),
    cex = 2
  )
}

#' Internal plotting helper for loaded dipole schematics
#'
#' Draws a simple (not-to-scale) schematic of a loaded dipole based on the
#' \code{build_table} produced by \code{design_loaded_dipole_full()}.
#'
#' @param design A design object returned by \code{design_loaded_dipole_full()}.
#'
#' @return Called for its side effect of drawing a base R plot.
#' @export
design_loaded_dipole_full_plot <- function(design) {
  
  bt <- design$build_table
  
  get_num <- function(label) {
    v <- bt$Value[bt$Item == label]
    if (!length(v)) return(NA_real_)
    as.numeric(gsub("[^0-9.-]", "", v))
  }
  
  total_len  <- get_num("Total dipole length")
  coil_pos   <- get_num("Optimal coil position")
  inductance <- get_num("Required inductance (uH)")
  freq_mhz   <- get_num("Operating frequency")
  
  half_len <- total_len / 2
  units <- if (isTRUE(design$metric)) "m" else "ft"
  
  # Expanded vertical space
  plot(
    NA, NA,
    xlim = c(-half_len - 5, half_len + 5),
    ylim = c(-6, 7),
    xlab = "", ylab = "",
    axes = FALSE,
    asp = 1,
    main = sprintf("Loaded Dipole Schematic (%.3f MHz)", freq_mhz),
    cex.main = 2.5
  )
  
  # Main wire
  segments(-half_len, 0, half_len, 0, lwd = 8)
  
  # Feedpoint
  points(0, 0, pch = 19, col = "blue", cex = 2.5)
  text(0, 1.2, "Feedpoint", col = "blue", cex = 2)
  
  # Coil positions
  coil_x <- c(-coil_pos, coil_pos)
  points(coil_x, c(0, 0), pch = 21, bg = "white", col = "red", cex = 3)
  
  text(
    coil_x,
    rep(-2.0, 2),
    sprintf("Coil @ %.1f %s", coil_pos, units),
    col = "red",
    cex = 2
  )
  
  # Inductance label (down)
  text(
    0, -5.0,
    sprintf("Per-coil inductance: %.2f uH", inductance),
    col = "red",
    cex = 2.3,
    font = 2
  )
  
  # End labels
  text(-half_len, 4.0, sprintf("End (%.1f %s)", -half_len, units), cex = 2)
  text( half_len, 4.0, sprintf("End (%.1f %s)",  half_len, units), cex = 2)
  
  # Distance arrows
  arrow_y <- 5.0
  
  arrows(0, arrow_y, coil_pos,  arrow_y, length = 0.12, lwd = 2)
  arrows(0, arrow_y, -coil_pos, arrow_y, length = 0.12, lwd = 2)
  
  # Coil distance text (up)
  text(
    0, arrow_y + 1.5,
    sprintf("Coils %.1f %s from center", coil_pos, units),
    cex = 2
  )
}

# ======================================================================
# Reporting and HTML/Markdown rendering helpers
# ======================================================================

#' Encode a PNG file as Base64 for HTML embedding
#'
#' @param file Path to a PNG file.
#' @return A single Base64 string (no data:image/... prefix).
#' @keywords internal
encode_png_base64 <- function(file) {
  bytes <- readBin(file, what = "raw", n = file.info(file)$size)
  base64enc::base64encode(bytes)
}

#' Generate a Base64-encoded QR Code PNG
#'
#' Internal helper used by the report renderer to embed a QR link to the
#' project repository.
#'
#' @param text Text or URL to encode into the QR code.
#' @param size Pixel size of the QR code image (not critical).
#'
#' @return A Base64 string suitable for use inside an HTML
#'   \code{<img src="data:image/png;base64,...">} tag.
#' @keywords internal
generate_qr_base64 <- function(text, size = 400) {
  if (!requireNamespace("qrcode", quietly = TRUE)) {
    stop("Package 'qrcode' is required. Install with install.packages('qrcode').")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required. Install with install.packages('png').")
  }
  
  tmp_png <- tempfile(fileext = ".png")
  
  # qrcode::qr_code() returns a logical matrix
  mat <- qrcode::qr_code(text)
  img <- ifelse(mat, 0, 1)  # black = 0, white = 1
  
  png::writePNG(img, target = tmp_png)
  
  encode_png_base64(tmp_png)
}

#' Draw a dipole schematic to PNG for a loaded dipole design
#'
#' @param design The output list from \code{design_loaded_dipole_full()}.
#' @param file Output PNG file path.
#' @param width Width in inches.
#' @param height Height in inches.
#'
#' @return Invisibly, the file path.
#' @keywords internal
draw_loaded_dipole_schematic <- function(
    design,
    file,
    width  = 9,
    height = 3
) {
  bt <- design$build_table
  
  get_num <- function(label) {
    v <- bt$Value[bt$Item == label]
    if (!length(v)) return(NA_real_)
    as.numeric(gsub("[^0-9.-]", "", v))
  }
  
  total_len  <- get_num("Total dipole length")
  coil_pos   <- get_num("Optimal coil position")
  inductance <- get_num("Required inductance (uH)")
  freq_mhz   <- get_num("Operating frequency")
  
  half_len <- total_len / 2
  units    <- if (isTRUE(design$metric)) "m" else "ft"
  
  grDevices::png(
    filename = file,
    width    = width,
    height   = height,
    units    = "in",
    res      = 150
  )
  
  par(mar = c(2, 2, 3, 2))
  plot(
    NA, NA,
    xlim = c(-half_len - 5, half_len + 5),
    ylim = c(-6, 7),
    xlab = "", ylab = "",
    axes = FALSE,
    asp  = 1,
    main = sprintf("Loaded Dipole Schematic (%.3f MHz)", freq_mhz)
  )
  
  # Dipole wire
  segments(-half_len, 0, half_len, 0, lwd = 8)
  
  # Feedpoint
  points(0, 0, pch = 19, col = "blue", cex = 2)
  text(0, 1.2, "Feedpoint", col = "blue", cex = 1.4)
  
  # Coils (symmetrical)
  coil_x <- c(-coil_pos, coil_pos)
  points(coil_x, c(0, 0), pch = 21, bg = "white", col = "red", cex = 1.8)
  text(
    coil_x,
    rep(-2.0, 2),
    sprintf("Coil @ %.1f %s", coil_pos, units),
    col = "red",
    cex = 1.3
  )
  
  # Inductance label
  text(
    0, -5.0,
    sprintf("Per-coil inductance: %.2f uH", inductance),
    col  = "red",
    cex  = 1.4,
    font = 2
  )
  
  # End labels
  text(-half_len, 4.0, sprintf("End (%.1f %s)", -half_len, units), cex = 1.3)
  text( half_len, 4.0, sprintf("End (%.1f %s)",  half_len, units), cex = 1.3)
  
  # Distance arrows
  arrow_y <- 5.0
  arrows(0, arrow_y, coil_pos,  arrow_y, length = 0.12, lwd = 2)
  arrows(0, arrow_y, -coil_pos, arrow_y, length = 0.12, lwd = 2)
  
  text(
    0, arrow_y + 1.0,
    sprintf("Coils %.1f %s from center", coil_pos, units),
    cex = 1.5
  )
  
  grDevices::dev.off()
  invisible(file)
}

#' Draw a dipole schematic and return a Base64 string
#'
#' @param design Design object from \code{design_loaded_dipole_full()}.
#' @param width Width in inches.
#' @param height Height in inches.
#'
#' @return Base64-encoded PNG suitable for HTML embedding.
#' @keywords internal
draw_loaded_dipole_schematic_base64 <- function(
    design,
    width  = 9,
    height = 3
) {
  tmp_png <- tempfile(fileext = ".png")
  draw_loaded_dipole_schematic(
    design = design,
    file   = tmp_png,
    width  = width,
    height = height
  )
  encode_png_base64(tmp_png)
}

#' Render a markdown build report for a loaded dipole design
#'
#' This function creates a self-contained Markdown report for a design created
#' by \code{design_loaded_dipole_full()}. The report includes:
#' \itemize{
#'   \item A formatted build table
#'   \item An embedded (Base64) schematic figure (optional)
#'   \item An embedded (Base64) QR code linking to the package repository
#'   \item A small embedded CSS block so the HTML produced via pandoc looks
#'         reasonable without external files.
#' }
#'
#' @param design Result from \code{design_loaded_dipole_full()}.
#' @param file Output \code{.md} filename.
#' @param include_plot Logical; if \code{TRUE}, include an embedded schematic.
#'
#' @return Invisibly returns the markdown file path.
#' @export
render_loaded_dipole_md <- function(
    design,
    file,
    include_plot = TRUE
) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required. Install with install.packages('knitr').")
  }
  
  schematic_b64 <- if (include_plot) {
    draw_loaded_dipole_schematic_base64(design)
  } else {
    NULL
  }
  
  qr_b64 <- generate_qr_base64("https://github.com/YoJimboDurant/loadedDipole")
  
  md <- c(
    "# Loaded Dipole Design Report",
    "",
    "Generated by **loadedDipole** by KE4MKG (Jim Durant)",
    "",
    "## Design Summary",
    "",
    knitr::kable(design$build_table, format = "markdown"),
    ""
  )
  
  # Antenna schematic
  if (!is.null(schematic_b64)) {
    md <- c(
      md,
      "## Antenna Schematic",
      "",
      sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%;">',
              schematic_b64),
      ""
    )
  }
  
  # Coil build table
  if (!is.null(design$coil$summary_table)) {
    md <- c(
      md,
      "## Coil Build Details",
      "",
      knitr::kable(design$coil$summary_table, format = "markdown"),
      ""
    )
  }
  
  # QR Code
  md <- c(
    md,
    "## Project Link (QR Code)",
    "",
    sprintf('<img src="data:image/png;base64,%s" width="200">', qr_b64),
    ""
  )
  
  md <- c(
    md,
    "## Notes",
    "",
    "This report summarises the electrical and mechanical design of a shortened loaded HF dipole ",
    "based on the Jerry Hall short-dipole model.",
    ""
  )
  
  # Technical notes block in smaller font (ASCII-safe)
  technical_notes <- c(
    "## Technical Notes, Modeling Assumptions, and Performance Expectations",
    "",
    "### 1. Electrical, Physical, and Operating Assumptions",
    "",
    "All impedance, reactance, radiation resistance, and loss values are computed at the user-supplied frequency f.",
    "Angular frequency is defined as omega = 2*pi*f and wavelength is lambda = c / f.",
    "We use c = 2.99792458e8 m/s for the speed of light.",
    "",
    "Sources: Balanis Antenna Theory; IEEE Std 145-2013.",
    "",
    "### 2. Radiation Resistance Models",
    "",
    "The package uses standard thin-wire approximations. For electrically short dipoles:",
    "",
    "* R_r ~ 80 * pi^2 * (L / lambda)^2",
    "",
    "This smoothly transitions to the classical half-wave value:",
    "",
    "* R_r (half-wave dipole) ~ 73 ohms",
    "",
    "Sources: Balanis; ARRL Antenna Book.",
    "",
    "### 3. Loss Models",
    "",
    "**Skin Effect Loss**",
    "",
    "* Uses frequency-dependent skin depth in copper (sigma ~ 5.8e7 S/m)",
    "* Classic RF conductor resistance formulas are used",
    "",
    "**Coil Loss**",
    "",
    "* Coil loss is R_coil = omega * L / Q",
    "* Wheeler-type inductance formulas estimate L",
    "",
    "**Ground Loss (optional)**",
    "",
    "* simple height-above-ground approximation",
    "",
    "Sources: Kraus; Wheeler (IRE 1928); ARRL Handbook.",
    "",
    "### 4. Resonance and Reactive Compensation",
    "",
    "The antenna is resonant when the input reactance X_in(f) is approximately zero.",
    "Required loading inductance is estimated from:",
    "",
    "* L_required = -X_antenna(f) / omega",
    "",
    "Sources: ARRL; Balanis.",
    "",
    "### 5. Efficiency Definitions (fraction form)",
    "",
    "* Radiation efficiency: eta_rad = R_r / (R_r + R_loss)",
    "* System efficiency: eta_sys = eta_rad * eta_match  (if matching losses modeled)",
    "* Relative efficiency: compared to a half-wave dipole at the same frequency",
    "",
    "Efficiencies are reported as fractions (for example, 0.75 means 75 percent).",
    "",
    "### 6. Performance Expectations for Shortened Dipoles",
    "",
    "Realistic radiation-efficiency ranges for shortened HF dipoles:",
    "",
    "* 0.25 lambda to 0.15 lambda: efficiency 0.70 to 0.95  (high performance)",
    "* 0.15 lambda to 0.10 lambda: efficiency 0.40 to 0.70  (heavily loaded dipoles)",
    "* Below 0.10 lambda: efficiency 0.05 to 0.40  (very short antennas dominated by loss)",
    "",
    "**Good performance thresholds:**",
    "",
    "* >= 0.80   excellent for any shortened dipole",
    "* >= 0.60   very good for 0.15 lambda to 0.20 lambda",
    "* >= 0.40   good for very short (< 0.12 lambda) portable or stealth designs",
    "",
    "Efficiency is not the same as SWR. SWR reflects matching, not radiated power.",
    "As length decreases below roughly 0.12 lambda, radiation resistance falls quickly and loading-coil loss dominates.",
    "",
    "### 7. Modeling Scope and Limitations",
    "",
    "The models in loadedDipole are based on classical thin-wire approximations,",
    "closed-form conductor and coil-loss equations, and lumped loading elements.",
    "This is not a full NEC or method-of-moments solver.",
    "",
    "### 8. References",
    "",
    "* ARRL Antenna Book, 24th Edition",
    "* RSGB Radio Communications Handbook",
    "* Balanis: Antenna Theory",
    "* Wheeler: IRE Proceedings, 1928",
    "* Kraus and Marhefka: Antennas",
    "* IEEE Std 145-2013"
  )
  
  
  md <- c(md,
          technical_notes)
 browser()
  
  writeLines(md, con = file)
  invisible(file)
}


#' Render a full HTML build report for a loaded dipole design
#'
#' This convenience wrapper calls \code{render_loaded_dipole_md()} to build a
#' temporary Markdown file and then uses \code{rmarkdown::pandoc_convert()} to
#' produce a standalone HTML document. All styling and images are embedded so
#' the HTML file is fully portable.
#'
#' @param design Result from \code{design_loaded_dipole_full()}.
#' @param file Output HTML file path. Relative paths are resolved against
#'   \code{getwd()}.
#' @param include_plot Logical; if \code{TRUE}, include the schematic figure.
#'
#' @return Invisibly returns the HTML file path.
#' @export
render_loaded_dipole_html <- function(
    design,
    file = "loaded_dipole_report.html",
    include_plot = TRUE
) {
  
  # ----------- Internal helpers ---------------------------------------------
  
  qst_css_block <- function() {
    "
<style>
body {
  font-family: 'Helvetica','Arial',sans-serif;
  background: #fff;
  color: #222;
  margin: 40px;
  line-height: 1.55;
}
h1,h2,h3 {
  color: #003366;
  font-weight: 700;
}
h1 { font-size: 2.2em; border-bottom: 3px solid #003366; padding-bottom: 8px; }
h2 { font-size: 1.7em; border-bottom: 2px solid #003366; padding-bottom: 4px; }
table {
  width: 70%;
  margin-left: auto;
  margin-right: auto;
  border-collapse: collapse;
  margin-bottom: 25px;
}

th, td {
  padding: 8px 12px;
}

th {
  background: #003366;
  color: white;
  padding: 10px;
  border-bottom: 2px solid #002244;
}
td {
  padding: 8px;
  border-bottom: 1px solid #ccc;
}
.footer {
  margin-top: 40px;
  padding-top: 10px;
  border-top: 2px solid #999;
  font-size: 0.9em;
  color: #555;
}
.qr-wrapper {
  text-align: center;
  margin-top: 25px;
  margin-bottom: 25px;
}
.qr-wrapper img {
  width: 140px;
  height: 140px;
  border: 4px solid #003366;
  border-radius: 6px;
}
.notes-block {
  font-size: 0.85em;
  line-height: 1.45;
  margin-top: 40px;
}
</style>
"
  }

generate_qr_base_base64 <- function(url) {
  if (!requireNamespace("qrcode", quietly = TRUE)) return(NULL)
  if (!requireNamespace("png", quietly = TRUE)) return(NULL)
  if (!requireNamespace("base64enc", quietly = TRUE)) return(NULL)
  
  m <- qrcode::qr_code(url)
  img <- ifelse(m, 0, 1)
  tmp <- tempfile(fileext = ".png")
  png::writePNG(img, tmp)
  enc <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0("data:image/png;base64,", enc)
}

generate_schematic_base64 <- function(design) {
  if (!requireNamespace("png", quietly = TRUE)) return(NULL)
  if (!requireNamespace("base64enc", quietly = TRUE)) return(NULL)
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 1800, height = 1200, res = 160)
  design_loaded_dipole_full_plot(design)
  dev.off()
  
  enc <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0("data:image/png;base64,", enc)
}

generate_coil_table_html <- function(design) {
  tab <- design$coil$summary_table
  knitr::kable(tab, format = "html")
}

# ----------- Start building HTML ------------------------------------------

html <- c(
  "<html><head><meta charset='UTF-8'>",
  qst_css_block(),
  "</head><body>",
  "<h1>Loaded Dipole Design Report</h1>"
)

# ----------- Summary table -------------------------------------------------

html <- c(html,
          "<h2>Design Summary</h2>",
          knitr::kable(design$build_table, format = "html")
)

# ----------- Coil ----------------------------------------------------------

html <- c(html,
          "<h2>Coil Build Details</hh2>",
          generate_coil_table_html(design)
)

# ----------- Schematic -----------------------------------------------------

if (include_plot) {
  schematic <- generate_schematic_base64(design)
  if (!is.null(schematic)) {
    html <- c(html,
              "<h2>Antenna Schematic</h2>",
              sprintf("<img src='%s' style='max-width:100%%;'>", schematic)
    )
  }
}

# ----------- QR Code -------------------------------------------------------

qr <- generate_qr_base_base64("https://github.com/YoJimboDurant/loadedDipole")

if (!is.null(qr)) {
  html <- c(html,
            "<h2>Project Repository</h2>",
            "<div class='qr-wrapper'>",
            sprintf("<img src='%s'>", qr),
            "<br><em>Scan for source code</em>",
            "</div>"
  )
}

# ----------- Technical Notes ----------------------------------------------

notes <- c(
  "<div class='notes-block'>",
  "<h2>Technical Notes, Modeling Assumptions, and Performance Expectations</h2>",
  
  "<h3>1. Electrical, Physical, and Operating Assumptions</h3>",
  "<p>All impedance, reactance, radiation resistance, and loss values are computed at the user-supplied frequency f.",
  "Angular frequency is omega = 2*pi*f and wavelength is lambda = c / f.</p>",
  "<p>Speed of light used: c = 2.99792458e8 m/s.</p>",
  
  "<h3>2. Radiation Resistance Models</h3>",
  "<p>Short dipoles use the classical approximation R_r = 80 * pi^2 * (L/lambda)^2, transitioning smoothly",
  "to the half-wave value of about 73 ohms.</p>",
  
  "<h3>3. Loss Models</h3>",
  "<ul>",
  "<li>Skin effect loss uses standard RF conductor formulas with copper conductivity 5.8e7 S/m.</li>",
  "<li>Coil loss uses R_coil = omega * L / Q with Wheeler-type L estimates.</li>",
  "<li>Optional ground loss uses a simple height-above-ground approximation.</li>",
  "</ul>",
  
  "<h3>4. Resonance and Loading</h3>",
  "<p>Resonance occurs when X_in(f) ~ 0. Required loading inductance follows:",
  "L_required = -X_antenna(f) / omega.</p>",
  
  "<h3>5. Efficiency Definitions</h3>",
  "<ul>",
  "<li>Radiation efficiency: eta_rad = R_r / (R_r + R_loss)</li>",
  "<li>System efficiency: eta_sys = eta_rad * eta_match (if matching included)</li>",
  "<li>Relative efficiency: compared to a half-wave dipole at same frequency</li>",
  "</ul>",
  
  "<h3>6. Performance Expectations for Shortened Dipoles</h3>",
  "<p>Typical real-world efficiency ranges (HF band):</p>",
  "<ul>",
  "<li>0.25 lambda to 0.15 lambda: 0.70 to 0.95 (high performance)</li>",
  "<li>0.15 lambda to 0.10 lambda: 0.40 to 0.70</li>",
  "<li>Below 0.10 lambda: 0.05 to 0.40</li>",
  "</ul>",
  "<p>Good performance thresholds:</p>",
  "<ul>",
  "<li>0.80 or higher: excellent shortened dipole</li>",
  "<li>0.60 or higher: very good for 0.15 to 0.20 lambda</li>",
  "<li>0.40 or higher: good for very short (&lt;0.12 lambda) designs</li>",
  "</ul>",
  
  "<h3>7. Limitations</h3>",
  "<p>Models based on classical thin-wire theory, closed-form loss equations, and lumped loading.",
  "Not a NEC or MOM solver.</p>",
  
  "<h3>8. References</h3>",
  "<ul>",
  "<li>ARRL Antenna Book</li>",
  "<li>RSGB Radio Communications Handbook</li>",
  "<li>Balanis: Antenna Theory</li>",
  "<li>Wheeler 1928 IRE Proceedings</li>",
  "<li>Kraus and Marhefka: Antennas</li>",
  "<li>IEEE Std 145-2013</li>",
  "</ul>",
  
  "</div>"
)

html <- c(html, notes)

# ----------- Footer --------------------------------------------------------

html <- c(html,
          "<div class='footer'>",
          "Report generated using the <strong>loadedDipole</strong> package.<br>",
          "Author: <strong>Jim Durant, KE4MKG</strong><br>",
          sprintf("Generated on: %s", Sys.time()),
          "</div>"
)

html <- c(html, "</body></html>")

# ----------- Write output --------------------------------------------------

if(!is.null(file)){
file <- normalizePath(file, mustWork = FALSE)
writeLines(html, file)
message("HTML report written to: ", file)
invisible(file)
}else{
  html
  }
}