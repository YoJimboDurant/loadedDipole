## -----------------------------------------------------------------------------
library(loadedDipole)

## -----------------------------------------------------------------------------
frequency    <- 3.57     # MHz (FT8)
total_length <- 60       # ft
L_coil       <- 66       # microhenry per coil

## -----------------------------------------------------------------------------
rec <- recommend_coil_position(
  frequency    = frequency,
  total_length = total_length,
  inductance   = L_coil
)

rec

## ----fig.width=8, fig.height=5------------------------------------------------
plot_inductor_placement(
  frequency    = frequency,
  total_length = total_length
)

## -----------------------------------------------------------------------------
eff <- estimate_efficiency(
  frequency       = frequency,
  total_length    = total_length,
  coil_inductance = L_coil,
  coil_position   = rec$coil_position,
  model           = "both"
)

eff

## ----fig.width=8, fig.height=5------------------------------------------------
library(ggplot2)

wire_diameter = 0.065

# sweep from 10% to 90% of half-length
half_len <- total_length / 2
fractions <- seq(0.01, 0.99, by = 0.01)
coil_positions <- fractions * half_len

eff_list <- lapply(seq_along(coil_positions), function(i) {
  B <- coil_positions[i]

  # compute required inductance at this coil position
  L_req <- loading_coil_inductance(
    frequency,
    total_length = total_length,
    coil_position = B,
    wire_diameter = wire_diameter,
    metric = FALSE
  )

  # compute efficiency using segmented model
  eff <- estimate_efficiency(
    frequency       = frequency,
    total_length    = total_length,
    coil_inductance = L_req,
    coil_position   = B,
    model           = "segmented"
  )

  data.frame(
    coil_position = B,
    fraction_of_half = fractions[i],
    inductance = L_req,
    efficiency_loss = -10 * log10(eff$efficiency)

  )
})

eff_df <- do.call(rbind, eff_list)

# plot the result
ggplot(eff_df, aes(x = coil_position, y = efficiency_loss)) +
  geom_line(size = 1.1) +
  geom_vline(
    xintercept = rec$coil_position,
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "text",
    x = rec$coil_position,
    y = max(eff_df$efficiency),
    label = sprintf("Recommended: %.2f ft", rec$coil_position),
    vjust = -0.5,
    color = "red"
  ) +
  labs(
    x = "Coil position from feedpoint (ft)",
    y = "Efficiency Loss (db)",
    title = "Efficiency vs Coil Position (Required Inductance Computed per Position)",
    subtitle = "Dashed line shows recommended coil placement"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------

positions <- seq(8, 22, by = 0.5)   # ft from feedpoint

opt_df <- optimize_inductance_for_position(
  frequency       = 3.57,
  total_length    = 60,
  positions       = positions,
  efficiency      = TRUE,
  efficiency_model = "segmented"
)

opt_df

## ----fig.width=8, fig.height=5------------------------------------------------

ggplot(opt_df, aes(x = coil_position, y = inductance)) +
  geom_line(size = 1) +
  labs(
    title = "Required inductance vs coil position",
    x     = "Coil position from feedpoint (ft)",
    y     = "Inductance (µH)"
  )

## ----fig.width=8, fig.height=5------------------------------------------------
ggplot(opt_df, aes(x = coil_position, y = eff_segmented)) +
  geom_line(size = 1) +
  labs(
    title = "Efficiency vs coil position (segmented model)",
    x     = "Coil position from feedpoint (ft)",
    y     = "Efficiency (0–1)"
  )

## ----design-60ft, message=FALSE, fig.width=8, fig.height=4--------------------
library(loadedDipole)

design <- design_loaded_dipole(
  frequency    = 3.57,
  total_length = 60,
  L_max        = 66
)

design$best

## ----design-plot, fig.width=9, fig.height=4-----------------------------------
library(ggplot2)

df <- design$designs

ggplot(df, aes(x = coil_position, y = inductance)) +
  geom_line(color="steelblue", size=1) +
  geom_point(data = design$best, aes(x = coil_position, y = inductance),
             color="red", size=3) +
  labs(
    title = "Required inductance vs coil position (60 ft dipole, 3.57 MHz)",
    x = "Coil position (ft from feedpoint)",
    y = "Inductance (µH)"
  )

## ----design-efficiency-plot, fig.width=9, fig.height=4------------------------
ggplot(df, aes(x = coil_position, y = loss_dB)) +
  geom_line(size=1) +
  geom_point(data = design$best, aes(x = coil_position, y = loss_dB),
             color="red", size=3) +
  labs(
    title = "Efficiency loss vs coil position (segmented model)",
    x = "Coil position (ft)",
    y = "Loss relative to perfect dipole (dB)"
  )

## ----pvc-design, message=FALSE------------------------------------------------
coil <- design_pvc_coil(
  inductance     = 33.5,
  pvc_size       = "1-1/2",
  wire_diameter  = 0.064,
  turn_spacing   = 0.070
)

coil

## ----pvc-plot, fig.width=8, fig.height=4--------------------------------------
plot_coil_turns(
  pvc_size       = "1-1/2",
  wire_diameter  = 0.064,
  turn_spacing   = 0.070,
  N_range        = 10:70
)

## ----full_report, eval = FALSE------------------------------------------------
# des <- design_loaded_dipole_full(frequency = 3.574, total_length = 60, L_max = 66,
#                                  pvc_size = "1-1/2", make_plot = FALSE
#                                   )
# render_loaded_dipole_html(
#   design = des,
#   file = "loaded_dipole_report.html",
#   include_plot = FALSE
# )
# 

## ----full_reportx, echo = FALSE-----------------------------------------------
des <- design_loaded_dipole_full(frequency = 3.574, total_length = 60, L_max = 66, 
                                 pvc_size = "1-1/2", make_plot = FALSE
                                  )
htmltools::HTML(
 render_loaded_dipole_html(
  design = des,
  file = NULL,
  include_plot = TRUE
  )
)


