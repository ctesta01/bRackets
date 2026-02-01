library(dplyr)
library(ggplot2)
library(readr)
library(purrr)

df <- readr::read_csv("data_annotated.csv")

df_err <- df %>%
  group_by(group, week) %>%
  summarise(
    y_mean = mean(y),
    ymin   = min(y),
    ymax   = max(y),
    .groups = "drop"
  )

# Helpers to pull values (mean by default)
pull_y <- function(g, w, stat = c("y_mean","ymin","ymax")) {
  stat <- match.arg(stat)
  df_err %>% filter(group == g, week == w) %>% pull(.data[[stat]])
}

# ---- bracket builder (returns data frames for segments + labels) ----

build_bracket <- function(
    # High-level spec (recommended)
  orientation = c("v", "h"),   # "v" = vertical bracket at one x; "h" = horizontal bracket at one y
  x0 = NULL, x1 = NULL,        # for "v": x0 used; for "h": x0..x1 used
  y0 = NULL, y1 = NULL,        # for "v": y0..y1 used; for "h": y0 used (and y1 ignored)
  side = c("left","right","up","down"),
  tick = 0.10,
  pad = 0.00,
  text = NULL,
  text_nudge = 0.08,
  text_size = 4,
  color = "black",
  linewidth = 0.9,
  linetype = "solid",
  
  # Low-level override: if you supply `segments`, we use them as-is.
  # Must have columns x, xend, y, yend. (One row per segment.)
  segments = NULL,
  
  # Low-level override for label placement: x, y, label
  label_xy = NULL
) {
  orientation <- match.arg(orientation)
  side <- match.arg(side)
  
  seg_df <- NULL
  lab_df <- NULL
  
  if (!is.null(segments)) {
    seg_df <- segments %>%
      mutate(color = color, linewidth = linewidth, linetype = linetype)
  } else {
    if (orientation == "v") {
      stopifnot(!is.null(x0), !is.null(y0), !is.null(y1))
      yb <- min(y0, y1) - pad
      yt <- max(y0, y1) + pad
      
      # tick direction depends on left/right
      if (!(side %in% c("left","right"))) stop("For orientation='v', side must be 'left' or 'right'.")
      x_br   <- x0
      x_tick <- if (side == "left") x_br - tick else x_br + tick
      
      seg_df <- tibble::tibble(
        x    = c(x_br, x_br, x_br),
        xend = c(x_tick, x_br, x_tick),
        y    = c(yb, yb, yt),
        yend = c(yb, yt, yt)
      ) %>%
        mutate(color = color, linewidth = linewidth, linetype = linetype)
      
      if (is.null(label_xy)) {
        x_text <- if (side == "left") x_br - tick - text_nudge else x_br + tick + text_nudge
        y_text <- (yb + yt) / 2
      } else {
        x_text <- label_xy$x
        y_text <- label_xy$y
      }
      
      lab_df <- tibble::tibble(
        x = x_text, y = y_text, label = text,
        hjust = if (side == "left") 1 else 0,
        color = color, size = text_size
      )
    }
    
    if (orientation == "h") {
      stopifnot(!is.null(x0), !is.null(x1), !is.null(y0))
      xl <- min(x0, x1)
      xr <- max(x0, x1)
      yb <- y0
      
      # tick direction depends on up/down
      if (!(side %in% c("up","down"))) stop("For orientation='h', side must be 'up' or 'down'.")
      y_tick <- if (side == "up") yb + tick else yb - tick
      
      seg_df <- tibble::tibble(
        x    = c(xl, xl, xr),
        xend = c(xl, xr, xr),
        y    = c(yb, yb, yb),
        yend = c(y_tick, yb, y_tick)
      ) %>%
        mutate(color = color, linewidth = linewidth, linetype = linetype)
      
      if (is.null(label_xy)) {
        x_text <- (xl + xr) / 2
        y_text <- if (side == "up") yb + tick + text_nudge else yb - tick - text_nudge
      } else {
        x_text <- label_xy$x
        y_text <- label_xy$y
      }
      
      lab_df <- tibble::tibble(
        x = x_text, y = y_text, label = text,
        hjust = 0.5,
        color = color, size = text_size
      )
    }
  }
  
  list(segments = seg_df, labels = lab_df)
}

add_brackets <- function(
    plt,
    bracket_specs
) {
  built <- purrr::map(bracket_specs, ~do.call(build_bracket, .x))
  segs  <- dplyr::bind_rows(purrr::map(built, "segments"))
  labs  <- dplyr::bind_rows(purrr::map(built, "labels"))
  
  plt +
    geom_segment(
      data = segs,
      inherit.aes = FALSE,
      aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = segs$linewidth,
      linetype  = segs$linetype,
      color     = segs$color
    ) +
    geom_text(
      data = labs,
      inherit.aes = FALSE,
      aes(x = x, y = y, label = label),
      color = labs$color,
      size  = labs$size,
      hjust = labs$hjust
    )
}

# -----------------------------------------------------------------------
# “All comparisons that were originally there” (matching the example image)
# -----------------------------------------------------------------------

# Numeric x positions for discrete weeks (W1 = 1, W2 = 2)
x_W1 <- 1
x_W2 <- 2

# 1) Week 1: High vs Control (vertical bracket on left)
y_high_W1 <- pull_y("High", "W1", "y_mean")
y_ctrl_W1 <- pull_y("Control", "W1", "y_mean")

# 2) Week 1: Control vs Low (vertical bracket on left)
y_low_W1  <- pull_y("Low", "W1", "y_mean")

# 3) High group change W1->W2 (horizontal dashed bracket near top)
y_high_W2 <- pull_y("High", "W2", "y_mean")
y_high_top <- max(y_high_W1, y_high_W2) + 0.35  # put bracket above the points; tweak as needed

# 4) Low group change W1->W2 (horizontal dashed bracket near bottom)
y_low_W2 <- pull_y("Low", "W2", "y_mean")
y_low_bot <- min(y_low_W1, y_low_W2) - 0.35     # put bracket below the points; tweak as needed

bracket_specs <- list(
  # Week 1: High vs Control
  list(
    orientation = "v",
    x0 = x_W1 - 0.22,          # nudge left of W1
    y0 = y_ctrl_W1,
    y1 = y_high_W1,
    side = "right",            # ticks point into the panel
    tick = 0.10,
    pad = 0.05,
    text = "***  d = 1.65",
    color = "black",
    linewidth = 0.9
  ),
  
  # Week 1: Control vs Low
  list(
    orientation = "v",
    x0 = x_W1 - 0.34,          # slightly further left, like the example
    y0 = y_low_W1,
    y1 = y_ctrl_W1,
    side = "right",
    tick = 0.10,
    pad = 0.05,
    text = "***  d = -1.60",
    color = "black",
    linewidth = 0.9
  ),
  
  # High: W1 -> W2 (top dashed)
  list(
    orientation = "h",
    x0 = x_W1, x1 = x_W2,
    y0 = y_high_top + .5,
    side = "down",             # ticks downward (bracket above)
    tick = 0.10,
    text = "***  d = -0.76",
    color = "black",
    linewidth = 0.9,
    linetype = "dashed"
  ),
  
  # Low: W1 -> W2 (bottom dashed)
  list(
    orientation = "h",
    x0 = x_W1, x1 = x_W2,
    y0 = y_low_bot-.5,
    side = "up",               # ticks upward (bracket below)
    tick = 0.10,
    text = "*  d = 0.74",
    color = "black",
    linewidth = 0.9,
    linetype = "dashed"
  )
)

# Usage: assuming you already have your base plot in `plt`
plt2 <- add_brackets(plt, bracket_specs) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 30, 5.5, 30))
plt2
