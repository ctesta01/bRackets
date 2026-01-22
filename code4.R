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
#
# Desired label placement logic:
# - Horizontal bracket, ticks going UP: text should be BELOW the bracket
# - Horizontal bracket, ticks going DOWN: text should be ABOVE the bracket
# - Vertical bracket on LEFT: text to the LEFT
# - Vertical bracket on RIGHT: text to the RIGHT

build_bracket <- function(
    # High-level spec (recommended)
  orientation = c("v", "h"),   # "v" = vertical bracket at one x; "h" = horizontal bracket at one y
  x0 = NULL, x1 = NULL,        # for "v": x0 used; for "h": x0..x1 used
  y0 = NULL, y1 = NULL,        # for "v": y0..y1 used; for "h": y0 used (and y1 ignored)
  
  # Intuitive placement:
  # - If orientation == "v": position is where the bracket sits ("left" or "right"). Ticks point inward.
  # - If orientation == "h": position is where the bracket sits ("top" or "bottom"). Ticks point inward.
  position = NULL,
  
  # Back-compat (deprecated): older code used side = left/right/up/down.
  side = NULL,
  
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
  
  # Low-level override for label placement: x, y
  label_xy = NULL
) {
  orientation <- match.arg(orientation)
  
  # Resolve position (new) from side (old) if needed
  if (is.null(position)) {
    if (!is.null(side)) {
      if (orientation == "v") {
        # Old: side was left/right (interpreted as bracket location)
        if (!(side %in% c("left", "right"))) {
          stop("For orientation='v', `side` must be 'left' or 'right' (deprecated). Use `position` instead.")
        }
        position <- side
      } else {
        # Old: side was up/down; map to top/bottom
        if (!(side %in% c("up", "down"))) {
          stop("For orientation='h', `side` must be 'up' or 'down' (deprecated). Use `position` instead.")
        }
        position <- if (side == "up") "bottom" else "top"
      }
    } else {
      # Defaults if nothing provided
      position <- if (orientation == "v") "left" else "top"
    }
  }
  
  # Validate position
  if (orientation == "v" && !(position %in% c("left", "right"))) {
    stop("For orientation='v', `position` must be 'left' or 'right'.")
  }
  if (orientation == "h" && !(position %in% c("top", "bottom"))) {
    stop("For orientation='h', `position` must be 'top' or 'bottom'.")
  }
  
  seg_df <- NULL
  lab_df <- NULL
  
  if (!is.null(segments)) {
    seg_df <- segments %>%
      mutate(color = color, linewidth = linewidth, linetype = linetype)
    
    # If user provides custom segments and ALSO wants default label placement,
    # we cannot reliably infer the bracket geometry. Require label_xy if text is desired.
    if (!is.null(text)) {
      if (is.null(label_xy)) {
        stop("When `segments` is provided, please also provide `label_xy` for the text position.")
      }
      lab_df <- tibble::tibble(
        x = label_xy$x,
        y = label_xy$y,
        label = text,
        hjust = 0.5,
        vjust = 0.5,
        color = color,
        size = text_size
      )
    }
    
  } else {
    
    if (orientation == "v") {
      stopifnot(!is.null(x0), !is.null(y0), !is.null(y1))
      
      yb <- min(y0, y1) - pad
      yt <- max(y0, y1) + pad
      
      # Bracket sits at x_br; ticks point inward:
      # - position == left  => ticks go RIGHT (+tick)
      # - position == right => ticks go LEFT  (-tick)
      x_br   <- x0
      x_tick <- if (position == "left") x_br + tick else x_br - tick
      
      seg_df <- tibble::tibble(
        x    = c(x_br, x_br, x_br),
        xend = c(x_tick, x_br, x_tick),
        y    = c(yb, yb, yt),
        yend = c(yb, yt, yt)
      ) %>%
        mutate(color = color, linewidth = linewidth, linetype = linetype)
      
      # Label placement:
      # - left bracket  => label to the LEFT
      # - right bracket => label to the RIGHT
      if (!is.null(text)) {
        if (is.null(label_xy)) {
          y_text <- (yb + yt) / 2
          vjust  <- 0.5
          
          if (position == "left") {
            x_text <- x_br - text_nudge
            hjust  <- 1
          } else {
            x_text <- x_br + text_nudge
            hjust  <- 0
          }
        } else {
          x_text <- label_xy$x
          y_text <- label_xy$y
          hjust  <- 0.5
          vjust  <- 0.5
        }
        
        lab_df <- tibble::tibble(
          x = x_text,
          y = y_text,
          label = text,
          hjust = hjust,
          vjust = vjust,
          color = color,
          size = text_size
        )
      }
    }
    
    if (orientation == "h") {
      stopifnot(!is.null(x0), !is.null(x1), !is.null(y0))
      
      xl <- min(x0, x1)
      xr <- max(x0, x1)
      yb <- y0
      
      # Bracket sits at yb; ticks point inward:
      # - position == top    => ticks go DOWN (-tick)
      # - position == bottom => ticks go UP   (+tick)
      y_tick <- if (position == "top") yb - tick else yb + tick
      
      seg_df <- tibble::tibble(
        x    = c(xl, xl, xr),
        xend = c(xl, xr, xr),
        y    = c(yb, yb, yb),
        yend = c(y_tick, yb, y_tick)
      ) %>%
        mutate(color = color, linewidth = linewidth, linetype = linetype)
      
      # Label placement:
      # - bottom bracket (ticks up) => label BELOW
      # - top bracket (ticks down)  => label ABOVE
      if (!is.null(text)) {
        if (is.null(label_xy)) {
          x_text <- (xl + xr) / 2
          hjust  <- 0.5
          
          if (position == "bottom") {
            y_text <- yb - text_nudge
            vjust  <- 1
          } else {
            y_text <- yb + text_nudge
            vjust  <- 0
          }
        } else {
          x_text <- label_xy$x
          y_text <- label_xy$y
          hjust  <- 0.5
          vjust  <- 0.5
        }
        
        lab_df <- tibble::tibble(
          x = x_text,
          y = y_text,
          label = text,
          hjust = hjust,
          vjust = vjust,
          color = color,
          size = text_size
        )
      }
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
  
  # segments
  p2 <- plt +
    geom_segment(
      data = segs,
      inherit.aes = FALSE,
      aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = segs$linewidth,
      linetype  = segs$linetype,
      color     = segs$color
    )
  
  # labels (optional)
  if (nrow(labs) > 0) {
    p2 <- p2 +
      geom_text(
        data = labs,
        inherit.aes = FALSE,
        aes(x = x, y = y, label = label),
        color = labs$color,
        size  = labs$size,
        hjust = labs$hjust,
        vjust = labs$vjust
      )
  }
  
  p2
}

# -----------------------------------------------------------------------
# “All comparisons that were originally there” (matching the example image)
# -----------------------------------------------------------------------

# Numeric x positions for discrete weeks (W1 = 1, W2 = 2)
x_W1 <- 1
x_W2 <- 2

# 1) Week 1: High vs Control (vertical bracket)
y_high_W1 <- pull_y("High", "W1", "y_mean")
y_ctrl_W1 <- pull_y("Control", "W1", "y_mean")

# 2) Week 1: Control vs Low (vertical bracket)
y_low_W1  <- pull_y("Low", "W1", "y_mean")

# 3) High group change W1->W2 (horizontal dashed bracket near top)
y_high_W2 <- pull_y("High", "W2", "y_mean")
y_high_top <- max(y_high_W1, y_high_W2) + 0.35  # put bracket above the points; tweak as needed

# 4) Low group change W1->W2 (horizontal dashed bracket near bottom)
y_low_W2 <- pull_y("Low", "W2", "y_mean")
y_low_bot <- min(y_low_W1, y_low_W2) - 0.35     # put bracket below the points; tweak as needed

bracket_specs <- list(
  # Week 1: High vs Control (bracket is on the LEFT of W1; ticks point inward to the RIGHT)
  list(
    orientation = "v",
    x0 = x_W1 - 0.22,          # bracket location left of W1
    y0 = y_ctrl_W1,
    y1 = y_high_W1,
    position = "left",
    tick = 0.10,
    pad = 0.05,
    text = "***  d = 1.65",
    color = "black",
    linewidth = 0.9
  ),
  
  # Week 1: Control vs Low (also on the LEFT)
  list(
    orientation = "v",
    x0 = x_W1 - 0.34,
    y0 = y_low_W1,
    y1 = y_ctrl_W1,
    position = "left",
    tick = 0.10,
    pad = 0.05,
    text = "***  d = -1.60",
    color = "black",
    linewidth = 0.9
  ),
  
  # High: W1 -> W2 (TOP dashed bracket; ticks point DOWN; label goes ABOVE)
  list(
    orientation = "h",
    x0 = x_W1, x1 = x_W2,
    y0 = y_high_top + .5,
    position = "top",
    tick = 0.10,
    text = "***  d = -0.76",
    color = "black",
    linewidth = 0.9,
    linetype = "dashed"
  ),
  
  # Low: W1 -> W2 (BOTTOM dashed bracket; ticks point UP; label goes BELOW)
  list(
    orientation = "h",
    x0 = x_W1, x1 = x_W2,
    y0 = y_low_bot - .5,
    position = "bottom",
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
