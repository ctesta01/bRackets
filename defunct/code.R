# Goal: 
# Use R (ggplot2) to add annotations to the error bars in a ggplot2 
# so that the user doesn't have to annotate (as much or if at all) 
# in Inkscape 


# dependencies ------------------------------------------------------------

library(tidyverse)



# load data ---------------------------------------------------------------


df <- readr::read_csv("data_annotated.csv")



# format for ggplot2 ------------------------------------------------------

df_err <- df %>%
  group_by(group, week) %>%
  summarise(
    y_mean = mean(y),
    ymin   = min(y),
    ymax   = max(y),
    .groups = "drop"
  )

# graphic without annotations ---------------------------------------------

plt <- ggplot(df, aes(x = week, y = y, color = group, group = group)) +
  geom_point(
    data = df |> filter(is.na(error_type)),
    size = 2) +
  geom_errorbar(
    data = df_err,
    aes(ymin = ymin, ymax = ymax, y = NULL),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_line(
    data = df_err,
    aes(y = y_mean),
    linewidth = 0.8
  ) +
  theme_minimal() + 
  xlab("Week Number") + 
  ylab("log-transform") + 
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5))

plt

# create one annotation ----------------------------------------------------

# pick some groups to compare

# ---- pick the two groups to compare at Week 1 ----
g_lo <- "Control"
g_hi <- "High"

y_lo <- df_err %>% filter(week == "W1", group == g_control) %>% pull(y_mean)
y_hi <- df_err %>% filter(week == "W1", group == g_hi) %>% pull(y_mean)

# ---- bracket geometry (x positions are numeric for easy nudging) ----
x_w1    <- 1
x_br    <- x_w1 - 0.25   # where the bracket sits (left of W1)
tick_w  <- 0.10          # tick length
y_pad   <- 0.05          # tiny pad so the ticks don't collide with points

# Add three connected segments to make a bracket
plt +
  # bottom tick
  geom_segment(
    inherit.aes = FALSE,
    aes(x = x_br, xend = x_br + tick_w, y = y_lo - y_pad, yend = y_lo - y_pad),
    linewidth = 0.9,
    color = "black"
  ) +
  # vertical connector
  geom_segment(
    inherit.aes = FALSE,
    aes(x = x_br, xend = x_br, y = y_lo - y_pad, yend = y_hi + y_pad),
    linewidth = 0.9,
    color = "black"
  ) +
  # top tick
  geom_segment(
    inherit.aes = FALSE,
    aes(x = x_br, xend = x_br + tick_w, y = y_hi + y_pad, yend = y_hi + y_pad),
    linewidth = 0.9,
    color = "black"
  ) +
  # label (place it centered on the bracket)
  annotate(
    "text",
    x = x_br - tick_w - 0.15,
    y = (y_lo + y_hi) / 2,
    label = "d = 1.65",
    hjust = 0,
    size = 4
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 30))  # extra left margin for the bracket




# now let's try with a function -------------------------------------------


bracket_layers <- function(
    x,                 # x position (numeric) where bracket attaches (e.g., 1 for W1, 2 for W2)
    y_bottom,          # bottom y of bracket
    y_top,             # top y of bracket
    label,             # text to annotate, e.g. "d = 1.65"
    side = c("left", "right"),
    color = "black",
    linewidth = 0.9,
    tick_w = 0.10,     # tick length (in x units)
    x_offset = 0.25,   # how far bracket sits away from x
    y_pad = 0.00,      # extra padding added outside y_bottom/y_top (optional)
    text_nudge = 0.05, # how far text sits from the bracket
    text_size = 4,
    ...
) {
  side <- match.arg(side)
  
  # ensure ordering
  y0 <- min(y_bottom, y_top) - y_pad
  y1 <- max(y_bottom, y_top) + y_pad
  
  # bracket x geometry
  if (side == "left") {
    x_br   <- x - x_offset
    x_tick <- x_br + tick_w
    x_text <- x_br - text_nudge
    hjust  <- 1
  } else {
    x_br   <- x + x_offset
    x_tick <- x_br - tick_w
    x_text <- x_br + text_nudge
    hjust  <- 0
  }
  
  list(
    # bottom tick
    geom_segment(
      inherit.aes = FALSE,
      aes(x = x_br, xend = x_tick, y = y0, yend = y0),
      color = color, linewidth = linewidth
    ),
    # vertical connector
    geom_segment(
      inherit.aes = FALSE,
      aes(x = x_br, xend = x_br, y = y0, yend = y1),
      color = color, linewidth = linewidth
    ),
    # top tick
    geom_segment(
      inherit.aes = FALSE,
      aes(x = x_br, xend = x_tick, y = y1, yend = y1),
      color = color, linewidth = linewidth
    ),
    # label
    annotate(
      "text",
      x = x_text,
      y = (y0 + y1) / 2,
      label = label,
      hjust = hjust,
      size = text_size,
      color = color,
      ...
    )
  )
}

# recreating the above example 

g_lo <- "Control"
g_hi <- "High"

y_lo <- df_err %>% filter(week == "W1", group == g_lo) %>% pull(y_mean)
y_hi <- df_err %>% filter(week == "W1", group == g_hi) %>% pull(y_mean)

plt +
  bracket_layers(
    x = 1,                    # W1
    y_bottom = y_lo,
    y_top    = y_hi,
    label    = "d = 1.65",
    side     = "left",
    color    = "black",
    y_pad    = 0.05
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 30, 5.5, 30))



# another example but with my own specified color and positions 

plt +
  bracket_layers(
    x = 2, y_bottom = 4.9, y_top = 6.2,
    label = "p < 0.001",
    side = "right",
    color = "red"
  ) +
  coord_cartesian(clip = "off")

