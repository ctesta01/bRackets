#' Build bracket geometry (segments + optional label) for ggplot2 annotations
#'
#' Construct the data frames needed to draw an annotated bracket in ggplot2.
#' The returned object contains (i) a segment data frame suitable for
#' \code{\link[ggplot2]{geom_segment}} and (ii) an optional label data frame
#' suitable for \code{\link[ggplot2]{geom_text}}.
#'
#' Brackets can be specified at a high level using \code{orientation}, \code{x0/x1},
#' \code{y0/y1}, and \code{position}, or at a low level by supplying explicit
#' \code{segments}. When \code{segments} is provided, bracket geometry is used
#' as-is and label placement cannot be inferred.
#'
#' For faceting, provide \code{facet_vars} (a named list) containing the facetting
#' variables and their values; these columns are appended to the returned segment
#' and label data so the annotation appears only in matching panels.
#'
#' @param orientation Bracket orientation. \code{"v"} draws a vertical bracket at
#'   a fixed x-position spanning \code{y0} to \code{y1}. \code{"h"} draws a horizontal
#'   bracket at a fixed y-position spanning \code{x0} to \code{x1}.
#' @param x0,x1 Bracket x-coordinates. For \code{orientation = "v"}, only \code{x0}
#'   is used (the bracket x-position). For \code{orientation = "h"}, \code{x0} and
#'   \code{x1} define the bracket endpoints.
#' @param y0,y1 Bracket y-coordinates. For \code{orientation = "v"}, \code{y0} and
#'   \code{y1} define the bracket endpoints. For \code{orientation = "h"}, only
#'   \code{y0} is used (the bracket y-position) and \code{y1} is ignored.
#' @param position Bracket placement. For \code{orientation = "v"}, one of
#'   \code{"left"} or \code{"right"} indicating where the bracket sits; ticks point
#'   inward (toward the plotted region). For \code{orientation = "h"}, one of
#'   \code{"top"} or \code{"bottom"}; ticks point inward.
#'   If \code{NULL}, defaults to \code{"left"} for vertical and \code{"top"} for
#'   horizontal.
#' @param side Deprecated alias for \code{position}. For \code{orientation = "v"},
#'   \code{side} must be \code{"left"} or \code{"right"}. For \code{orientation = "h"},
#'   \code{side} must be \code{"up"} or \code{"down"} and is mapped to
#'   \code{position = "bottom"} or \code{position = "top"}, respectively.
#' @param tick Tick length in data units. For vertical brackets, tick length is along
#'   the x-axis. For horizontal brackets, tick length is along the y-axis.
#' @param pad Padding in data units added to the bracket span. For vertical brackets,
#'   expands the y-span; for horizontal brackets, expands neither x-span nor y-span
#'   (it is applied only in the vertical case in the current implementation).
#' @param text Optional label to display near the bracket. If \code{NULL}, no label
#'   data is returned.
#' @param text_nudge Label offset in data units from the bracket. If \code{label_xy}
#'   is \code{NULL}, \code{text_nudge} determines the default text position relative
#'   to the bracket depending on \code{orientation} and \code{position}.
#' @param text_size Label size (passed through as a numeric column named \code{size}
#'   for downstream use by \code{\link[ggplot2]{geom_text}}).
#' @param color Line and label color.
#' @param linewidth Line width (passed through as a numeric column named \code{linewidth}).
#' @param linetype Line type (e.g., \code{"solid"}, \code{"dashed"}).
#' @param segments Optional low-level override for bracket geometry. A data frame with
#'   columns \code{x}, \code{xend}, \code{y}, \code{yend} (one row per segment).
#'   If provided, the function will not compute bracket geometry from
#'   \code{orientation/x0/x1/y0/y1/position}.
#' @param label_xy Optional low-level override for label placement. A list-like object
#'   with elements \code{x} and \code{y}. Required if \code{segments} is provided and
#'   \code{text} is not \code{NULL}.
#' @param facet_vars Optional named list of facetting variables and values to append
#'   to the returned segment and label data. Useful for \code{\link[ggplot2]{facet_wrap}}
#'   or \code{\link[ggplot2]{facet_grid}} so brackets appear only in the intended panels.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{segments}{A tibble with columns \code{x}, \code{xend}, \code{y}, \code{yend}
#'     and styling columns \code{color}, \code{linewidth}, \code{linetype}. If
#'     \code{facet_vars} is provided, those columns are included as well.}
#'   \item{labels}{Either \code{NULL} (if \code{text} is \code{NULL}) or a tibble
#'     with columns \code{x}, \code{y}, \code{label}, \code{hjust}, \code{vjust},
#'     \code{color}, \code{size}, and any facet columns from \code{facet_vars}.}
#' }
#'
#' @importFrom dplyr mutate bind_cols
#' @importFrom tibble tibble as_tibble
#'
#' @examples
#' # A vertical bracket with a label, intended for a single facet panel
#' b <- build_bracket(
#'   orientation = "v",
#'   x0 = 0.8, y0 = 1.0, y1 = 2.0,
#'   position = "left",
#'   text = "***",
#'   facet_vars = list(group = "High")
#' )
#'
#' # Use the returned data frames in ggplot2 layers:
#' # ggplot(...) +
#' #   geom_segment(data = b$segments, aes(x=x, xend=xend, y=y, yend=yend),
#' #                inherit.aes = FALSE) +
#' #   geom_text(data = b$labels, aes(x=x, y=y, label=label),
#' #             inherit.aes = FALSE)
#'
#' @export
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
  label_xy = NULL,
  facet_vars = NULL
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
    seg_df <- segments |>
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
      ) |>
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
      ) |>
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

  # append faceting vars
  if (! is.null(facet_vars)) {
    facet_df <- tibble::as_tibble(facet_vars)
    seg_df <- dplyr::bind_cols(seg_df,
                               facet_df[rep(1, nrow(seg_df)), , drop = FALSE])
    if (!is.null(lab_df)) {
      lab_df <- dplyr::bind_cols(lab_df,
                                 facet_df[rep(1, nrow(lab_df)), , drop = FALSE])
    }
  }

  list(segments = seg_df, labels = lab_df)
}
