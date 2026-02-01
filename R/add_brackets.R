#' Add one or more brackets to a ggplot
#'
#' Adds bracket annotations to an existing ggplot object using a list of bracket
#' specifications. Each specification is passed to \code{\link{build_bracket}}
#' (via \code{do.call()}) to produce segment and label data frames, which are then
#' combined and added to the plot as \code{\link[ggplot2]{geom_segment}} and
#' (optionally) \code{\link[ggplot2]{geom_text}} layers.
#'
#' For facet-specific brackets, include facetting variables in each spec via
#' \code{facet_vars} (see \code{\link{build_bracket}}). ggplot2 will place each row
#' of the annotation data into matching panels.
#'
#' @param plt A ggplot object to annotate.
#' @param bracket_specs A list of bracket specification lists. Each element should be
#'   a named list of arguments accepted by \code{\link{build_bracket}} (e.g.,
#'   \code{list(orientation="v", x0=..., y0=..., y1=..., text="***")}).
#'
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 geom_segment geom_text
#'
#' @details
#' This function currently passes vector-valued aesthetics (e.g., \code{segs$color})
#' directly as geom parameters. In ggplot2, scalar parameters are recycled and are
#' not intended for per-row styling. If you want per-row aesthetics, map \code{color},
#' \code{linewidth}, and \code{linetype} inside \code{aes()} and use identity scales
#' (e.g., \code{scale_colour_identity()}, \code{scale_linewidth_identity()},
#' \code{scale_linetype_identity()}).
#'
#' @return A ggplot object: \code{plt} with added bracket layers.
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = c(1, 2), y = c(0, 1))
#' p <- ggplot(df, aes(x, y)) + geom_point()
#'
#' specs <- list(
#'   list(
#'     orientation = "h",
#'     x0 = 1, x1 = 2, y0 = 1.2,
#'     position = "top",
#'     text = "*** significant!",
#'     linetype = "dashed"
#'   )
#' )
#'
#' p2 <- add_brackets(p, specs)
#'
#' @export
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
