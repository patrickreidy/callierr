

#' Create a ggplot Color Scale Using Callier Center Colors
#'
#' \code{scale_color_callier()} creates a discrete color scale using Callier
#' Center colors, which can be added to a \code{ggplot} object.
#'
#' @param scheme A character vector that must be matched by
#'   \code{'qual'} (qualitative palette), \code{'seq'} (sequential palette), or
#'   \code{'div'} (diverging palette).
#' @param steps An integer, the number of colors in the scale.
#' @param ... optional arguments passed to \code{CallierSequential()},
#'   \code{CallierDiverging()}, or \code{scale_color_manual()}.
#' @seealso \code{\link{CallierQualitative}}, \code{\link{CallierSequential}},
#'   and \code{\link{CallierDiverging}} for the color palette constructors;
#'   \code{\link{scale_color_manual}} and \code{\link{discrete_scale}} for the
#'   ggplot scale constructors.
#' @export
scale_color_callier <- function(scheme, steps, ...) {
  .other_args <- list(...)
  if (length(grep(pattern = '^qual', x = tolower(scheme))) == 1) {
    # Qualitative scale.
    .values <- CallierQualitative(steps = steps)
  } else if (length(grep(pattern = '^seq', x = tolower(scheme))) == 1) {
    # Sequential scale.
    .args <- list('steps' = steps)
    if ('hue' %in% names(.other_args)) {
      .args <- c(.args, list('hue' = .other_args[['hue']]))
    }
    if ('lightest' %in% names(.other_args)) {
      .args <- c(.args, list('lightest' = .other_args[['lightest']]))
    }
    if ('direction' %in% names(.other_args)) {
      .args <- c(.args, list('direction' = .other_args[['direction']]))
    }
    .values <- do.call(what = CallierSequential, args = .args)
  } else if (length(grep(pattern = '^div', x = tolower(scheme))) == 1) {
    # Diverging scale.
    .args <- list('steps' = steps)
    if ('lower' %in% names(.other_args)) {
      .args <- c(.args, list('lower' = .other_args[['lower']]))
    }
    if ('lightest' %in% names(.other_args)) {
      .args <- c(.args, list('lightest' = .other_args[['lightest']]))
    }
    .values <- do.call(what = CallierDiverging, args = .args)
  }
  .scale_args <- setdiff(names(.other_args), c('hue', 'lower', 'lightest', 'direction'))
  if (length(.scale_args) > 0) {
    .scale_args <- .other_args[.scale_args]
  } else {
    .scale_args <- NULL
  }
  .arg_list <- c(list('values' = .values), .scale_args)
  do.call(what = ggplot2::scale_color_manual, args = .arg_list)
}
