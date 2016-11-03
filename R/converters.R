

#' Convert RGB or HSL to HEX
#'
#' \code{as.hex()} converts a value in the RGB (red-green-blue) or
#' HSL (hue-saturation-lightness) color model to a hexadecimal code that can
#' be reliably recognized by various plotting functions
#' (e.g., base graphics or ggplot).
#'
#' If \code{x} is an integer vector, then it is assumed that \code{x} denotes
#' a value in the RGB color model---i.e., that the elements of \code{x} are
#' integers in between 0 and 255 that specify the values of the red, green,
#' and blue beams, respectively.
#'
#' If \code{x} is a numeric vector (but not an integer vector), then it is
#' assumed that \code{x} denotes a value in the HSL color model---i.e.,
#' that the first element of \code{x} is an whole number between 0 and 359,
#' denoting the angle of the hue on the color wheel;
#' that the second element is a floating point number (to 2 decimal places)
#' between 0 and 1, denoting the saturation proportion;
#' that the third element is a floating point number (to 2 decimal places)
#' between 0 and 1, denoting the lightness proportion.
#'
#' \code{as.hex()} is not vectorized; hence, \code{x} should denote a single
#' value in either the RGB or HSL color models.
#'
#' @param x An integer or numeric vector with 3 elements.
#'   An integer vector is assumed to be a value in the RGB color model;
#'   a numeric vector is assumed to be a value in the HSL color model.
#' @return A character string, the hexadecimal code for a value in the RGB
#'   color model. A hexadecimal code has the form: \code{'#RRGGBB'}, where the
#'   substrings \code{'RR'}, \code{'GG'}, and \code{'BB'} denote the values of
#'   the red, green, and blue beams, respectively, in the RGB color model.
as.hex <- function(x) {
  if (is.integer(x)) {   ### RGB to HEX
    .hex <- toupper(sprintf('%02s', as.hexmode(x)))
    .hex <- sprintf('#%s', paste(.hex, collapse = ''))
  }
  if (! is.integer(x) & is.numeric(x)) {   ### HSL to HEX
    .hex <- as.hex(as.rgb(x))
  }
  return(.hex)
}


#' Convert HEX or RGB to HSL
#'
#' \code{as.hsl()} converts a value in the RGB (red-green-blue) color model to
#' one in the HSL (hue-saturation-lightness) color model. The value in the RGB
#' color model may be specified either as a hexadecimal code or as integer
#' levels of the red, green, and blue beams.
#'
#' If \code{x} is a character string, then it is assumed that \code{x} is
#' a hexadecimal code (\code{'#RRGGBB'}) whose substrings denote the the values
#' of the red (\code{'RR'}), green (\code{'GG'}), and blue (\code{'BB'}) beams.
#'
#' If \code{x} is an integer vector, then it is assumed that \code{x} denotes
#' the integer values of the red, green, and blue beams for the color.
#'
#' @param x A character string or an integer vector with 3 elements.
#'   A character string is assumed to be a hexadecimal code;
#'   an integer vector is assumed to be a value in the RGB color model, whose
#'   elements are integers between 0 and 255
#' @return A numeric vector with three elements.
#'   The first element denotes the color's hue and is represented as a degree
#'   on the color wheel (i.e., a whole number between 0 and 359).
#'   The second element denotes the color's (proportion of) saturation to 2
#'   decimal places (i.e., a number between 0 and 1.
#'   The third element denotes the color's (proportion of) lightness to 2
#'   decimal places (i.e., a number between 0 and 1).
as.hsl <- function(x) {
  if (is.character(x)) {   ### HEX to HSL
    .hsl <- as.hsl(as.rgb(x))
  }
  if (is.integer(x)) {   ### RGB to HSL
    # Parse `x` into RGB components.
    .r <- x['r'] / 255
    .g <- x['g'] / 255
    .b <- x['b'] / 255
    # Find min() and max() components of `x`.
    .min <- min(x) / 255
    .max <- max(x) / 255
    # Compute luminescence `.L`.
    .L <- (.min + .max) / 2
    .L <- round(.L, digits = 2)
    if (.min == .max) {
      # If min() and max() of `x` are the same, then `x` denotes a shade of grey.
      # Hence, set saturation `.S` and hue `.H` to 0s.
      .S <- 0.00
      .H <- 0
    } else {
      # Otherwise, `x` doesn't denote a shade of grey.
      # Compute saturation `.S`.
      if (.L < 0.5) {
        .S <- (.max - .min) / (.max + .min)
      } else {
        .S <- (.max - .min) / (2.0 - .max - .min)
      }
      .S <- round(.S, digits = 2)
      # Compute hue `.H`.
      if (.max == .r) {
        .H <- ((.g - .b) / (.max - .min)) + ifelse(.g < .b, 6, 0)
      } else if (.max == .g) {
        .H <- ((.b - .r) / (.max - .min)) + 2
      } else if (.max == .b) {
        .H <- ((.r - .g) / (.max - .min)) + 4
      }
      .H <- round(.H * 60, digits = 0)
    }
    .hsl <- c(.H, .S, .L)
  }
  names(.hsl) <- c('h', 's', 'l')
  return(.hsl)
}


#' Convert HSL or HEX to RGB
#'
#' \code{as.rgb()} converts a value in the HSL (hue-saturation-lightness)
#' color model or a hexadecimal code for a value in the RGB (red-green-blue)
#' color model to a value in the RGB color model.
#'
#' If \code{x} is a character string, then it is assumed that \code{x} is
#' a hexadecimal code (\code{'#RRGGBB'}) whose substrings denote the the values
#' of the red (\code{'RR'}), green (\code{'GG'}), and blue (\code{'BB'}) beams.
#'
#' If \code{x} is a numeric vector (but not an integer vector), then it is
#' assumed that \code{x} denotes a value in the HSL color model---i.e.,
#' that the first element of \code{x} is an whole number between 0 and 359,
#' denoting the angle of the hue on the color wheel;
#' that the second element is a floating point number (to 2 decimal places)
#' between 0 and 1, denoting the saturation proportion;
#' that the third element is a floating point number (to 2 decimal places)
#' between 0 and 1, denoting the lightness proportion.
#'
#' @param x A character string or a numeric vector with 3 elements.
#'   A character string is assumed to be the hexadecimal code for a value in
#'   the RGB color model;
#'   a numeric vector is assumed to be a value in the HSL color model.
#' @return An integer vector with three elements (valued between 0 and 255) that
#'   denote the values of the red, green, and blue beams, respectively.
as.rgb <- function(x) {
  if (is.character(x)) {   ### HEX to RGB
    .hex <- sub(pattern = '#', replacement = '', x = x)
    .rgb <- c(
      'r' = strtoi(x = substring(.hex, 1, 2), base = 16),
      'g' = strtoi(x = substring(.hex, 3, 4), base = 16),
      'b' = strtoi(x = substring(.hex, 5, 6), base = 16)
    )
  }
  if (is.numeric(x)) {   ### HSL to RGB
    # Parse `x` into HSL components.
    .h <- x['h']
    .s <- x['s']
    .l <- x['l']
    # If there is no saturation, then `x` denotes a shade of grey; hence, the
    # red, green, and blue beams will each be proportional to the lightness.
    if (.s == 0) {
      .r <- .g <- .b <- as.integer(round(.s * 255))
    } else {
      .beam <- function(.p, .q, .t) {
        if (.t < 0) .t <- .t + 1
        if (.t > 1) .t <- .t - 1
        if (.t < 1/6) {
          .v <- .p + (.q - .p) * 6 * .t
        } else if (.t < 1/2) {
          .v <- .q
        } else if (.t < 2/3) {
          .v <- .p + (.q - .p) * (2/3 - .t) * 6
        } else {
          .v <- .p
        }
        .v <- as.integer(round(.v * 255))
        return(.v)
      }
      .q <- ifelse(.l < 0.5, .l * (1 + .s), .l + .s - .l * .s)
      .p <- 2 * .l - .q
      .r <- .beam(.p, .q, .h/360 + 1/3)
      .g <- .beam(.p, .q, .h/360)
      .b <- .beam(.p, .q, .h/360 - 1/3)
    }
    .rgb <- c(.r, .g, .b)
  }
  # Coerce to an integer-vector; add names.
  .rgb <- as.integer(.rgb)
  names(.rgb) <- c('r', 'g', 'b')
  return(.rgb)
}

