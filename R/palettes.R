

#' Create Qualitative Color Palettes with Callier Center Colors
#'
#' \code{CallierQualitative()} creates a qualitative color palette based on
#' the primary, secondary, and tertiary Callier Center colors, in the following
#' order of precedence:
#' solar orange, space blue, callier gray,
#' spark orange, stratos blue, sky blue,
#' eco green, sapling green, and seedling green.
#'
#' A qualitative color palette does not imply magnitude differences between
#' levels in the data. As such, a qualitative palette is best suited to
#' representing nominal or categorical data.
#'
#' A qualitative color palette is most effective when hues are used to signal
#' visual differences between levels in the data; hence, a palette that
#' uses only the three primary Callier Center colors is thus quite effective.
#' But the effectiveness of the Callier Center colors as a qualitative palette
#' diminishes when the secondary Callier Center colors are used: the secondary
#' colors all repeat hues (orange or blue) that are already represented in the
#' primary Callier Center colors.
#'
#' If \code{steps} is a character vector, its entries should correspond
#' exhaustively to the levels of the variable that is being mapped onto the
#' color palette. The number of colors in the returned palette is equal to the
#' length of \code{steps}. The mapping from \code{steps} onto Callier Center
#' colors preserves the order of elements in \code{steps} and the order of
#' precedence in the Callier Center colors: i.e., the first element of
#' \code{steps} is mapped to \code{solarOrange}; the second element, to
#' \code{spaceBlue}; and so on.
#'
#' If \code{steps} is an integer, then it controls only the number of
#' Callier Center colors in the returned palette. The mapping from the levels
#' of the data variable onto Callier Center colors is inherited from however
#' the data levels are ordered: If the variable is an ordered factor, then the
#' data levels are mapped in that order. If the variable is unordered, then
#' the data levels are mapped alphabetically.
#'
#' @param steps An integer or character vector. Default is 3.
#' @return A character vector of hexadecimal codes for the Callier Center colors.
#'   If \code{steps} is greater than the number of Callier Center colors (9),
#'   then \code{NULL} is returned; hence, this palette cannot be used for
#'   qualitative data with more than 9 levels.
#' @seealso \code{\link{CallierSequential}}, \code{\link{CallierDiverging}}
#' @export
CallierQualitative <- function(steps = 3) {
  .callier <- c(solarOrange(),
                spaceBlue(),
                callierGray(),
                sparkOrange(),
                stratosBlue(),
                skyBlue(),
                ecoGreen(),
                saplingGreen(),
                seedlingGreen())
  if (is.numeric(steps)) {
    .palette <- .callier[1:steps]
  } else if (is.character(steps)) {
    .palette <- .callier[1:length(steps)]
    names(.palette) <- steps
  }
  return(.palette)
}


#' Create Sequential Color Palettes with Callier Center Colors
#'
#' \code{CallierSequential()} creates a sequential color palette of tints of
#' one of the primary Callier Center colors (solar orange, space blue, or
#' callier gray).
#'
#' A sequential color palette is well suited to ordered data whose values
#' progress from low to high. The look of a sequential palette is dominated
#' by steps that share a common hue and saturation, but differ in terms of
#' lightness. Hence, a sequential palette is a sequence of tints that progress
#' from lightest to darkest, with light colors corresponding to low data values
#' and dark colors corresponding to high data values.
#'
#' The primary Callier Center color whose hue matches the \code{hue}
#' argument anchors the palette and is the darkest tint in the palette.
#' The other tints in the palette share the same hue and saturation as this
#' anchor color, but differ in terms of lightness, in the HSL
#' (hue-saturation-lightness) color model.
#'
#' The lightness of the lightest tint in the palette is equal to the
#' \code{lightest} argument. The tints that fall between the lightest tint and
#' the anchor are determined by linearly interpolating values between
#' \code{lightest} and the lightness-value of the anchor color.
#'
#' If \code{direction} is matched by \code{'inc'} (increasing), then the
#' sequential palette progresses from lighter to darker tints.
#' If \code{direction} is matched by \code{'dec'} (decreasing), then the
#' sequential palette progresses from darker to lighter tints.
#'
#' @param steps An integer or character vector. Default is 3.
#'   If \code{steps} is an integer, it only controls the number of tints in
#'   the palette.
#'   If \code{steps} is a character vector, it should correspond
#'   exhaustively to the levels of the variable that is mapped onto the
#'   palette.
#' @param hue A character string that is matched by either \code{'or'} (orange),
#'   \code{'bl'} (blue), or \code{'gr'} (gray). Default is \code{'orange'}.
#' @param lightest A numeric between 0 and 1 that denotes the lightness of the
#'   lightest tint in the palatte. Values closer to 0 are darker; those
#'   closer to 1 are lighter. Default is 0.85.
#' @param direction A character string that is matched by either
#'   \code{'inc'} (increasing) or \code{'dec'} (decreasing). Default is
#'   \code{'increasing'}.
#' @return A character vector of hexadecimal codes for colors in the
#'   RGB (red-green-blue) color model. The first hexadecimal code denotes the
#'   lightest tint in the palette; the last hexadecimal code denotes the
#'   darkest.
#' @seealso \code{\link{CallierQualitative}}, \code{\link{CallierDiverging}}
#' @export
CallierSequential <- function(steps = 3, hue = 'orange', lightest = 0.85, direction = 'increasing') {
  if (length(grep(pattern = 'or', x = tolower(hue))) == 1) {
    .anchor = as.hsl(solarOrange())
  } else if (length(grep(pattern = 'bl', x = tolower(hue))) == 1) {
    .anchor = as.hsl(spaceBlue())
  } else if (length(grep(pattern = 'gr', x = tolower(hue))) == 1) {
    .anchor = as.hsl(callierGray())
  }
  .h <- .anchor['h']
  .s <- .anchor['s']
  .l <- .anchor['l']
  if (lightest > .l) {
    if (is.numeric(steps)) {
      .n <- steps
      .names <- NULL
    } else if (is.character(steps)) {
      .n <- length(steps)
      .names <- steps
    }
    .sequential <- character()
    for (.l_step in rev(seq(from = .l, to = lightest, length.out = .n))) {
      .hsl <- c('h' = .h, 's' = .s, 'l' = round(.l_step, digits = 2))
      names(.hsl) <- c('h', 's', 'l')
      .sequential <- c(.sequential, as.hex(x = .hsl))
    }
  } else {
    .sequential <- NULL
  }
  if (length(grep(pattern = 'dec', x = tolower(direction))) == 1) {
    .sequential <- rev(.sequential)
  }
  names(.sequential) <- .names
  return(.sequential)
}


#' Create Diverging Color Palettes with Callier Colors
#'
#' \code{CallierDiverging()} creates a diverging color palette. The values on
#' one half of the palette are tints of \code{solarOrange}; the values on
#' the other half of the palette are tints of \code{spaceBlue}. The endpoints
#' of the palette are \code{solarOrange} and \code{spaceBlue}. Colors closer
#' to the middle of the palette increase in lightness from the endpoint.
#'
#' A diverging color palette emphasizes: (1) critical values near the middle of
#' the data range and (2) extreme values at both ends of the data range. This
#' is accomplished by concatenating two sequential palettes of different hues;
#' hence, differences above or below the mid-range critical value are
#' represented by a change in hue and differences within either half of the
#' palette are represented by changes in lightness within a fixed hue.
#'
#' If the argument \code{lower} is matched by \code{'orange'}, then the lowest
#' value of the diverging palette is \code{solarOrange}, and the palette
#' progresses through lightening orange tints, at the low end, and through
#' darkening blue tints, at the high end. If \code{lower} is matched by
#' \code{'blue'}, then the lowest value of the palette is \code{spaceBlue}, and
#' the palette progresses through lightening blue tints, at the low end, and
#' through darkening orange tints, at the high end.
#'
#' Give some consideration to the type of data that are being represented on
#' the diverging palette and how the structure of that data relates to physical
#' properties of blue and orange hues. (Blue hues have a shorter wavelength
#' and a higher frequency than orange hues.) For example, if the data
#' represented on the diverging palette are (audible) frequencies, then a
#' palette with orange hues on the \code{lower} end will parallel the order of
#' the visible frequency scale. Conversely, if the data are distances, then a
#' palette with blue hues on the \code{lower} end will parallel the order of
#' the visible wavelength scale.
#'
#' If the argument \code{steps} is an odd integer, then the color in the middle
#' of the palette is the \code{lightest} tint of \code{callierGray}.
#'
#' @param steps An integer or list. Default is 3.
#'   If \code{steps} is an integer, it controls only the number of colors in
#'   the palette.
#'   If \code{steps} is a list, it should comprise character vectors that
#'   partition the variable mapped onto the color palette.
#'   Furthermore, the elments of the list should be named
#'   \code{'orange'}, \code{'blue'}, or \code{'gray'}.
#' @param lower A character string that must be matched by either \code{'orange'}
#'   or \code{'blue'}. Default is \code{'orange'}.
#' @param lightest A numeric between 0 and 1 that denotes the lightness of the
#'   lightest tint in the middle of the palatte. Values closer to 0 are darker;
#'   those closer to 1 are lighter. Default is 0.85.
#' @return A character vector of hexadecimal codes for colors in the
#'   RGB (red-green-blue) color model.
#' @seealso \code{\link{CallierQualitative}}, \code{\link{CallierSequential}}
#' @export
CallierDiverging <- function(steps = 3, lower = 'orange', lightest = 0.85) {
  .diverging <- NULL
  if (is.numeric(steps)) {
    # orange sequence is primary.
    .orange <- CallierSequential(steps = floor(steps/2),
                                 hue = 'orange',
                                 lightest = lightest)
    # blue sequence is secondary.
    .blue <- CallierSequential(steps = floor(steps/2),
                               hue = 'blue',
                               lightest = lightest)
    # gray is the middle step if `steps` is odd.
    if (steps %% 2) {
      .grays <- CallierSequential(steps = 2,
                                  hue = 'gray',
                                  lightest = lightest)
      if (steps > 3) {
        .gray <- .grays[1]
      } else {
        .gray <- .grays[2]
      }
    } else {
      .gray <- NULL
    }
    .diverging <- c(rev(.orange), .gray, .blue)
    if (length(grep(pattern = 'blue', x = tolower(lower))) == 1) {
      .diverging <- rev(.diverging)
    }
  } else if (is.list(steps)) {
    if (length(steps) < 2 |length(steps) > 3 | (length(steps) == 3 & length(steps[[2]]) != 1)) {
      .diverging <- NULL
    } else {
      .left <- CallierSequential(steps = steps[[1]],
                                 hue = names(steps)[1],
                                 direction = 'decreasing')
      .right <- CallierSequential(steps = steps[[length(steps)]],
                                  hue = names(steps)[length(steps)],
                                  direction = 'increasing')
      if (length(steps) == 3) {
        .middle <- CallierSequential(steps = steps[[2]],
                                     hue = names(steps)[2])
      } else {
        .middle <- NULL
      }
      .diverging <- c(.left, .middle, .right)
      names(.diverging) <- Reduce(c, steps)
    }
  }
  return(.diverging)
}


