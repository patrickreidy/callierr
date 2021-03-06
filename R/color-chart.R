#' Print a Color Chart for a Palette
#'
#' \code{ColorChart()} creates a visual reference of a color palette as it
#' would appear to trichromat viewers and to viewers with three types of
#' color blindness:
#' deuteranopia (red-green color blindness; 1\% of males),
#' protanopia (red-green color blindness; 1\% of males), and
#' tritanopia (blue-yellow color blindness; < 1\% of males and females).
#'
#' If \code{palette} is \code{NULL}, then a color chart for the six Callier
#' Center colors (solar orange, space blue, callier gray, spark orange,
#' stratos blue, sky blue) is created, and the x-axis is labeled with the
#' color names rather than their hexadecimal codes.
#'
#' The \code{dichromat} package is used to simulate color blindness.
#'
#' @param palette \code{NULL} (default) or a character vector, whose entries
#'   are hexadecimal codes for values in the RGB (red-green-blue) color model.
#' @param colorblind Logical that determines whether simulations of
#'   color blindness (deuteranopia, protanopia, tritanopia) are shown
#'   (\code{TRUE}) or not (\code{FALSE)}) along with the trichromat color
#'   palette. Default is \code{TRUE}.
#' @param chipsize Numeric that is passed to \code{geom_point(size = )}.
#'   Determines the size of the color chips in the chart. Default is 25.
#' @return A \code{ggplot} object with the following aesthetics:
#'   colors are ordered as they occur in \code{palette} and mapped to the
#'   x-dimension, with the x-axis labeled by hexadecimal codes;
#'   vision is mapped to the y-dimension, with the y-axis labeled by type of
#'   vision (trichromat, deuteranopia, protanopia, tritanopia).
#' @export
ColorChart <- function(palette = NULL, colorblind = TRUE, chipsize = 25) {
  if (is.null(palette)) {
    palette <- c(
      'solarOrange'   = solarOrange(),
      'spaceBlue'     = spaceBlue(),
      'callierGray'   = callierGray(),
      'sparkOrange'   = sparkOrange(),
      'stratosBlue'   = stratosBlue(),
      'skyBlue'       = skyBlue(),
      'ecoGreen'      = ecoGreen(),
      'saplingGreen'  = saplingGreen(),
      'seedlingGreen' = seedlingGreen()
    )
  }
  if (is.null(names(palette))) {
    names(palette) <- palette
  }
  # Rectangularize the information in `palette`.
  .callier <- data.frame(
    Vision = 'Trichromat',
    Color  = names(palette),
    Hex    = palette,
    stringsAsFactors = FALSE
  )
  if (colorblind) {
    # Map the `.callier` scheme to different forms of dichromatism.
    .schemes <- rbind(
      .callier,
      within(.callier, {
        Vision <- 'Deuteranopia'
        Hex    <- dichromat::dichromat(Hex, type = 'deutan')
      }),
      within(.callier, {
        Vision <- 'Protanopia'
        Hex    <- dichromat::dichromat(Hex, type = 'protan')
      }),
      within(.callier, {
        Vision <- 'Tritanopia'
        Hex    <- dichromat::dichromat(Hex, type = 'tritan')
      })
    )
    # Factor the variables in `.schemes`.
    .schemes <- within(.schemes, {
      Vision <- factor(
        Vision,
        levels = c('Tritanopia', 'Protanopia', 'Deuteranopia', 'Trichromat')
      )
      Color  <- factor(Color, names(palette))
    })
    # Partition `.schemes` by level of $Vision.
    .trichrom <- subset(.schemes, as.character(Vision) == 'Trichromat')
    .deuteran <- subset(.schemes, as.character(Vision) == 'Deuteranopia')
    .protan   <- subset(.schemes, as.character(Vision) == 'Protanopia')
    .tritan   <- subset(.schemes, as.character(Vision) == 'Tritanopia')
    # Plot.
    .x <- ggplot2::ggplot(data = .schemes, ggplot2::aes(x = Color, y = Vision)) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(data = .schemes, colour = 'white', size = 1) +
      ggplot2::geom_point(data = .trichrom, colour = .trichrom$Hex, size = chipsize) +
      ggplot2::geom_point(data = .deuteran, colour = .deuteran$Hex, size = chipsize) +
      ggplot2::geom_point(data = .protan, colour = .protan$Hex, size = chipsize) +
      ggplot2::geom_point(data = .tritan, colour = .tritan$Hex, size = chipsize)
  } else {
    .callier <- within(.callier, {
      Vision <- ''
      Color <- factor(Color, names(palette))
    })
    .x <- ggplot2::ggplot(data = .callier, ggplot2::aes(x = Color, y = Vision)) +
      ggplot2::theme_bw() +
      ggplot2::ylab('') +
      ggplot2::scale_y_discrete(breaks = NULL) +
      ggplot2::geom_point(data = .callier, colour = .callier$Hex, size = chipsize)
  }
  return(.x)
}

