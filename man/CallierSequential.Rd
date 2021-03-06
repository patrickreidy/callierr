% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{CallierSequential}
\alias{CallierSequential}
\title{Create Sequential Color Palettes with Callier Center Colors}
\usage{
CallierSequential(steps = 3, hue = "orange", lightest = 0.85,
  direction = "increasing")
}
\arguments{
\item{steps}{An integer or character vector. Default is 3.
If \code{steps} is an integer, it only controls the number of tints in
the palette.
If \code{steps} is a character vector, it should correspond
exhaustively to the levels of the variable that is mapped onto the
palette.}

\item{hue}{A character string that is matched by either \code{'or'} (orange),
\code{'bl'} (blue), or \code{'gr'} (gray). Default is \code{'orange'}.}

\item{lightest}{A numeric between 0 and 1 that denotes the lightness of the
lightest tint in the palatte. Values closer to 0 are darker; those
closer to 1 are lighter. Default is 0.85.}

\item{direction}{A character string that is matched by either
\code{'inc'} (increasing) or \code{'dec'} (decreasing). Default is
\code{'increasing'}.}
}
\value{
A character vector of hexadecimal codes for colors in the
  RGB (red-green-blue) color model. The first hexadecimal code denotes the
  lightest tint in the palette; the last hexadecimal code denotes the
  darkest.
}
\description{
\code{CallierSequential()} creates a sequential color palette of tints of
one of the primary Callier Center colors (solar orange, space blue, or
callier gray).
}
\details{
A sequential color palette is well suited to ordered data whose values
progress from low to high. The look of a sequential palette is dominated
by steps that share a common hue and saturation, but differ in terms of
lightness. Hence, a sequential palette is a sequence of tints that progress
from lightest to darkest, with light colors corresponding to low data values
and dark colors corresponding to high data values.

The primary Callier Center color whose hue matches the \code{hue}
argument anchors the palette and is the darkest tint in the palette.
The other tints in the palette share the same hue and saturation as this
anchor color, but differ in terms of lightness, in the HSL
(hue-saturation-lightness) color model.

The lightness of the lightest tint in the palette is equal to the
\code{lightest} argument. The tints that fall between the lightest tint and
the anchor are determined by linearly interpolating values between
\code{lightest} and the lightness-value of the anchor color.

If \code{direction} is matched by \code{'inc'} (increasing), then the
sequential palette progresses from lighter to darker tints.
If \code{direction} is matched by \code{'dec'} (decreasing), then the
sequential palette progresses from darker to lighter tints.
}
\seealso{
\code{\link{CallierQualitative}}, \code{\link{CallierDiverging}}
}

