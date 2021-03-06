% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{CallierQualitative}
\alias{CallierQualitative}
\title{Create Qualitative Color Palettes with Callier Center Colors}
\usage{
CallierQualitative(steps = 3)
}
\arguments{
\item{steps}{An integer or character vector. Default is 3.}
}
\value{
A character vector of hexadecimal codes for the Callier Center colors.
  If \code{steps} is greater than the number of Callier Center colors (9),
  then \code{NULL} is returned; hence, this palette cannot be used for
  qualitative data with more than 9 levels.
}
\description{
\code{CallierQualitative()} creates a qualitative color palette based on
the primary, secondary, and tertiary Callier Center colors, in the following
order of precedence:
solar orange, space blue, callier gray,
spark orange, stratos blue, sky blue,
eco green, sapling green, and seedling green.
}
\details{
A qualitative color palette does not imply magnitude differences between
levels in the data. As such, a qualitative palette is best suited to
representing nominal or categorical data.

A qualitative color palette is most effective when hues are used to signal
visual differences between levels in the data; hence, a palette that
uses only the three primary Callier Center colors is thus quite effective.
But the effectiveness of the Callier Center colors as a qualitative palette
diminishes when the secondary Callier Center colors are used: the secondary
colors all repeat hues (orange or blue) that are already represented in the
primary Callier Center colors.

If \code{steps} is a character vector, its entries should correspond
exhaustively to the levels of the variable that is being mapped onto the
color palette. The number of colors in the returned palette is equal to the
length of \code{steps}. The mapping from \code{steps} onto Callier Center
colors preserves the order of elements in \code{steps} and the order of
precedence in the Callier Center colors: i.e., the first element of
\code{steps} is mapped to \code{solarOrange}; the second element, to
\code{spaceBlue}; and so on.

If \code{steps} is an integer, then it controls only the number of
Callier Center colors in the returned palette. The mapping from the levels
of the data variable onto Callier Center colors is inherited from however
the data levels are ordered: If the variable is an ordered factor, then the
data levels are mapped in that order. If the variable is unordered, then
the data levels are mapped alphabetically.
}
\seealso{
\code{\link{CallierSequential}}, \code{\link{CallierDiverging}}
}

