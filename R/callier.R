
#' callier: Color Palettes and Scales for the Callier Center
#'
#' An implementation of the color schemes set out by the Graphic Standards for
#' the Callier Center for Communication Disorders.
#'
#' @section License:
#' Do not distrubte outside of faculty and staff of the Callier Center
#' for Communication Disorders.
#'
#' @section Constants:
#'   \itemize{
#'     \item \code{\link{solarOrange}()}
#'     \item \code{\link{spaceBlue}()}
#'     \item \code{\link{callierGray}()}
#'     \item \code{\link{sparkOrange}()}
#'     \item \code{\link{stratosBlue}()}
#'     \item \code{\link{skyBlue}()}
#'     \item \code{\link{ecoGreen}()}
#'     \item \code{\link{saplingGreen}()}
#'     \item \code{\link{seedlingGreen}()}
#'   }
#'
#' @section Color-model converters (internal):
#'   \itemize{
#'     \item \code{\link{as.hex}()}: convert RGB or HSL to HEX
#'     \item \code{\link{as.hsl}()}: convert HEX or RGB to HSL
#'     \item \code{\link{as.rgb}()}: convert HSL or HEX to RGB
#'   }
#'
#' @section Palette constructors:
#'   \itemize{
#'     \item \code{\link{CallierQualitative}()}: create qualitative color palettes with Callier Center colors
#'     \item \code{\link{CallierSequential}()}: create sequential color palettes with Callier Center colors
#'     \item \code{\link{CallierDiverging}()}: create diverging color palettes with Callier Center colors
#'   }
#'
#' @section Scale constructors (for ggplot objects):
#' scale_color_callier()
#'
#' @section Other functions:
#' ColorChart()
#'
#' @docType package
#' @name callier
NULL

