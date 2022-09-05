#' @title Fleishman's Power Method Transformation Constants
#' @description This table includes Fleishman's Power Method
#' Transformation constants.
#' @format A \code{data.frame} with 5 columns, which are
#' \describe{
#'  \item{Skew}{The skewness value}
#'  \item{Kurtosis}{The standardized kurtosis value}
#'  \item{b}{Outcome that is based on \code{Skew,Kurtosis}}
#'  \item{c}{Outcome that is based on \code{Skew,Kurtosis}}
#'  \item{d}{Outcome that is based on \code{Skew,Kurtosis}}
#' }
#' @seealso \code{\link[SimMultiCorrData]{find_constants}}
#'
#' @references
#' Fleishman AI (1978). A Method for Simulating Non-normal Distributions.
#'  \emph{Psychometrika, 43, 521-532.} \doi{10.1007/BF02293811}.
#'
#' Fialkowski, A. C.  (2018). SimMultiCorrData: Simulation of Correlated
#' Data with Multiple Variable Types.  R package version 0.2.2. Retrieved
#' from
#' https://cran.r-project.org/web/packages/SimMultiCorrData/index.html
#' 
#' @examples
#'
#' # First 6 rows of the table
#' data(constants_table)
#' head(constants_table)
#'
"constants_table"
