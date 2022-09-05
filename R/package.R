#' @title  Draw Samples with the Desired Properties from a Data
#'
#'
#' @description \pkg{draw_sample}, functions take a sample of the specified
#' sample size,skewness, and kurtosis form a data set (dist)with or without
#' resampling.
#' Fleishman's power method (\doi{10.1007/BF02293811})  was used
#' for the desired skewness and kurtosis level.
#' Therefore, the coefficient of skewness can be chosen between 0 and 3.6.
#' Although the kurtosis coefficient varies for each skewness coefficient
#' and varies from -1.2 and 20.
#' If convenient kurtosis and skew values are not provided,
#' no solutions can be found and an error is given.
#'
#' @references
#' Fleishman AI (1978). A Method for Simulating Non-normal Distributions.
#'  \emph{Psychometrika, 43, 521-532.} \doi{10.1007/BF02293811}.
#'  
#' Atalay Kabasakal, K. & Gunduz, T . (2020). Drawing a Sample with Desired Properties from 
#' Population in R Package “drawsample”.\emph{Journal of Measurement and Evaluation in Education 
#' and Psychology,11}(4),405-429. \doi{10.21031/epod.790449}
#' @docType package
"_PACKAGE"
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
