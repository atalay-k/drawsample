#'
#' Sample data with individual responses 
#'
#' A Function to sample data close to desired characteristics with individual responses.
#' 
#' @param dist           data frame:consists of id and scores with no missing
#' @param n              numeric: desired sample size
#' @param skew           numeric: the skewness value
#' @param kurts          numeric: the kurtosis value
#' @param replacement    logical:Sample with or without replacement?
#' (default is FALSE).
#' @param col_id        index of column ID's
#' @param col_total     index of column total score 
#' @param save.output logical: should the output be saved into a text file? (Default is FALSE).
#' @param output_name    character: a vector of two components.
#'                       The first component is the name of the output file,
#'                       user can change the second component.
#'
#'
#' @import dplyr
#' @import lattice
#' @import tibble
#' @importFrom psych describe
#' @importFrom grDevices dev.off
#' @importFrom graphics hist
#' @importFrom stats na.omit rnorm
#' @importFrom utils capture.output
#' @return This function returns a \code{list} including following:
#' \itemize{
#' \item a matrix: Descriptive statistics of the given data,
#'                 the reference vector and the sample.
#' \item a data frame: The id's and individual response of the sample.
#' \item graph: Histograms for the “data” and the “sample”
#' }
#' @details
#' The execution of the function may take some time since it tries to obtain
#' the specified value for skewness and kurtosis.
#' @references
#' Fleishman AI (1978). A Method for Simulating Non-normal Distributions.
#'  \emph{Psychometrika, 43, 521-532.} \doi{10.1007/BF02293811}.
#'
#' Fialkowski, A. C.  (2018). SimMultiCorrData: Simulation of Correlated Data
#' with Multiple #' Variable Types.  R package version 0.2.2. Retrieved from
#' https://cran.r-project.org/web/packages/SimMultiCorrData/index.html
#' 
#' Atalay Kabasakal, K. & Gunduz, T. (2020). Drawing a Sample with Desired Properties from 
#' Population in R Package “drawsample”.\emph{Journal of Measurement and Evaluation in Education 
#' and Psychology,11}(4),405-429. \doi{10.21031/epod.790449}
#' @export
#' @examples
#' # Example data provided with package
#' data(likert_example)
#' # First 6 rows of the example_data
#' head(likert_example)
#' # Draw a sample based on total(from flattened to normal)
#' output3 <- draw_sample_ir(dist=likert_example,n=200,skew = 1,kurts = 1.2,
#' col_id=1,col_total=7,save.output = FALSE) # Histogram of the reference data set
#' # descriptive statistics of the given data,reference data, and drawn sample
#' output3$desc
#' # First 6 rows of the drawn sample
#' head(output3$sample)
#' # Histogram of the given data set and drawn sample
#' output3$graph
#'\dontrun{
#' # Draw a sample based on total(from flattened to normal)
#' draw_sample_ir(dist=likert_example,n=200,skew = 0.5,kurts =0.5,
#' col_id=1,col_total=7,save.output = TRUE,
#' output_name = c("sample", "3"))
#'}
draw_sample_ir <-  function(dist,n,skew,kurts,
                         replacement =FALSE,
                         col_id=1,col_total=numeric(),
                         save.output = FALSE,
                         output_name = c("sample","1")){

  
dist_v2 <- data.frame(dist[,col_id],dist[,ncol(dist)])

  
  
output <- draw_sample(dist = dist_v2,n=n,skew = skew,
            kurts = kurts,
            output_name = output_name)

names(dist)[col_id] <- "id"


output$sample <- dist %>%  filter(id %in%  output$sample$id)



return(output)

}


