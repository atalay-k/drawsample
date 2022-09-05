#'
#' Sample data close to desired characteristics - nearest
#'
#' A Function to sample data close to desired characteristics - nearest
#' 
#' @param dist         data frame:consists of id and scores with no missing
#' @param n            numeric: desired sample size
#' @param skew         numeric: the skewness value
#' @param kurts        numeric: the kurtosis value
#' @param location     numeric: the value for adjusting mean (default is 0).
#' @param delta_var    numeric: the value for adjusting variance (default is 0).
#' @param save.output logical: should the output be saved into a text file? (Default is FALSE).
#' @param output_name character: a vector of two components.
#'                    The first component is the name of the output file,
#'                    user can change the second component.
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
#' \item a data frame: The id's and scores of the sample
#' \item graph: Histograms for the “data” and the “sample”
#' }
#' @details
#' The desired skewness and kurtosis values cannot be met while the function
#' execution is faster. The attributes of kurtosis are in doubt.
#' This is because the range of kurtosis is greater than the skewness.
#' For \code{location} values can be entered to position the midpoint or mean of the
#' distribution differently. For \code{delta_var} the value can be entered for
#' how much will increase or decrease the variability of reference distribution.
#' In other words, the reference distribution is generated as the standard normal distribution,
#' unless the user changes the default values of the \code{location} and \code{delta_var} arguments.
#'
#' @references
#' Fleishman AI (1978). A Method for Simulating Non-normal Distributions.
#'  \emph{Psychometrika, 43, 521-532.} \doi{10.1007/BF02293811}.
#'
#' Fialkowski, A. C.  (2018). SimMultiCorrData: Simulation of Correlated Data
#' with Multiple #' Variable Types.  R package version 0.2.2. Retrieved from
#' https://cran.r-project.org/web/packages/SimMultiCorrData/index.html
#' @export
#' @examples
#' # Example data provided with package
#' data(example_data)
#' # Draw a sample based on Score_1
#' output2 <- draw_sample_n(dist=example_data[,c(1,2)],n=200,skew = 0,
#' kurts = 0, location=0, delta_var=0,save.output=FALSE) # Histogram of the reference data set
#' # descriptive statistics of the given data,reference data, and drawn sample
#' output2$desc
#' # First 6 rows of the drawn sample
#' head(output2$sample)
#' # Histogram of the given data set and drawn sample
#' output2$graph
#'\dontrun{
#'# Draw a sample based on Score_2 (location par)
#'# draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,location=-0.5,delta_var=0,
#'# save.output=TRUE, output_name = c("sample", "2"))
#'# Draw a sample based on Score_2 (delta_var par)
#'# draw_sample_n(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,location=0,delta_var=0.3,
#'# save.output=TRUE, output_name = c("sample", "3"))
#'}
draw_sample_n <-  function(dist,n,skew,kurts,
                                 location= 0,
                                 delta_var = 0, save.output = FALSE,
                                 output_name = c("sample","default")){
  

  skew <- round(skew,1)
  kurts <- round(kurts,1)
  names(dist) <- c("id","x")
  # extract x column
  x <- dist$x
  
  if(n >= length(x)){
    stop("Cannot take a sample larger than the length of the data")
  }
  
  # arrange table for negative skewness
  if(skew<0){
    constants_table$c <- -1* constants_table$c
    constants_table$Skew <- -1* constants_table$Skew
    
    if(skew %in% constants_table$Skew == FALSE){
      stop("No valid power method constants could be found for
          the specified values. Change the values")
    }else if (skew %in%  constants_table$Skew == TRUE &
              kurts %in%   constants_table[ constants_table$Skew==skew,]$Kurtosis == FALSE){
      stop("No valid power method constants could be found for
           the specified values.Change the values")
    }
    
  }
  
  #  random generation for the normal distribution
  reference_v1 <- matrix(rnorm(n,0,1),ncol=1)
  reference_v2<- reference_v1*(1+delta_var) + location
  
  

  constants <- constants_table[constants_table$Skew  == 0 & constants_table$Kurtosis  == 0 ,]
  b <- constants$b
  c <- constants$c
  d <- constants$d
  reference_v3 <- -c + b*reference_v2 + c*(reference_v2^2) + d*(reference_v2^3)
  
  
  # Rescale the reference vector to have specified minimum and maximum
  scale_ref <- function(x, from, to) {
    x <- x - min(x)
    x <- x / max(x)
    x <- x * (to - from)
    x + from
  }
  reference_v4 <- scale_ref(reference_v3, from=min(x),to=max(x))
  
  x_counts <-  graphics::hist(reference_v4)$counts
  n_break <-   length(graphics::hist(reference_v4)$breaks) -1
  x_break <-   graphics::hist(reference_v4)$breaks
  
  x_v1 <- as.numeric(cut(x,x_break,include.lowest = TRUE))
  dist2 <- data.frame(dist,x_v1)
  x_n <- unname(table(x_v1))
  
  control <- sum(x_n>= x_counts)
  if(control!=length(x_counts)){
    warning("It is not eligible to extract data with the specified properties from this data
      without replacement.The data set nearest to the values listed will be drawn.")
  }
  
  new_sample <- list()
  ID_list <- list()
  
  
  
  for(i in 1:n_break){
    if(x_n[i]>= x_counts[i]){
      IDx <-  dplyr::filter(dist2,x_v1==i)
      IDx <- dplyr::sample_n(IDx,x_counts[i])
      new_sample[[i]] <- IDx$x
      ID_list[[i]]<- IDx$id
      dist2 <- dplyr::anti_join(dist2,IDx,by = "id")
    }else{
      IDx_e <-  dplyr::filter(dist2,x_v1==i)
      IDx_e <- dplyr::sample_n(IDx,x_n[i])
      IDx_e1 <-  dplyr::filter(dist2,x_v1==i-1 | x_v1==i+1)
      IDx_e1 <-  dplyr::sample_n(IDx,x_counts[i]- x_n[i] )
      new_sample[[i]] <- c(IDx_e$x,IDx_e1$x)
      ID_list[[i]]<- c(IDx_e$id,IDx_e1$id)
      dist2 <- dplyr::anti_join(dist2,IDx_e,by = "id")
      dist2 <- dplyr::anti_join(dist2,IDx_e1,by = "id")
    }
  }
  
  
  new_sample_2 <- unlist(new_sample)
  ID_list_2 <- unlist(ID_list)
  
  S1 <- data.frame(id=ID_list_2,x=new_sample_2)
  
  # Organize the output
  dist3 <-  dplyr::select(dist2,id,x)
  dist3 <-  dplyr::mutate(dist3,type="population")
  S2   <-   dplyr::mutate(S1,type="sample")
  result <-  rbind(dist3,S2)
  # histogram(~x|type,data=result,xlab="Score")
  # to capture the graph
  graph <-      lattice::histogram(~x|type,data=result,xlab="Score",
                                   nint = n_break,
                                   scales = list(x = list(tick.number = 5,relation = "free")))
  print(graph)
  
  if (save.output==TRUE) {
    
  # Save the output
  if (output_name[2] == "default") {
    wd <- paste(getwd(), "/", sep = "")
  }else {wd <- output_name[2]}
  fileName <- paste( output_name[1], wd,".dat", sep = "")
  utils::capture.output(data.frame(S1), file = fileName)
  lattice::trellis.device(device="png",
                          filename=paste(output_name[1], wd,".png", sep = ""))
  grDevices::dev.off()
  }

  



  
  desc <-  rbind(psych::describe(x),
                 psych::describe(reference_v4),
                 psych::describe(S1$x))[,c(2:4,8:9,11:12)]
  rownames(desc) <- c("population","reference","sample")
  # output with three components
  output <- list(desc =desc ,
                 sample = tibble::as_tibble(data.frame(S1)),
                 graph = lattice::trellis.last.object()
  )
  
  return(output)
}
