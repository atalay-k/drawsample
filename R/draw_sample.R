#'
#' Draw Samples with the Desired Properties from a Data Set
#'
#' A function to sample data with desired properties.
#' 
#' @param dist         data frame:consists of id and scores with no missing
#' @param n            numeric: desired sample size
#' @param skew         numeric: the skewness value
#' @param kurts        numeric: the kurtosis value
#' @param replacement  logical:Sample with or without replacement?
#' (default is FALSE).
#' @param save.output logical: should the output be saved into a text file? (default is FALSE).
#' @param output_name character: a vector of two components.
#'                    The first component is the name of the output file,
#'                    user can change the second component.
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
#' data(example_data)
#' # First 6 rows of the example_data
#' head(example_data)
#'# Draw a sample based on Score_1(from negatively skewed to normal)
#' output1 <- draw_sample(dist=example_data[,c(1,2)],n=200,skew = 0,kurts = 0,
#' save.output=FALSE) # Histogram of the reference data set
#' # descriptive statistics of the given data,reference data, and drawn sample
#' output1$desc
#' # First 6 rows of the drawn sample
#' head(output1$sample)
#' # Histogram of the given data set and drawn sample
#' output1$graph
#'\dontrun{
#'# Draw a sample based on Score_2 (from negatively skewed to positively skewed)
#'# draw_sample(dist=example_data[,c(1,3)],n=200,skew = 1,kurts = 1,
#'# output_name = c("sample", "1"))
#'# Draw a sample based on Score_2 (from negatively skewed to positively skewed
#'# with replacement)
#'# draw_sample(dist=example_data[,c(1,3)],n=200,skew = 0.5,kurts = 0.4,
#'# replacement=TRUE,output_name = c("sample", "2"))
#'}
draw_sample <-  function(dist,n,skew,kurts,
                         replacement =FALSE,save.output = FALSE,
                         output_name = c("sample","default")){
  
  # rename the data
  skew <- round(skew,1)
  kurts <- round(kurts,1)
  names(dist) <- c("id","x")
  # extract x column
  x <- dist$x
  
  
  N <- length(x)
  if(n >= length(x)){
    stop("Cannot take a sample larger than the length of the data")
  }
  
  # arrange table for negative skewness
  if(skew<0){
    constants_table$c <- -1*constants_table$c
    constants_table$Skew <- -1*constants_table$Skew
  }
  
  
  if(skew %in% constants_table$Skew == FALSE){
    stop("No valid power method constants could be found for
          the specified values. Change the values")
  }else if (skew %in% constants_table$Skew == TRUE &
            kurts %in%  constants_table[constants_table$Skew==skew,]$Kurtosis == FALSE){
    stop("No valid power method constants could be found for
           the specified values.Change the values")
  }
  
  reference_v3 <- NULL
  
  # conduct Fleishman's power method for the specified
  # skewness and standardized kurtosis
  repeat{
    for( i in 1:dim(constants_table)[1]){
      #  random generation for the normal distribution
      reference <- stats::rnorm(n,0,1)
      constants <- constants_table[i,3:5]
      b <- constants$b
      c <- constants$c
      d <- constants$d
      reference_v2 <- -c + b*reference + c*(reference^2) + d*(reference^3)
      skew_value <- round(psych::describe(reference_v2)$skew,1)
      kurt_value <- round(psych::describe(reference_v2)$kurtosis,1)
      if(skew_value == skew & kurt_value == kurts){
        reference_v3 <- reference_v2
        break
      }
    }
    if(is.null(reference_v3) == FALSE)
      break
  }
  
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
    if(replacement==FALSE){
      stop("Cannot take a sample form that data without replacement.
           Please change replacement=TRUE")
    }
  }
  
  new_sample <-  matrix(NA, nrow = n_break, ncol = max(x_counts))
  ID_list <-  matrix(NA, nrow = n_break, ncol = max(x_counts))
  
  new_sample_2 <- list()
  ID_list_2 <- list()
  
  for(i in 1:n_break){
    new_count <- 0
    j <- 0
    while(new_count < x_counts[i]){
      j <- j + 1
      IDx <- dplyr::filter(dist2,x_v1==i)
      IDx <- dplyr::sample_n(IDx,1)
      if(replacement==FALSE){
        dist2 <- dplyr::filter(dist2,id!=IDx$id)
      } else{ dist2 <- dist2}
      new_count <- new_count + 1
      new_sample[i,j] <- IDx$x
      ID_list[i,j]<-   IDx$id
    }
    new_sample_2[[i]] <-  stats::na.omit(new_sample[i,])
    ID_list_2[[i]] <-  stats::na.omit(ID_list[i,])
  }
  
  new_sample_3 <- unlist(new_sample_2)
  ID_list_3 <- unlist(ID_list_2)
  S1 <- data.frame(id=ID_list_3,x=new_sample_3)
  
  
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

