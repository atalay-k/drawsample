#' Multiple Sample Selection
#'
#' @param dist          data frame:consists of id and scores with no missing
#' @param n             numeric: desired sample size
#' @param rep           numeric: replication
#' @param skew          numeric: the skewness value
#' @param kurts         numeric: the kurtosis value
#' @param replacement   logical:Sample with or without replacement? (default is FALSE).
#' @param col_id        index of column ID's
#' @param col_total     index of column total score
#' @param exact         default is FALSE conduct draw_sample_n_ir function, it is faster and nearest version of draw_sample_ir function.
#' @return This function returns a \code{list} including following:
#' \itemize{
#' \item a matrix: Descriptive statistics of the given data,
#'                 the reference vector and the sample.
#' \item a data frame: The id's and scores of the sample
#' \item graph: Histograms for the “data” and the “sample”
#' }
#' @export
#' @examples
#' # Example data provided with package
#' data(likert_example)
#' # First 6 rows of the example_data
#' head(likert_example)
#' # Draw three samples based on Score_1(from negatively skewed to normal)
#' # This example takes considerable computation time.
#' samples <- draw_sample_rep(dist=likert_example,n=200,rep=3,skew=0,
#' kurts=0,replacement =TRUE,  col_id = 1,
#' col_total = numeric(),
#' exact = FALSE)
#' # to get first sample
#' samples$sample[[1]]
#' # to get second sample
#' samples$sample[[2]]
#'\dontrun{
#' # to export 10 samples
#' for(i in 1:3){
#'  write.csv(samples$sample[[i]],row.names = FALSE,paste("sample_",i,".csv",sep=""))
#'  }
#' }
draw_sample_rep <- function(dist,n,rep=1,skew,kurts,replacement =TRUE,
                            col_id=1,col_total=numeric(),
                            exact=FALSE){
  
  i=1
  result_desc <- list()
  result_sample <- list()

  if(exact==FALSE){
  while(i < (rep+1)) {
    skip_to_next <- FALSE
    tryCatch({
      set.seed(sample(1:10000,1))
      result_1 <- draw_sample_n_ir(dist =dist, n = n,
                                           skew=skew,kurts=kurts,
                                           col_id=col_id,col_total=col_total,
                                           output_name = c("sample",paste(i))) },
      error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    if(is.na(result_1$desc[3,6])==FALSE){
      result_desc[[1]] <- result_1$desc[1,]
      result_desc[[i+1]] <- result_1$desc[3,]
      result_sample[[i]] <- result_1$sample
      i= i +1
    }else{i=i}
  }
  }else{
    while(i < (rep+1)) {
      skip_to_next <- FALSE
      tryCatch({
        set.seed(sample(1:10000,1))
        result_1 <- draw_sample_ir(dist =dist, n = n,
                                                 skew=skew,kurts=kurts, replacement =FALSE,
                                                 col_id=col_id,col_total=col_total,
                                                 output_name = c("sample",paste(i))) },
        error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next }
      if(is.na(result_1$desc[3,6])==FALSE){
        result_desc[[1]] <- result_1$desc[1,]
        result_desc[[i+1]] <- result_1$desc[3,]
        result_sample[[i]] <- result_1$sample

        i= i +1
      }else{i=i}
    }
}
 


  desc =  do.call(rbind,result_desc)
  row.names(desc) <- c("population", paste("sample",1:rep,sep ="_"))  
  output <- list( desc = desc,
                 sample = result_sample)

  return(output)
  
  
}
  

  
