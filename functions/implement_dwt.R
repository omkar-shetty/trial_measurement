library(forecast)
library(quantmod)

tickers = 'SPY'

set.seed(123)
ts1 <- sample(c(1:20),12)
ts2 <- sample(c(1:20),12)

#' Title
#'
#' @param ts1 first time series
#' @param ts2  second time series
#' @param window neighbourhood within which the closest point can be identified
#'
#' @return similarity value
#' @export
#'
#' @examples
calc_similarity_measure <- function(ts1, ts2, window){
  
  dist_mat <- as.matrix(cluster::daisy(cbind(ts1,ts2), metric = 'euclidean'))
  diag(dist_mat) <- Inf
  
  for(i in length(ts1)){
    for(j in length(ts2)){
      sub_mat <- dist_mat[c()]
    }
  }
  
}