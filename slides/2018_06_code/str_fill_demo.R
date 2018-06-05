#' @title str_fill_demo
#' @description Compare streching an object and filling in the predefined object.
#' The object is a matrix; in each loop `2^exp` rows are added at the same time. 
#' @param n [required]: number of loops
#' @param ncols [required]: number of columns to create
#' @param exp [required]: Numeric power to which to raise 2 to generate `2^exp` 
#' random numbers
#' @return elapsed_time

str_fill_demo <- function(n = 40, ncols = 16, exp = 20) {
  
  # initialize shared storage objects
  tmp_times <- matrix(-1, nrow = n, ncol = 1)
  
  # a working vector
  # 2*n x 2 matrix
  elapsed_time <- matrix(0, nrow = 2 * n, ncol = 2)
  
  # an output vector
  colnames(elapsed_time) <- c("Cumulative_Time (s)", "Vector")
  elapsed_time[, "Vector"] <- rep(1:2, each = n)
  
  # collect garbage before starting so we have a fair comparison
  
  ###-----------Streching an object ---------------
  obj <- vector(mode = "numeric", length = 0)
 
  # run garbage collection 
  gc()
  
  # strech the vector (obj) in the for loop
  for (i in seq_len(n)) {
    # stretch
    obj <- rbind(obj, matrix(rpois(2 ^ exp, lambda = 3), ncol = ncols))
    tmp_times[i, 1] <- proc.time()[3]
  }
  
  
  # Store information
  elapsed_time[seq_len(n)[-1], 1] <- cumsum(diff(tmp_times[, 1]))

  
  ### --------- Planning an object ----------
  nn <- 2^exp / ncols
  obj <- matrix(-1, nrow = n * nn, ncol = ncols)
  # planned vector
  # collect garbage before starting
  gc()
  
  # loop
  for (i in seq_len(n)) {
    # fill
    obj[seq.int(from = ((i - 1) * nn + 1), to = (i * nn)), ] <- 
      matrix(rpois(2 ^ exp, lambda = 3), ncol = ncols)
    
    tmp_times[i, 1] <- proc.time()[3]
  }
  
  elapsed_time[seq_len(n)[-1] + n, 1] <- cumsum(diff(tmp_times[, 1]))

  
  # coerce to data frame
  elapsed_time <- as.data.frame(elapsed_time)
  elapsed_time$Vector <- factor(elapsed_time$Vector, 
                                labels = c("Stretch", "Plan"))
  elapsed_time$Iter <- rep(1:40, 2)
  
  return(elapsed_time)
}


