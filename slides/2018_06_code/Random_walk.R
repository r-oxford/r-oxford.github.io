#' @title rw2d_loop
#' @description Random walk function; the output dataframe holds information 
#' about steps. The temporary vectors have not been prealocated
#' @param n [required]: number of steps

rw2d_loop <- function(n) {
  
  xpos <- 0
  ypos <- 0  
  
  # Initialise the first step
  
  xdir <- c(TRUE, FALSE) # go right
  pm1 <- c(1,-1) # up, down
  
  for (i in 2:n){
    
    if (sample(xdir, 1)) {
      xpos[i] <- xpos[i-1] + sample(pm1, 1)
      ypos[i] <- ypos[i-1]
    }
    else {
      xpos[i] <- xpos[i-1]
      ypos[i] <- ypos[i-1] + sample(pm1, 1)
    }
  }
  result <- data.frame(x = xpos, y = ypos) 
  
  return(result)
}


#' @titlerw2d_loop_1
#' @description Random walk function; the output dataframe holds information 
#' about steps. The tem. vectors have been prealocate and the random samples
#' generated in advance 
#' @param n [required]: number of steps

rw2d_loop_1 <- function(n) {
  
  # create vectors of length n
  xpos <- numeric(n) 
  ypos <- numeric(n) 
  
  xdir <- c(TRUE, FALSE)
  pm1 <- c(1,-1)
  
  # create all random values in advance
  x_s <- sample(xdir, n, replace = TRUE)
  pm_s <-sample(pm1, n, replace = TRUE)
 
  
   for (i in 2:n){
     if (x_s[i]) {
       xpos[i] <- xpos[i-1] + pm_s[i]
       ypos[i] <- ypos[i-1]
     }
     else {
       xpos[i] <- xpos[i-1]
       ypos[i] <- ypos[i-1] + pm_s[i]
     }
     
  }
  result <- data.frame(x=xpos, y=ypos) 
  return(result)
}


#' @title rw2d2_recursive
#' @description Recursive version of random walk function; 
#' @param n [required]: number of steps
#' @param x: starting x-coordinate
#' @param y: starting y-coordinate
rw2d2_recursive <- function(n, x = 0, y = 0) {
  if(n == 1) return(cumsum(data.frame(x=x, y=y)))
  step = sample(1:4, 1)
  result <- rw2d2_recursive(n=n-1, 
                            x=c(x, c(-1, 1, 0, 0)[step]), 
                            y=c(y, c(0, 0, -1, 1)[step]))
  
  return(result)  
}
