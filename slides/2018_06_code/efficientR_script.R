
# object size
vec <- c(1, 2, 1, 1, 2, 2, 2)
f <- factor(vec)
ch <- as.character(vec)
dbl <- as.double(vec)
intg <- as.integer(vec)

object.size(f)
object.size(ch)
object.size(dbl)
object.size(intg)


#-------------------- Vectorise  --------------------
N <- 10000
x1 <- runif(N)
x2 <- runif(N)
d <- as.data.frame(cbind(x1, x2))

# calculate mean of each row
mn1 <-rep(NA, nrow(d))
system.time({
  for (i in seq_along(d$x1)) {
   mn1[i] <- mean(c(d[i, 1], d[i, 2]))
  }
})

# call the base rowMeans function (coded in C and compiled)
system.time( mn2<- rowMeans(d[, c(1, 2)]))




#-------------------- Matrices/Data Frames --------------------
# Use matrices instead of dataframes whenever you can

df1 <- data.frame(A = seq_len(1e4), B = 0)
head(df1, n = 3)

# change elements one by one (in a data frame)
system.time(for (i in seq_len(1e4)) { df1[i, "B"] <- i })

# change elements one by one (in a matrix)
mt1 <- cbind(A = seq_len(1e4), B = 0)
head(mt1, n = 3)

system.time(for (i in seq_len(1e4)) { mt1[i, "B"] <- i })


library(data.table)
?data.table


#--------------------Strech or pre-allocate--------------------


source("str_fill_demo.R")
times <- str_fill_demo(n = 40)
library(ggplot2)
qplot(x = Iter, y = `Cumulative_Time (s)`, data = times, 
      colour = Vector, 
      geom = "line")


# Some simple examples of extending the vector

n <- 50000

# case 1 Dinamicaly grow the vector
system.time({
  vec <- c()
  for(i in 1:n) vec[i] <-i
})

# case 2 -  Predifine the vector; use the loop
system.time({
  vec <- rep(0, n)
  for(i in 1:n) vec[i] <- i
})

# case 3 - Use `seq` function (coded in C)
system.time({
  vec <- seq(from = 1, to = n, by = 1)
})




#-------------------- Subsetting --------------------
# dataframe 1e+05 rows and 26 columns
df1 <- as.data.frame(matrix(rnorm(2.6e6), ncol = 26))
colnames(df1) <- LETTERS
head(df1)
# add the column "ID" 
df1$ID <- rep(seq_len(1e3), each = 1e2)
head(df1)

# we want to create a vector of sums of selected elements
# use dplyr::filter
library(dplyr)
out1 <- rep(NA, 1e3)
fltr <- system.time(for (i in seq_len(1e3)) {
  
  sub <- df1 %>% 
    filter(ID == i) %>%
    select(A, B, C)
  
  out1[i] <- sum(abs(sub))
})
print(fltr)



# use base::subset
out2 <- rep(NA, 1e3)
sbs <-system.time(for (i in seq_len(1e3)) {
  sub <- subset(df1, subset = ID == i, select = c("A", "B", "C"))
  
  out2[i] <- sum(abs(sub))
})
print(sbs)

# use [,] notation
out3 <- rep(NA, 1e3)
sqbr <- system.time(for (i in seq_len(1e3)) {
  
  sub <- df1[df1$ID == i, c("A", "B", "C")]
  
  out3[i] <- sum(abs(sub))
})

print(sqbr)

data.frame(method = c("filter", "subset", "[,]"),
           user_time = c(fltr[[1]], sbs[[1]], sqbr[[1]]))

  
  


#-------------------- Loops --------------------

# Loops are not bad
# Some other things in combination with loops are

source("Random_walk.R")




# ------- TEST ------- 

n<- 1e6

# grow one at the time
system.time({
  rw2d_loop(n)
})

# create the space first; do the sampling in advance
system.time({
  rw2d_loop_1(n)
})

# recursion
# this one is going to break; 
# Error: C stack usage is too close to the limit
system.time({
  rw2d2_recursive(n)
})


#  What is slow: nested loops 

# Nested loops can be very slow; If the speed is important spend some time 
# thinking how to avoid them
# If you have to use them, think about any efficiency gain if possible

# Create a distance matrix based on the Euclidean distance between 
# rows in a given matrix m
doubleLoopDist <- function(m) {
  n <- nrow(m)
  dists <- matrix(0, ncol=n, nrow=n)
  for (i in 1:n){
    for (j in 1:n){
      dists[i,j] <- sqrt(sum((m[i,] - m[j,])^2))
    }
  }
  return(dists)
}


# Since the distance matrix is symmetric, we only need to compute half of it, 
# then fill in the rest. This would at least cut the time in half

doubleLoopDist2 <- function(m) {
  n <- nrow(m)
  dists <- matrix(0, ncol=n, nrow=n)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      dists[i,j] <- sqrt(sum((m[i,] - m[j,])^2))
    }
  }
  
  dists <- dists + t(dists) + diag(diag(dists))
  diag(dists) = 0
  return(dists)
}

# ------- TEST ------- 

d <- matrix(rnorm(10000), ncol=100)

system.time({
  doubleLoopDist(d)
})

system.time({
  doubleLoopDist2(d)
})



# ====== Apply Functions

#  There is a misconception that these functions are faster than loops, but this 
#  is incorrect. They can be slightly faster, but frequently can even be slower.

#  BUT, they are parallelizable, and so POTENTIALLY far faster. 
#  With apply family the gain is in coding efficiency and in computation 
#  time with the use of parallelization.

# apply functions

# apply: arrays, matrices, data.frames
# lapply, sapply, vapply: lists, data.frames, vectors
# tapply: grouped operations (table apply)
# mapply: multivariate version of sapply
# replicate: similar to sapply

# Annoying feature: input and output objects can be of different type 


# Apply handles these memory allocation issues for you, but then you have to 
# write the loop part as a function to pass to apply. 

# At its heart, apply is just a for loop with extra convenience.

x <- matrix(rnorm(1e7), ncol=1e6)

means_loop <- rep(0, ncol(x))  # initialize the means vector

system.time({
  for (n in 1:ncol(x)){
    means_loop[n] <- mean(x[,n])
  }
})

system.time({
  means_apply <- apply(x, 2, mean)
})

# this is going to take veeery long time (we are working with data frame here).
# I gave up after 2min, so let's lower the number of columns
x <- matrix(rnorm(1e5), ncol=1e4)

means_loop <- rep(0, ncol(x))

library(dplyr)
system.time({
  tmp <- as.data.frame(x)
  summarise_all(tmp, mean)
  
})




# ---------------- Reading tables ---------------- 
library(readr)
dat <- as.data.frame(matrix(rnorm(1e7), ncol = 20),
                     col.names = LETTERS[1:20])

dat[,1] = "a"

write.table(dat, 
            file = "dat.csv", 
            sep = ",", 
            col.names = LETTERS[1:20], 
            row.names = FALSE)


system.time(read.table("dat.csv", header = T))

library(readr)
system.time(a<- read_csv("dat.csv"))


library(data.table)
system.time(fread("dat.csv"))


# ---------------- Profiling ---------------- 

library("profvis")
profvis({
  data(movies, package = "ggplot2movies") # Load data
  movies = movies[movies$Comedy == 1,]
  plot(movies$year, movies$rating)
  model = loess(rating ~ year, data = movies) # loess regression line
  j = order(movies$year)
  lines(movies$year[j], model$fitted[j]) # Add line to the plot
})


# or you can use Rprof 

# call Rprof
Rprof("prof.txt")

# code to profile
data(movies, package = "ggplot2movies") # Load data
movies = movies[movies$Comedy == 1,]
plot(movies$year, movies$rating)
model = loess(rating ~ year, data = movies) # loess regression line
j = order(movies$year)
lines(movies$year[j], model$fitted[j]) # Add line to the plot

# close Rprof
Rprof(NULL)

# summarize the results
summaryRprof("prof.txt")


