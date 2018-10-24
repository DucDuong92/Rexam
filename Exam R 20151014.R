# Exam R 20151014

#1.a
f <- function(x) {
  #Check input
  if (!is.vector(x)) {
    stop("x should be a vector")
  }
  
  
  leng <- length(x)
  sum <- 0
  for (i in 1:leng) {
    sum <- sum + x[i]
  }
  #print(sum)
  mean <- sum/leng
  value <- 0
  for (j in 1:leng) {
    value <- value + abs(x[j] - mean)  
  }
  output <- value/leng
  return(output)
}

# 1.b
#comlexity is O(x)

# 1.c
library(ggplot2)
x <- 1:200
y <- rnorm(x)
qplot(x=x,y=y, geom ="line")


#2.1
hilbert <- function(n,m){
  mat <- matrix(0, nrow = n, ncol = m, byrow = TRUE)
  for (i in 1:n) {
    for (j in 1:m) {
      mat[i,j] <- 1/(i + j -1)
    }
  }
  
  return(mat)
}

#2.2
H <- hilbert(5,5)
value  <- det(H%*% t(H))
# value is the result of this question

#2.3 
library(testthat)
test_that("hilbert() is working",{
  H<-hilbert(1,4)
  expect_is(H,"matrix")
  expect_equal(H,matrix(c(1,1/2,1/3,1/4),nrow=1))
})

#4.1
counter_factory <- function(start, max){
  i <- start
  f <- function(){
   # i<-start
    #function(){
      if(i < max) i <<- i + 1
      return(i)
  }
}

#4.2

counter_factory <- setRefClass("counter_factory",
                               fields = list( start = "numeric",
                                              max = "numeric",
                                              i = "numeric"),
                               
                               methods = list (
                                initialize = function(start, max){
                                  i <<- start
                                  max <<- max
                                 },
                                 
                                 summary = function() 
                                   {
                                   if(i < max) i <<- i + 1
                                   cat("Count object\ni:",i)
                                   cat( "\nmax: ", max)
                                  }
                                 )
)




counter_factory <- function(start, max){
  i<-start
  function(){
    if(i < max) i <<- i + 1
    res <- c(i, max)
    class(res) <- "counter"
    res
  }
}
summary.counter <- function(x, ...){
  cat("Count object\ni:", x[1], "\nmax: ",x[2])
}
  

#4.3

#'@title A counter factory
#'@param start Starting iteration number
#'@param max Maximum iterations
#'@description Creates a counter object that is a clojure.
#'@value Returns a function/clojure with environment containing i and max