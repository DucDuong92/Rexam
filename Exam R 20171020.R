#1.1
rm(list = ls())
#1.2
f <- function(x){
  return(x*x)
}

#1.3

# file "Read-and-delete_me" have some information about package. We should delete it after reading
# file "NAMESPACE" 
# file DESCRPTION contains introduction information about the package like name, author, license..
# folder R has R script (.R files) for this package, for example the script for task 1.2 
# folder man has .Rd files, which describe information about the R script. This .Rd files generate base on documents in R script.

#2.1
build_mgb <- setRefClass("MGB",
                       fields = list(
                         name = "character",
                         speed = "numeric",
                         status = "character",
                         weight = "numeric",
                         rate = "numeric",
                         travel_time = "numeric",
                         history = "data.frame"),
                       
                       methods = list(
                         
                         initialize = function(name, speed)
                           {
                           #Check input
                           if(!is.character(name))
                             stop("Name should be a character!")
                           if (!is.numeric(speed)) {
                             stop("Speed should be a numeric!")
                           }
                           
                           #Create data frame to storage the run history 
                           history <<- data.frame(matrix(ncol = 5, nrow = 0))
                            colnames(history) <<- c("name", "speed", "weight", "travel_time", "status")
                            name <<- name
                            speed <<- speed
                            status <<- "in_service"
                         },
                         
                        
                         simulate_home_run = function() {
                            # Check input: sunk or in_service
                           if (status== "sunk") {
                             cat(name) 
                             cat(" is reported lost")
                           } 
                           else {
                            
                            #Calculate   
                           weight <<- rnorm(1, mean = 40, sd = 2)
                           rate <<- 1/(weight*900/speed/250)
                           travel_time <<- rexp(1, rate = rate)
                           
                           #Check sunk
                           if (travel_time >10) {
                             status <<- "sunk"
                           } #else cat("Success")
                           
                           #Insert to history
                           history[nrow(history) +1,] <<- list(name,speed, weight, travel_time, status)
                           }
                           return(history)
                         },
                         #function that print the history
                         report = function() {
                           print(history)
                         }
                         
                       )
)


#2.c
MGB_504 <- build_mgb(name="Hopewell",speed=46)
for (i in 1:100) {
  history <- MGB_504$simulate_home_run()
}

#function that run 100 times and draw result.
plot = function(history){
  library(ggplot2)
  times <- c(1:nrow(history))
  ggplot(data=history, mapping= aes(y=travel_time, x=times)) + 
    geom_line() +
    labs(title="Line plot of travel time")
}


#3.1
find_max_value <- function(x){
  #Check input
  if (!is.vector(x))
    stop("Input should be a vector!")
  if (!is.numeric(x))
    stop("Vector should contain number!")
    
  
  max <- 0
  position <- 0
  for (i in 1:length(x)) {
    if (max <= x[i]) {
      max <- x[i]
      position <- i
    }
    
  }
  output <- list("maxvalue"= max, "max_value_position"= position)
  return(output)
}

#3.2
#O(x)

#3.3
find_max_value_2 <- function(x){
  
  if (!is.vector(x))
    stop("Input should be a vector!")
  if (!is.numeric(x))
    stop("Vector should contain number!")
  
  max <- 0
  position <- 0
  
  #Invert, find min and invert again
  x <- -x
  max <- min(x)
  a <- match(x, max)
  position <- tail(which(a == 1),1)
  max <- - max
  output <- list("maxvalue"= max, "max_value_position"= position)
  return(output)
}


#3.4

library(testthat)
context("find_max")


test_that("check wrong input", {
  expect_error(a <- find_max_value("example") )
  expect_error(a <- find_max_value2("example") )
})

#initial value 
x<-c(1,2,3,56,4,56,5)
test1 <- find_max_value(x)
test2 <- find_max_value_2(x)

test_that("return correct value", {
  expect_equal(test1[[1]], 56 )
  expect_equal(test1[[2]], 6 )
  expect_equal(test2[[1]], 56 )
  expect_equal(test2[[2]], 6 )
})