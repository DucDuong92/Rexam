#1.a It varibles and function can effect your work. 
#    For example, it can have a function with the same name as some default function in start but it content is different.

#1.b

f <-function(a,b){
  return(a+b)
}

package.skeleton()

#1.c
#tests: This directory contains test suite using for test the package. 
#vignette: This directories contain long-form guide of the package
#data: Contain .rad files. which stores the binary data. The data can make available for the user.

#2.a + 2.b

create_household <- setRefClass("household",
                            fields = list(
                              address = "character",
                              number_of_devices= "numeric",
                              Id = "numeric",
                              time = "numeric",
                              connection_rate= "numeric",
                              days = "numeric",
                              history = "data.frame"),
                            
                           methods = list(
                             initialize = function(address, number_of_devices){
                               
                              # Check input
                               if (!is.numeric(number_of_devices) | !is.character(address)) {
                                 stop("Invalid input!")
                               }
                               if (number_of_devices<1) {
                                 stop("Number of devices should be >= 1")
                               }
                               
                               #Create data frame to storge the data
                              address  <<- address
                              number_of_devices <<- number_of_devices
                              history <<- data.frame(matrix(ncol = 4, nrow = 0))
                              colnames(history) <<- c("address", "Id", "time", "connection_rate")
                              time <<- 0
                              
                              #Create data for devices
                              for (i in 1:number_of_devices) {
                                history[i,] <<- c(address, i, time, runif(1))
                              }
                                
                             },

                             # 2.b
                             simulate_internet_usage = function(days) {
                               if (!is.numeric(days)) {
                                 stop("Days should be a number!")
                               }
                               if (days <1) {
                                 stop("Days should be greater than 0")
                               }
                               
                               for (i in 1:number_of_devices) {
                                 for (j in 1:days) {
                                    time <<- time + rexp(1, rate = as.numeric(history[i,4]))
                                 }
                                 history[i,3] <<- time
                                 time <<-0
                               }
                               return(history)
                             }
                           )
)

#2.c
plot <- function(house){
  library(ggplot2)
  house[,2] <- as.numeric((house[,2]))
  house[,3] <- as.numeric((house[,3]))
  ggplot(house, mapping = aes(x=house[,2], y= house[,3])) +
    geom_bar(stat = "identity") +
  labs(title    = "Bar plot of time for using device", x="Device ID" , y="Time (s)")
}


#3.a
scalar_product<- function(x,y){
  #Check input
  if (!is.numeric(x) | !is.numeric(y))
    stop("Input should be a vector!")
   if (length(x) != length(y)) {
     stop("Length of input vectors should be equal!")
   } 
    leng <- length(x)
    total <- 0
    
    for (i in 1:leng) {
        total <- total + x[i]*y[i]
    }
    return(total)
}

#3.b 
#suppose that the input vector dimention is n
#Compleysity: O(n)

#3.c
library(testthat)

x <-1:4
y <-5:8
value1 <- scalar_product(x,y)
value2 <- x%*%y

test_that("value is equal", {
  expect_equal(value1[1], value2[1,1])
})
