## KB 19p A
#Problem1
## KB 4p

#1.a
## KB 2p
# <- use for shallow assign.
# <<- use for deep assign, which mean the variable in the left can be use outside the function.

#1.b
## KB 1p
#If we don't supply an explicit environment, the functions ls() and rm() will look in the current environment.

#1.c 
## KB 1p
## This one is incorrect, I lose one point in here
# Import will import the package
# Depend only check the dependence of the package

# ImportFrom() should take the name of the package and the function that we want to import.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Problem2
## KB 10p
## KB a) 3p
## KB b) 3p
## KB c) 3p
## KB d) 1p
#2.a + 2.b + 2.c

build_Batory <- setRefClass("ship",
                       #Field list     
                       fields = list(
                         max_cargo_weight = "numeric",
                         fuel = "numeric",
                         cargo_weight ="numeric",
                         item = "character",
                         item_weight = "numeric",
                         sailor_weight = "numeric",
                         content = "data.frame"),
                       
                       #method list
                       methods = list(
                         initialize = function(fuel, max_cargo_weight) 
                         {
                           #Check input type
                           if (!is.numeric(fuel) | !is.numeric(max_cargo_weight)) {
                             stop("Input value should be a numeric!")
                           }
                           #Check input value
                           if (fuel<=0 | max_cargo_weight<=0) {
                              stop("Input value shoul be bigger than 0!")
                           }
                           if (fuel > max_cargo_weight) {
                             stop("Max weight should bigger than fuel!")
                           }
                           
                          #Value for object
                           fuel <<- fuel
                           max_cargo_weight <<- max_cargo_weight
                           cargo_weight <<- fuel
                           
                           content <<- as.data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
                           colnames(content) <<- c("ID", "name", "weight")
                           
                           content[1,] <<- list(1, "fuel", fuel) 
                         },
                         
                         equip_Batory = function(name, weight) {
                           #Check input type
                           if (!is.numeric(weight) | !is.character(name)) {
                             stop("Invalid input")
                           }
                           #Check value
                           if ((cargo_weight + weight) > max_cargo_weight) {
                             cat("The ship is full, can not add more items! ")
                           } else {
                             
                           
                           #Add item
                           nrow <- nrow(content) +1
                           content[nrow,] <<- list(nrow, name, weight)
                           #Plus the weight of new item
                           cargo_weight <<- cargo_weight + weight
                           }
                           
                         },
                         embark_sailor = function(sailor_weight) {
                           
                           #Check input type
                           if(!is.numeric(sailor_weight))
                             stop("Weight of sailor should be numeric")
                           
                           #Check weight
                           if(cargo_weight + sailor_weight <= max_cargo_weight){
                             equip_Batory("sailor", sailor_weight)
                           }
                           else {
                             i <- nrow(content)
                             check <- (cargo_weight + sailor_weight > max_cargo_weight)
                             while (check) {
                               
                               if (content[i,2]!= "fuel" & content[i,2]!= "sailor") {
                                 cargo_weight <<- cargo_weight - content[i,3]
                                 content[i,] <<- 0
                                 i <- i-1
                                 check <- (cargo_weight + sailor_weight > max_cargo_weight)
                                if (i<=1) {
                                  cat("The ship is full of sailors and fuel, can not add any sailor!")
                                 check = FALSE
                                }
                                 
                               }
                               else { i <- i-1}
                             }
                             cat("aaaaa")
                             equip_Batory("sailor", sailor_weight)
                           }
                             
                         },
                         print = function(){
                           #cat("Here is the content of the ship!")
                           nrow <- nrow(content)
                           temp_content <- data.frame(matrix(nrow=0, ncol= 3))
                           colnames(temp_content) <- c("ID", "name", "weight")
                           num <- 0
                           for (i in 1:nrow) {
                             if (content[i,1]!= 0) {
                               num <- num +1
                               content[i,1] <<- num
                               temp_content[num,] <- content[i,] 
                             }
                           }
                           return(temp_content)
                         }
                       )
)

#2.b run 50 time
#In this part, If the ship is full, it will not take any item and return the message: The ship is full, can not add more items!
n <- 50 # n is the number of times
item_weight <- rexp(n, 5)

ORP_Batory <- build_Batory(fuel=1.5,max_cargo_weight=10)
for (i in 1:n) {
  ORP_Batory$equip_Batory("food", item_weight[i])
}

#2.c run 10 time
#In this part, I will not remove fuel and sailors
#If the ship is full of fuel and sailors, It will not take any sailors and return the message: "The ship is full of sailors and fuel, can not add any sailor!"
n2 <- 10
sailor_weight<- runif(n2, min = 0.08, max = 0.2)
for (i in 1:n2) {
  ORP_Batory$embark_sailor(sailor_weight[i])
}

#2.d return the ship content 
print <- function(ship_object) {
  return(ship_object$print())
}
print(ORP_Batory)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Problem 3
## KB 5p
#3.1
## KB 3p
multi_maxtrix <- function(A, B){
  #Check input type
  if (!is.numeric(A) | !is.numeric(B)) {
    stop("Input should be a matrix")
  }
  #Convert to matrix, default with byrow= FALSE
  #Use for the input that is a number of vector...
  A <- as.matrix(A)
  B <- as.matrix(B)
  #Check input dimension
  dim1 <- dim(A)
  dim2 <- dim(B)
  
  if (dim1[2]!=dim2[1]) {
      stop("Wrong dimensions")
  }
  value <- c()
  
  #Calculate 
  for (i in 1:dim1[1]) {
    for (j in 1:dim2[2]) {
      temp_value <- 0
          for (k in 1:dim1[2]) {
             total <- A[i,k] * B[k,j]  
             temp_value <- temp_value + total
          }
        value <- c(value, temp_value)
    }
  }
  
  V <- matrix(value, nrow = dim1[1] , ncol=dim2[2], byrow = TRUE)
  return(V)
}


#3.b
## KB 1p
# The function take 3 loops, the first loop is the run n number, the second loop run m number, the third loop run p number.
# So, the complexity of my function is O(n*m*p)

#3c
## KB 1p
A <- matrix(c(1:4), nrow = 2, ncol = 2) 
B <- matrix(c(5:8), nrow = 2, ncol = 2) 
V1 <- multi_maxtrix(A, B)
V2 <- A%*%B

library(testthat)

test_that("Check the return type of the function", {
  expect_is(V1, "matrix")
})

test_that("Check the return value of the function", {
  expect_equal(V1, V2)
})
