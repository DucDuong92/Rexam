#2.1

create_fridge <- setRefClass("fridge",
                       fields = list(
                         fridge_content = "data.frame",
                         fridge_volume = "numeric",
                         #menu = "data.frame",
                         Id = "character",
                         amount = "numeric",
                         space = "numeric",
                         space_left = "numeric"),
                       methods = list(
                         
                         initialize = function(fridge_content, fridge_volume){
                          #Check input...
                           #type
                           #volume
                           #unique
                           
                          # Create dataframe
                           fridge_content <<- fridge_content
                           fridge_content[,1] <<- as.character(fridge_content[,1])
                           fridge_volume <<- fridge_volume
                           
                           space_temp <- fridge_content[,2] %*% fridge_content[,3]
                           space <<- space_temp[1,1]
                           space_left <<- fridge_volume - space
                            
                           
                         },
                         add_to_fridge = function(new_content) {
                           #Check input
                           
                           new_content[,1] <- as.character(new_content[,1])
                           num <- length(new_content[,1])
                           #Check value
                           
                           #Insert item
                           for (i in 1:num) {
                             if (new_content[i,1] %in% fridge_content[,1] ) {
                               element <- which(new_content[i,1]==fridge_content[,1])
                               fridge_content[element, 2] <<- fridge_content[element, 2] + new_content[i, 2] 
                               
                             } else {
                               fridge_content[nrow(fridge_content) +1,] <<- new_content[i,]
                             }
                             
                           }
                           #Update space
                           space_temp <- fridge_content[,2] %*% fridge_content[,3]
                           space <<- space_temp[1,1]
                           space_left <<- fridge_volume - space
                           
                         },
                         take_from_fridge = function(new_content) {
                           new_content[,1] <- as.character(new_content[,1])
                           num <- length(new_content[,1])
                           
                           for (i in 1:num) {
                             if (!new_content[i,1] %in% fridge_content[,1]) {
                               stop("Fridge don't have the item(s)")
                             } 
                             else {
                               element <- which(new_content[i,1]==fridge_content[,1])
                               if (new_content[i,2] > fridge_content[element,2]) {
                               stop("The amount is not enough!")
                               }
                             } 
                           }
                             #print(num)
                             for (i in 1:num) {
                               element <- which(new_content[i,1]==fridge_content[,1])
                               #print(element)
                               fridge_content[element, 2] <<- fridge_content[element, 2] - new_content[i,2]
                             }
                             
                             #update space
                             space_temp <- fridge_content[,2] %*% fridge_content[,3]
                             space <<- space_temp[1,1]
                             space_left <<- fridge_volume - space
                             
                          
                         },
                         print = function(){
                           return(fridge_content)
                         }
                       )
)

print <- function(fridge){
  fridge$print()
}


