
library(dplyr)

cars = as.data.frame(mtcars)
print(typeof(cars))

apply(cars, 2, median) # here 1 - row wise, 2 - column wise
apply(cars, 2, mad)

lapply(cars, function(x) x/2) # this is element wise operation and output is a list
lapply(cars, mean) # this is a group operation

sapply(cars, mean) # this is quite similar to lapply, but output is a vector


