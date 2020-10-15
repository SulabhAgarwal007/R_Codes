# Create vector like 2,4,8,...1024

 ans1 = 2**c(1:10)

# Create vector like a26, b25...z1
 ans2 = paste0(letters,c(26:1))

# Extract city names
 address_list=c("802/hiranandani/Mumbai",
                "2A/kalka-Delhi",
                "345#near adyar#Chennai",
                "10-shyaam bazzar-Kolkata")
 
 temp1=strsplit(address_list, "[/#-]", fixed = FALSE)
 class(temp1)
for(i in 1:length(temp1)){
  print(temp1[[i]][3])
}
 
 # print prime numbers between 48 and 100
 prime = function(x){
   a=x/2
   j=0
   for (i in 2:a) {
      if(x%%i==0){j=1}
   }  
   if(j==0){
     print(x)
   }
 }
 
 for(i in 48:100){
   (prime(i))
 }
 
# Find out , how many cars are there are in the dataset mtcars which have automatic transmission, number
 # of forward gears higher than 3 and below average mileage.
 rownames(mtcars[which(mtcars$am==0 & mtcars$gear>3 & mtcars$mpg < mean(mtcars$mpg)),])

 # Write Mode function
 
 set.seed(2)
 x=sample(letters[1:5],50,replace=T)
 y=sample(letters[1:3],50,replace=T)
 
 mode <- function(x){
          print(x[which(table(x)==max(table(x)))])
 }
 
 mode(x)
 mode(y)
 
 
 
 