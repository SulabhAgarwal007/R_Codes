# Read Census data

data = read.csv("Edvancer Data Science/R Code/datasets/census_income.csv", header = TRUE)
head(data)

l = numeric()
j=1
library(psych)
for(i in 1: length(data)){
  if(class(data[,i]) == "integer"){
    print(i)
    #append(l, i, after = length(l))
    l[j]=i
    j = j +1
  }
}

print(l)

describe(data[,l])

# Summary of categorical vairable

for(i in 1: length(data)){
  if(class(data[,i]) == "factor"){
    #print(i)
    #append(l, i, after = length(l))
    print(cat("Summary for ",colnames(data[i])))
    print(table(data[, i]))
  }
}

t=table(data$education, data$Y)
t = prop.table(t,2)
print(round(t,2))

#histogram

hist(data$fnlwgt)
hist(data$education.num)

q1 = quantile(data$fnlwgt)
q1[2]
range1 = c(q1[2]-1.5*(q1[4]-q1[2]), q1[4]+1.5*(q1[4]-q1[2]))
sum((data$fnlwgt<range1[1] | data$fnlwgt>range1[2]))

