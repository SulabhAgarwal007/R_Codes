# proving central limit theorem
library(ggplot2)
set.seed(1)
d = data.frame(X = rbeta(20000, 3, 10)) # positively skewed graph for population
ggplot(d, aes(x=X)) + geom_histogram()

fun_mean = function(df, sample_size=100){
    avg = list()
    for(i in 1 : 1000){
      s = sample(1: length(df), size = sample_size)
      avg[i] = mean(df[s])
  }
  return(avg)
}

a = as.numeric(fun_mean(d$X))
df = data.frame(a)

#hist(a)
ggplot(df, aes(x=a))+
  geom_histogram()
sprintf("SD of population: %f", sd(d$X))
sprintf("Variance of population: %f", var(d$X))
sprintf("SD of sample: %f", sd(df$a))
sprintf("Variance of sample: %f", var(df$a)) # see this is i/n times of population vairnace
