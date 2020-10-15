library(dplyr)
library(lubridate)
library(car)

getwd()
setwd("C:/Users/Sulabh/Documents/Edvancer Data Science/R Code")

df = data.frame(read.csv("datasets/Cycle_Shared.csv", sep=",", stringsAsFactors = F))
df$day = day(ymd(df$dteday)) 

#  mutate(day(df_mod$dteday)) 
# break into 3 parts
set.seed(1)
train_val = sample(1: nrow(df), 0.8*nrow(df))

df_trainval = df[train_val,] 
# df_train$dataset = "train"
# dim(df_train)

df_test = df[-train_val, ]

train = sample(1:nrow(df_trainval), 0.7*nrow(df_trainval))
df_train = df[train,]
df_val = df[-train,]
# df_val$dataset = "test"
dim(df_val)

rm(df_trainval)

fit = lm(cnt~.-instant-dteday-casual-registered, data=df_train)
sort(vif(fit))

fit = lm(cnt~.-instant-dteday-casual-registered-atemp, data=df_train)
sort(vif(fit))

fit = step(fit)
summary(fit)

sqrt(mean(predict(fit, df_test) - df_test$cnt)**2)

plot(fit, which=2)
