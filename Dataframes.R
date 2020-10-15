?mtcars
data("mtcars")
View(mtcars)

n = !(names(mtcars) %in% c('drat', "wt")) # this is to check values in a list

d = mtcars[, n]
d[d$mpg>18 & d$cyl>4, ]

library(dplyr)
# joining datasets based on keys
df1 = data.frame(id=c(1:5), product=c(rep('cric',3),rep('hockey', 2)))
df2 = data.frame(id=c(1,2,4,5), product=c('bat','bat','stick','stick'))
df3 = inner_join(df1, df2, by='id')

# summary statistics
v_summary = summary(mtcars)
library(psych)
v_describe = describe(mtcars)

library(dplyr)
glimpse(mtcars)
# in a box plot the whiskers are calculated as per below:
# upper whisker = min(max(x), Q3+1.5*IQR)
# lower whisker = max(min(x), Q1-1.5*IQR)

# Standard ex of Range for a varaiable 
# Mean +- N*SD
# Q1 - N*IQR, Q3 + N*IQR

getwd()

setwd("C:/Users/Sulabh/Documents")
# Note / instead of \ in Windows. 

# Reading multiple files from directory into a single variable or dataframe

setwd("C:/Users/Sulabh/Documents/Edvancer Data Science/R Code/datasets/Data/Data/namesbystate")
f = list.files(getwd(), pattern = "*.TXT")
# at this stage f is list of filenames

d = lapply(f, read.csv, header=F, stringsAsFactors=F)
# at this stage d is list of dataframes per file

data = do.call(rbind, d)
# at this stage all dataframes merged into one dataframe using rbind

names(data) = c("state","gender","year","name","count")


library(vcd) # load data set Arthritis

glimpse(Arthritis)
# Dataframe operations

# Adding new column
Arthritis$above40 = as.numeric(Arthritis$Age>40)

Arthritis$Improved_Above40 = ifelse(Arthritis$Improved!="None" & Arthritis$Age>40, "Y", "N")

# sample(c(1:20, rep(NA,5)),5) - creating a sample vector of 5 variables with NA

# Impute: Imputation is filling missing values in a dataset with either arithemetic 
# values like mean/median or some business logic or calculated values

# e.g.

df = data.frame(v1 = sample(c(1:50, rep(NA,5)), 20),
                v2 = sample(c(1:50, rep(NA,5)), 20),
                v3 = sample(c(1:50, rep(NA,5)), 20),
                v4 = sample(c(1:50, rep(NA,5)), 20),
                v5 = sample(c(1:50, rep(NA,5)), 20)
                )

apply(df,2, function(x) sum(is.na(x))) # here 2 means column wise, 1 mean row wise

df$v1[is.na(df$v1)] = mean(df$v1, na.rm = T) # imputing with mean
df$v2[is.na(df$v2)] =  median(df$v2, na.rm =T) # imputing with median
df$v3[is.na(df$v2)] = 999 # imputing with some business logic

df_new = na.omit(df) # removes all the observations having NA anywhere
# here if we see the rownames of df_new it will have missing rows deleted from df
# to fix it:
rownames(df_new) = 1: nrow(df_new)

# dplyr main useful functions:

# filter, select, arrange, mutate, summarize + group_by

library(dplyr)
library(hflights)

describe(hflights)
tbl_df(hflights)

# filter in Base R
df_fl = hflights[hflights$Month==1 & hflights$DayofMonth==1,]
# filter in dplyr
df_fl = filter(hflights, Month ==1 & DayofMonth ==1 & UniqueCarrier %in% c("AA","UA"))

# select in base R
df_fl_select = hflights[, c("DepTime","ArrTime","FlightNum")]
# select in dplyr
df_fl_select = select(hflights, DepTime, ArrTime, FlightNum, contains("delay"))

# dropping column

df_fl_select = select(hflights, -DepTime, -ArrTime, -FlightNum)

# mutate and arrange via pipe operator

df_fl %>%
  arrange(UniqueCarrier, desc(DepDelay)) %>% 
  mutate(speed = Distance/AirTime)

df_no_flights = hflights %>%
                  group_by(Dest) %>% 
                  summarize(flight_count =n(),   # n() count of observations
                            dist_flights = n_distinct(TailNum)
                          )

# lag function
mtcars %>% 
  group_by(c(cyl)) %>% 
  summarize(avg_mpg=mean(mpg)) %>% 
  mutate(lag_var=lag(avg_mpg,1))

# Spliting of dataset into test and train dataset is done using sample
# for e.g.
set.seed(1)
s = sample(1:nrow(hflights), 0.3*nrow(hflights))
df_test = hflights[s,]
df_train = hflights[-s,]

# bootstrapping, when we have to take more test and train sample then the available observations

# s = sample(1:nrow(hflights), 0.3*nrow(hflights), replace = T)

# to convert columns into rows use gather
# syntax: df %>% gather(label_header,key_header, column list)
# values in column list goes into label header and column values goes into key

# opposite of gather is spread
# syntax: df %>% spread(label_header, key_header)

# to split a column into multiple columns use seperate(column, sep=)
# to join two columns into 1, use unite()







