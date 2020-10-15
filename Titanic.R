library(dplyr)
library(car)

# load train and test dataset
df_train = data.frame(read.csv('Edvancer Data Science/R Code/datasets/titanic/train.csv', stringsAsFactors = F))
df_test = data.frame(read.csv('Edvancer Data Science/R Code/datasets/titanic/test.csv', stringsAsFactors = F))

summary(df_train)

# looking into the glimpse function, we can see that there are some columns with character type, 
# which needs to be converted into number
glimpse(df_train)

# Check for columns having missing values
col_names = colnames(df_train)

for(c in col_names){
  print(paste(c," :",sum(is.na(df_train[,c])==TRUE)))
}

#column Age is having missing values so lets impute it

table(df_train$Age)

mean_age = as.data.frame(aggregate(df_train$Age, by=list(df_train$Pclass, df_train$Sex), FUN=mean, na.rm=T))

for(i in 1:nrow(df_train)){
  if(is.na(df_train$Age[i])){
    for(j in 1:nrow(mean_age)){
      if (df_train$Pclass[i]==mean_age$Group.1[j] & df_train$Sex==mean_age$Group.2[j]){
      df_train$Age[i] = round(mean_age$x[j],2)}
    }
  }
}

# Convert the character columns into numeric
df_train$Sex = as.factor(df_train$Sex)
levels(df_train$Sex) = c(0,1)
df_train$Sex = as.numeric(df_train$Sex)
            
df_train$Embarked = as.factor(df_train$Embarked)
levels(df_train$Embarked) = c(0,1,2)
table(df_train$Embarked) # here it seems there are 2 records with Null Embarked. we can drop them.
df_train = subset(df_train, Embarked!="")
df_train$Embarked = as.numeric(df_train$Embarked)

str(df_train)

#draw initials from  the name
df_train$Name_Initial = unlist(lapply(df_train$Name, function(x){str_sub(trimws(str_split(x,",")[[1]][2]),1,str_locate(trimws(str_split(x,",")[[1]][2]), ". ")[,1]-1)}))

xtabs(~df_train$Name_Initial+df_train$Sex) 
# check distribution of initials across Sex attribute this is needed for grouping

df_train$Name_Initial[which(df_train$Name_Initial %in% ("Miss"))]=0
df_train$Name_Initial[which(df_train$Name_Initial %in% c("Mrs","Lady","Mlle","Mme","Ms","th"))]=1
df_train$Name_Initial[which(df_train$Name_Initial %in% ("Master"))]=3
df_train$Name_Initial[which(df_train$Name_Initial %in% c("Mr","Capt","Col","Don","Jonkheer","Major","Rev","Sir","Dr"))]=4

df_train$Name_Initial = as.numeric(df_train$Name_Initial)

# Remove Cabin as it has more blank values, Ticket as this does not add any value and Name
df_train = select(df_train,-c("Ticket","Cabin","Name"))

# lets combine Parch (Parent Child) and SibSp (sibling)
df_train$family = df_train$Parch + df_train$SibSp

df_train$family[which(df_train$family==0)]=0
df_train$family[which(df_train$family==1)]=1
df_train$family[which(df_train$family>1)]=2

df_train$family = as.numeric(df_train$family)

# drop Parch and sibsp
df_train = select(df_train, -Parch, -SibSp)
  
# Test Train split

n = sample(1:nrow(df_train), nrow(df_train)*0.7)
df_train.train = df_train[n,]
df_train.test = df_train[-n,]



linear_model = lm(Survived~.-PassengerId-Name_Initial, df_train.train)
sort(vif(linear_model), decreasing = T)
