library(caret)
library(lattice)
library(ggplot2)
library(dplyr)
library(doParallel)

C1 = makePSOCKcluster(3)
registerDoParallel(C1)

# load train and test dataset
df_train = data.frame(read.csv('Edvancer Data Science/R Code/datasets/titanic/train.csv', stringsAsFactors = F))
df_test = data.frame(read.csv('Edvancer Data Science/R Code/datasets/titanic/test.csv', stringsAsFactors = F))

df_train$type = "train"
df_test$type ="test"
df_test$Survived = ""

col = sort(names(df_train))

df_combined = rbind(df_train[, col], df_test[, col])

# Looking into the str() Embarked has 2 missing values so lets impute it with Mode
df_combined[which(df_combined$Embarked==""), "Embarked"] = "S"
table(df_combined$Embarked
      )

df_combined$Embarked = as.factor(df_combined$Embarked)
df_combined$Sex = as.factor(df_combined$Sex)

# check the structure
str(df_combined)

# dummy variable creation for factor datatypes

df_combined$abb_name = trimws(substr(df_combined$Name, regexpr("[,]", df_combined$Name)+1, 
                                     regexpr("[.]", df_combined$Name)))

df_combined$abb_name[which(df_combined$abb_name %in% 
                             c("Capt.", "Col.","Capt.","Col.","Don.","Major.","Jonkheer.","Rev.","Sir.", "Dr."))]= "Mr."

df_combined$abb_name[which(df_combined$abb_name %in% 
                             c("Lady.","Miss.","Ms.","the Countess.","Mlle.","Mme.","Dona."))] = "Mrs."

df_combined$Cabin[which(df_combined$Cabin =="")] = "U"

df_combined$Cabin = substr(df_combined$Cabin,1,1)

dummy_model = dummyVars(Survived ~ Embarked + Sex + abb_name + Cabin, data = df_combined, sep = ".",
                        levelsOnly = FALSE,
                        fullRank = TRUE
                        )

df_combined = cbind(df_combined, predict(dummy_model, newdata = df_combined))

df_combined$Family = df_combined$Parch + df_combined$SibSp + 1

df_combined = select(df_combined, 
                     -c("Embarked","Sex","abb_name","Ticket","Cabin","Name","Parch","SibSp"))


str(df_combined)

# remove name


df_train = subset(df_combined, type=="train")
df_test = subset(df_combined, type=="test")

df_train$type = NULL
df_test$type = NULL
df_test$Survived = NULL

df_train$Survived = as.numeric(df_train$Survived)

pp_model = preProcess(df_train[, !(names(df_train) %in% c("Survived"))], method = "knnImpute") # bagImpute

df_transformed = predict(pp_model, newdata = df_train[, !(names(df_train) %in% c("Survived"))])

df_transformed$Survived = as.factor(df_train$Survived)

train_index = createDataPartition(df_transformed$Survived, p= 0.75, list = FALSE)

df_transformed_train = df_transformed[train_index, ]
df_transformed_test = df_transformed[-train_index, ]

fitControl = trainControl(method = "repeatedcv", repeats = 5, number = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

model = train(Survived ~ ., 
              data=df_transformed_train,
              method = 'rf', # treebag, gbm
              trControl = fitControl,
              # tuneGrid = gbmGrid,
              verbose = FALSE
              )

model

stopCluster(C1)

p = predict(model, df_transformed_test)

confusionMatrix(p, df_transformed_test$Survived)
