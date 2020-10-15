library(caret)
library(lattice)
library(ggplot2)
library(C50)
# data(churn)

# str(churnTrain)

# createDataPartition, createFolds, createMultfolds, createResamples

# table(churnTrain$churn)

data("iris")
set.seed(20)
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
              plot = "box", # ellipses, density, box, scatter
            ## Add a key at the top
             # auto.key = list(columns = 3)
            layout = c(4,1)
            )

# https://rdrr.io/cran/caret/man/createDataPartition.html
training_ind <- createDataPartition(iris$Sepal.Length, p=0.75, list = FALSE)
train_data <- iris[training_ind,]
test_data <- iris[-training_ind,]


# https://rdrr.io/cran/caret/man/trainControl.html
# Check above link for more option in trainControl
fitcontrol <- trainControl(method="repeatedcv",
                           repeats = 10,
                           number = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

model <- train(Species ~ ., 
               data = train_data,
               method = 'gbm',
               trControl = fitcontrol,
               verbose = FALSE,
               tuneGrid = gbmGrid
               )

model

p = predict(model, test_data)
confusionMatrix(p, test_data$Species
                )

plot(model)
