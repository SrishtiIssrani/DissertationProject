install.packages('caTools')
library(caTools)

# Importing the dataset
dataset = read.csv('DataPrepforClassification copy 3.csv')

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$Revenue.Earned, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
#Feature Scaling
training_set[, 2:12] = scale(training_set[, 2:12])
test_set[, 2:12] = scale(test_set[, 2:12])


#Fitting Multiple Linear Regression
regressor = lm(formula = Revenue.Earned ~ Budget + Polarity.Ratio + NumberofTheatre+ NumberofReplies + NumberofLikes + ActorRating,
               data = training_set)

#Predicting the Test Set Results
y_pred = predict(regressor, newdata = test_set)

