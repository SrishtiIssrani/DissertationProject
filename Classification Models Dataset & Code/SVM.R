install.packages('caTools')
install.packages('e1071')
install.packages('caret')
install.packages('randomForest')
library(caTools)
library(e1071)
library(caret)
library(randomForest)
dataset = read.csv('DataPrepforClassification copy 3.csv')
dataset = dataset[3:14]

#Encoding Categorical Data Verdict
dataset$Verdict = factor(dataset$Verdict,
                         levels = c('Flop', 'Hit'),
                         labels = c(0,1))
dataset$CBFCRating = factor(dataset$CBFCRating,
                            levels = c('U', 'UA', 'A'),
                            labels = c(1,2,3))

# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(dataset$Verdict, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[, 1:10] = scale(training_set[, 1:10])
test_set[, 1:10] = scale(test_set[, 1:10])

#creating the classifier
classifier = svm(formula = Verdict ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

#making the predictions on the test set
y_pred = predict(classifier, newdata = test_set[-12])

#creating the confusion matrix
cm <-confusionMatrix(y_pred, test_set$Verdict)


fourfoldplot(cm$table)

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Hit', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Flop', cex=1.2)
  text(125, 370, 'Actual', cex=1.3, srt=90, font=2)
  text(245, 450, 'Predicted', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Hit', cex=1.2, srt=90)
  text(140, 335, 'Flop', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[4], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[1], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
draw_confusion_matrix(cm)

