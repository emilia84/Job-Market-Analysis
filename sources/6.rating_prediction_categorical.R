setwd("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

#1.Load the dataset
data <- read.csv("cleaned_rawdata.csv")

#Select columns for rating analysis
selected_columns <- c('rating', 'seniority', 'revenue', 'size', 'type', 'founded', 'avg_salary_estimate')
# Create a new dataframe with the selected columns
new_data <- data[selected_columns]

# Check the type of the new dataset
str(new_data)

#Handle missing data in numerical & categorical values at the same time
library(vtreat)
treatmentZ = designTreatmentsZ(new_data, colnames(new_data), #apply to all columns
                               rareCount = 0) #disable rareCount occurence
data_treated <- prepare(treatmentZ, new_data)
summary(data_treated)

#Convert $rating to binary target variable
# Define the threshold for top-rated companies
table(data_treated$rating)
rating_threshold <- 4.0

# Create the binary target variable
data_treated$rating <- ifelse(data_treated$rating >= rating_threshold, "high_rating", "low_rating")

#Scaling dataset
library(caret)
process <- preProcess(data_treated, method=c("range"))
norm_cleaned_data <- predict(process, data_treated)
summary(norm_cleaned_data)

str(norm_cleaned_data)


#Split the dataset
library(dplyr)
set.seed(42)
training_obs <- norm_cleaned_data$rating %>% createDataPartition (p=0.7, list=FALSE) 

train <- norm_cleaned_data[training_obs, ]
test <- norm_cleaned_data[-training_obs, ]

str(train) #------------------------------------IF NOT DATAFRAME
#Make sure train is dataframe 
train$rating <- as.factor(train$rating)

#Create formula before modeling
labels <- c('high_rating', 'low_rating')
y <- c('rating')
#x = c('seniority', 'revenue', 'size', 'type', 'founded', 'avg_salary_estimate')

# Get the names of the normalized columns (excluding the dependent variable)
x <- colnames(norm_cleaned_data)[colnames(norm_cleaned_data) != y]

# Create the formula using mk_formula() with dynamic column names
library(wrapr)
fmla <- mk_formula(y, x)
print(fmla)

#----------------------MODELING
#1.Logistic
logit_model <- glm(fmla, data=train, family = binomial(link = "logit"))

train$pred_prob <- predict(logit_model, newdata = train, type = "response")
test$pred_prob <- predict(logit_model, newdata = test, type = "response")

train$pred <- ifelse(train$pred_prob > 0.5, labels[1], labels[2])
test$pred <- ifelse(test$pred_prob > 0.5, labels[1], labels[2])

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- c("logit")
accuracy_train <- c(as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- c(as.numeric(cfm_test$overall["Accuracy"]))
print(method)
print(accuracy_train)
print(accuracy_test)

#------->Perform ROC, cross validation to evaluate logistic model
library(pROC)
roc_score = roc(test$rating, test$pred_prob, positive = labels[1])
print(roc_score)
plot(roc_score, main ="ROC curve - Logistic Regression")

#2.Normalized KNN (K=5)
library(class)
norm_knn_model <- knn(train = train[,x], test = train[,x], cl = train[,y],
                      k=5)
cfm_train <- confusionMatrix(data = as.factor(norm_knn_model), 
                             reference = as.factor(train$rating), positive = labels[1])
norm_knn_model <- knn(train = train[,x], test = test[,x], cl = train[,y],
                      k=5)
cfm_test <- confusionMatrix(data = as.factor(norm_knn_model), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "Normalized KNN (K=5)")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#3.Decision Tree
library(rpart) #we use recursive partition method
tree_model <- rpart(fmla, method = "class", data=train)

plot(tree_model, uniform=TRUE, margin=0.1)
text(tree_model, use.n=TRUE, all=TRUE, cex=0.8)

train$pred <- predict(tree_model, newdata = train, type = "class")
test$pred <- predict(tree_model, newdata = test, type = "class")

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "Decision Tree")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#4.Decision Tree with complexity control 
#to avoid overfitting
tree_model <- rpart(fmla, method = "class", data=train, control = rpart.control(cp = 0.05))

plot(tree_model, uniform=TRUE, margin=0.1)
text(tree_model, use.n=TRUE, all=TRUE, cex=0.8)

train$pred <- predict(tree_model, newdata = train, type = "class")
test$pred <- predict(tree_model, newdata = test, type = "class")

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "Decision Tree with complexity control")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#5.Random Forest
library(randomForest)
rf_model <- randomForest(fmla, data=train)

train$pred <- predict(rf_model, newdata = train, type = "response")
test$pred <- predict(rf_model, newdata = test, type = "response")

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "Random Forest")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#6.SVM
library(e1071)
svm_model_linear_c01 <- svm(fmla, data=train, kernel="linear", cost=0.1, scale=FALSE)

train$pred <- predict(svm_model_linear_c01, newdata = train)
test$pred <- predict(svm_model_linear_c01, newdata = test)

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "SVM (Linear, C=0.1)")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#7.SVM (Linear, C=10)
svm_model_linear_c10 <- svm(fmla, data=train, kernel="linear", cost=10, scale=FALSE)

train$pred <- predict(svm_model_linear_c10, newdata = train)
test$pred <- predict(svm_model_linear_c10, newdata = test)

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "SVM (Linear, C=10)")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#8. SVM with Radial Basic Function
svm_model_radial_c10 <- svm(fmla, data=train, kernel="radial", cost=10, scale=FALSE)

train$pred <- predict(svm_model_radial_c10, newdata = train)
test$pred <- predict(svm_model_radial_c10, newdata = test)

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "SVM (Radial, C=10)")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#9.Naive Bayesian
library(class)
nb <- naiveBayes(fmla, data=train)

train$pred <- predict(nb, newdata = train)
test$pred <- predict(nb, newdata = test)

cfm_train <- confusionMatrix(data = as.factor(train$pred), 
                             reference = as.factor(train$rating), positive = labels[1])
cfm_test <- confusionMatrix(data = as.factor(test$pred), 
                            reference = as.factor(test$rating), positive = labels[1])
method <- append(method, "Naive Bayes")
accuracy_train <- append(accuracy_train, as.numeric(cfm_train$overall["Accuracy"]))
accuracy_test <- append(accuracy_test, as.numeric(cfm_test$overall["Accuracy"]))

data.frame(method, accuracy_train, accuracy_test)

#--------->Perform cross validation to improve logistic model 
train_control <- trainControl(method = "repeatedcv", number = 5)
model <- train(fmla, data = norm_cleaned_data, method = "glm", family = binomial, 
               trControl = train_control, metric = "Accuracy")
#train() is inside library(caret) #use the norm_cleaned_data for whole cross validation

#finally, get the accuracy
print(model)  #to get the accuracy value, the accuracy number is more representative than in confusionMatrix
#because it test the entire dataset, not only the testset

#see the result in each fold
model$resample

#calculate mean of all accuracy results
mean(model$resample$Accuracy)


