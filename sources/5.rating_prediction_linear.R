setwd("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

#Load the dataset
data <- read.csv("cleaned_rawdata.csv")

#Load libraries
library(dplyr)
library(ggplot2)

#Data Preprocessing 
#Find columns that have missing data
count_missing = function(df){
  sapply(df, FUN=function(col) sum(is.na(col)))
}
nacounts <- count_missing(data)
hasNA = which(nacounts > 0)
nacounts[hasNA]
#Treating missing data (by mean values of column)
library(vtreat)
treatment_plan <- design_missingness_treatment(data)
data_prepared <- prepare(treatment_plan, data)
nacounts <- count_missing(data_prepared)
hasNA = which(nacounts > 0)
nacounts[hasNA] #1: missing data, 0: no missing data

#Write clean data to the file
write.csv(data_prepared, "data_prepared.csv")

#Reload the cleaned file 
data_cleaned <- read.csv("data_prepared.csv")

#Remove outliers
boxplot(data_cleaned$rating)
Q1 <- quantile(data_cleaned$rating, 0.25)
Q3 <- quantile(data_cleaned$rating, 0.75)
IQR <- IQR(data_cleaned$rating)
new_data <- subset(data_cleaned, 
                   data_cleaned$rating > (Q1 - 1.5*IQR) & 
                     data_cleaned$rating < (Q3 + 1.5*IQR))
boxplot(new_data$rating)

sum(is.na(new_data$rating))

#Correlation between variables
numeric_columns <- unlist(lapply(new_data, is.numeric), use.names = FALSE)
cor(new_data[numeric_columns])


#Heatmap - correlation relationship
corr_matrix <- round(cor(new_data[numeric_columns]),2)
library(reshape2)
melted_cormat <- melt(corr_matrix)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + ggtitle("Correlation Heatmap") + #the number close to 1: related
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))

#Split dataset
set.seed(42)
gp <- runif(nrow(new_data))
dtrain <- subset(new_data, gp >= 0.5)
dtest <- subset(new_data, gp < 0.5)

#Linear model
summary(new_data)
#Fit a Multiple Linear Regression Model
model <- lm(rating ~ seniority + revenue + size + type + founded + avg_salary_estimate, data = dtrain)
summary(model)

#Prediction
dtrain$pred_rating <- predict(model, newdata = dtrain)
dtest$pred_rating <- predict(model, newdata = dtest) #we use the original model here

#R-sq/ rmse
rsq <- function(y, f) { 1 - sum((y - f)^2)/sum((y - mean(y))^2) }
rmse <- function(y, f) { sqrt(mean( (y - f)^2 )) }

rsq(dtrain$rating, dtrain$pred_rating)
rsq(dtest$rating, dtest$pred_rating) 

rmse(dtrain$rating, dtrain$pred_rating)
rmse(dtest$rating, dtest$pred_rating)


