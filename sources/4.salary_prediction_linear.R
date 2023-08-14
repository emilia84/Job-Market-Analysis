setwd("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

#Load the dataset
data <- read.csv("cleaned_rawdata.csv")
library(dplyr)

#Change "temporary" to "contract" to avoid bias into spliting the dataset 
data$job.type[data$job.type %in% c("Temporary")] <- "Contract"

#Drop the job description column
data <- subset(data, select = -c(job.description))

summary(data$avg_salary_estimate)

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
#Confirm dataset with 0 NA
nacounts <- count_missing(data_prepared)
hasNA = which(nacounts > 0)
nacounts[hasNA] #1: missing data, 0: no missing data

#Write clean data to the file
write.csv(data_prepared, "data_prepared.csv")

#Reload the cleaned file 
data_cleaned <- read.csv("data_prepared.csv")

#Remove outliers
boxplot(data_cleaned$avg_salary_estimate)
Q1 <- quantile(data_cleaned$avg_salary_estimate, 0.25)
Q3 <- quantile(data_cleaned$avg_salary_estimate, 0.75)
IQR <- IQR(data_cleaned$avg_salary_estimate)
new_data <- subset(data_cleaned, 
                   data_cleaned$avg_salary_estimate > (Q1 - 1.5*IQR) & 
                     data_cleaned$avg_salary_estimate < (Q3 + 1.5*IQR))
boxplot(new_data$avg_salary_estimate)

sum(is.na(new_data$avg_salary_estimate))

#Correlation between variables
numeric_columns <- unlist(lapply(new_data, is.numeric), use.names = FALSE)
cor(new_data[numeric_columns])


#Heatmap - correlation relationship
corr_matrix <- round(cor(new_data[numeric_columns]),2)
library(reshape2)
library(ggplot2)
melted_cormat <- melt(corr_matrix)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + ggtitle("Correlation Heatmap") + #the number close to 1: related
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))

#Split dataset
set.seed(42)
gp <- runif(nrow(new_data))
dtrain <- subset(new_data, gp >= 0.5)
dtest <- subset(new_data, gp < 0.5)

#Create formula
library(wrapr)
y <- "avg_salary_estimate"
x <- c("size","type","revenue","seniority","job_type","company_age")
fmla <- mk_formula(y,x)

#Linear model
summary(new_data)
model <- lm(fmla, data = dtrain)
summary(model)


#Prediction
dtrain$pred_salary <- predict(model, newdata = dtrain)
dtest$pred_salary <- predict(model, newdata = dtest) #we use the original model here

#R-sq/ rmse
rsq <- function(y, f) { 1 - sum((y - f)^2)/sum((y - mean(y))^2) }
rmse <- function(y, f) { sqrt(mean( (y - f)^2 )) }

rsq(dtrain$avg_salary_estimate, dtrain$pred_salary)
rsq(dtest$avg_salary_estimate, dtest$pred_salary) 

rmse(dtrain$avg_salary_estimate, dtrain$pred_salary)
rmse(dtest$avg_salary_estimate, dtest$pred_salary)

#--------->Perform cross validation to improve regression model
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 5) 
model <- train(fmla, data = new_data, method = "lm", trControl = ctrl)

#calculate mean of all Rq, RMSE results
mean(model$resample$Rsquared)
mean(model$resample$RMSE)






