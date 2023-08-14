setwd("C:/Users/Eirlys Hoang/Desktop/p2_Emilia_Hoang/data")

# Load the dataset 
data <- read.csv("cleaned_rawdata.csv")

library(dplyr)
library(ggplot2)

#Get dataset summary
summary(data)

# Descriptive Statistics
ratings_summary <- data %>%
  group_by(size, industry, HQ_city) %>%
  summarise(
    Mean_Rating = mean(rating),
    Median_Rating = median(rating),
    SD_Rating = sd(rating),
    Count = n()
  )
print(head(ratings_summary, 10))

# Calculate summary statistics for Salary Estimate
mean(data$avg_salary_estimate, na.rm = TRUE)
median(data$avg_salary_estimate, na.rm = TRUE)
sd(data$avg_salary_estimate, na.rm = TRUE)

# Calculate summary statistics for Company Age
mean(data$company.age, na.rm = TRUE)
median(data$company.age, na.rm = TRUE)
sd(data$company.age, na.rm = TRUE)

# Calculate summary statistics for Salary Estimate
summary(data$avg_salary_estimate)

# Calculate summary statistics for Company Size
summary(data$size)
summary(data$company.age)

