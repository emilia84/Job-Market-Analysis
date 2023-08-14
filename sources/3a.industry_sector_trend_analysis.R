setwd("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

# Load libraries
library(ggplot2)

data <- read.csv("data_prepared.csv")
library(dplyr)

# Explore Data
summary(data)
str(data)

# Distribution of Companies across Industries and Sectors
ggplot(data, aes(x = industry)) + 
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Companies across Industries",
       x = "Industry",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = sector)) + 
  geom_bar(fill = "green") +
  labs(title = "Distribution of Companies across Sectors",
       x = "Sector",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify Most Common Industries/Sectors
most_common_industries <- table(data$industry) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head(10)
print(most_common_industries)

# Create a bar plot for the most common industries
ggplot(most_common_industries, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Most Common Industries",
       x = "Industry",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

most_common_sectors <- table(data$sector) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head(10)
print(most_common_sectors)

# Create a bar plot for the most common sectors
ggplot(most_common_sectors, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Most Common Sectors",
       x = "Sector",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
