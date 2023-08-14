setwd("C:/Users/Emilia Hoang/Desktop/p2_emilia_hoang/data")

#Load the dataset 
data <- read.csv("data_prepared.csv")

#SENIORITY
library(ggplot2)
ggplot(data = data, aes(x = seniority)) +
  geom_bar() +
  labs(x = "Job seniority", y = "Number of job offers", title = "Number of job offers per Seniority") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot") 

ggplot(data = data, aes(x = reorder(sector, -table(sector)[sector]), fill = seniority)) +
  geom_bar(position = "dodge") +
  labs(x = "Sector", y = "Number of job offers", title = "Number of job offers per Seniority and Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title = "Seniority"))

#SALARY
library(dplyr)
grouped_data <- data %>%
  group_by(sector) %>%
  summarise(avg_salary_mean = mean(avg_salary_estimate, na.rm = TRUE))

ggplot(data = grouped_data, aes(x = reorder(sector, avg_salary_mean), y = avg_salary_mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Sector", y = "Average estimate salary", title = "Average estimate salary per Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot") +
  scale_y_continuous(labels = scales::comma)

ggplot(data = data, aes(x = avg_salary_estimate, y = rating, color = seniority)) +
  geom_point() +
  labs(x = "Average estimate salary", y = "Rating", title = "Correlation between the average salary and the company rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot")

ggplot(data = data, aes(x = avg_salary_estimate, fill = type)) +
  geom_histogram(binwidth = 5000) +
  labs(x = "Average estimate salary", y = "Number of job offers", title = "Distribution of the average estimate salary per type of ownership") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot")

ggplot(data = data, aes(x = size, y = avg_salary_estimate)) +
  geom_boxplot() +
  labs(x = "Company size", y = "Average estimate salary", title = "Distribution of the average salary in each company size") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot")

#COMPANY SIZE
ggplot(data = data, aes(x = size)) +
  geom_bar() +
  labs(x = "Company size", y = "Number of job offers", title = "Number of job offers per Company size") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot")

ggplot(data = data, aes(x = reorder(sector, -table(sector)[sector]), fill = job_type)) +
  geom_bar(position = "dodge") +
  labs(x = "Sector", y = "Number of job offers", title = "Number of job offers per Job Type and Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title = "Job Type"))



