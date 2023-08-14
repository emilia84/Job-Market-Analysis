("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

data <- read.csv("data_prepared.csv")

# Bar plots comparing revenue distribution 
ggplot(data = data, aes(x = type, fill = revenue)) +
  geom_bar() +
  labs(x = "Company Type", y = "Number of job offers", title = "Revenue Distribution across Company Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar plots comparing job types 
ggplot(data = data, aes(x = type, fill = job_type)) +
  geom_bar(position = "stack") +
  labs(x = "Company Type", y = "Number of job offers", title = "Distribution of Job Types across Company Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Box plot to visualize ratings across different categories of company size
ggplot(data, aes(x = size, y = rating)) +
  geom_boxplot() +
  labs(title = "Company Ratings by Company Size",
       x = "Company Size",
       y = "Company Rating")
