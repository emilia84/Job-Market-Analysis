setwd("C:/Users/Emilia Hoang/Desktop/p2_Emilia_Hoang/data")

#1.Load the dataset
data <- read.csv("joblisting.csv")

#2.Show the summary of the dataset.
summary(data)

#3.Remove the 'index' column
data$X <- NULL

#4.Loop through each column in the dataframe
for (col in names(data)) {
  # Replace -1 values with 'N/A' in the current column
  data[, col][data[, col] == -1] <- NA
}


#COMPANY
# Split the 'Company Name' column at the newline character and keep only the first part (Company Name)
data$company.name <- sapply(strsplit(data$company, "\n"), `[`, 1)
# Store the remaining part (rating) of the split into the 'rating' column
data$rating <- sapply(strsplit(data$company, "\n"), `[`, 2)
# Drop the 'company' column
data <- subset(data, select = -company)

# Swap misunderstanding data
new_dataset <- subset(data, company.name %in% c("Intuit - Data", "Instacart - Corporate"))
new_dataset$job.title <- new_dataset$headquarters
new_dataset$headquarters <- new_dataset$salary.estimate
new_dataset$salary.estimate <- NA

# Drop duplicates
data <- data[!(data$company.name %in% c("Intuit - Data", "Instacart - Corporate")), ]
merged_data <- rbind(data, new_dataset)
write.csv(merged_data, "merged_dataset.csv", row.names = FALSE)


#HEADQUARTERS
merged_data$headquarters <- gsub(" City", "", merged_data$headquarters)
merged_data$headquarters <- gsub('United States', 'CA', merged_data$headquarters)
# Split 'Headquarters' into 'HQ_city' and 'HQ_state' columns
merged_data <- transform(merged_data, HQ_city = sub(",.*", "", headquarters),
                  HQ_state = sub(".*,(.*)", "\\1", headquarters))
merged_data$HQ_city <- gsub('CA', NA, merged_data$HQ_city)
# Drop the original 'Headquarters' and 'Location' columns
merged_data <- subset(merged_data, select = -headquarters)


#FOUNDED
# Calculate the current year
current_year <- as.integer(format(Sys.Date(), "%Y"))
# Calculate the Company_age by subtracting 'Founded' year from the current year
merged_data$company.age <- current_year - merged_data$founded


#JOB TYPE vs Company TYPE
# Remove the prefix 'company - ' from the 'type' column
merged_data$job.type <- gsub("^Job Type : ", "", merged_data$job.type)
merged_data$type <- gsub("^company - ", "", merged_data$type)
merged_data$job.type <- gsub("N/A", NA, merged_data$job.type)


#SIZE #Note: split similar to salary
# Remove ' employees' from the 'Size' column
merged_data$size <- gsub(" employees", "", merged_data$size)
# Replace ' to ' with a hyphen ('-') in the 'Size' column
merged_data$size <- gsub(" to ", "-", merged_data$size)
# Replace "unknown" with "N/A" in the 'size' column
merged_data$size[merged_data$size == "unknown"] <- NA
#
merged_data$size <- gsub("1-50", NA, merged_data$size)
table(merged_data$size)
sum(is.na(merged_data$size))



#REVENUE #Note: split similar to salary
# Replace 'Unknown / Non-Applicable' with 'N/A' in the 'Revenue' column
merged_data$revenue <- gsub('unknown / non-applicable', NA, merged_data$revenue)
# Remove 'USD' from the 'Revenue' column
merged_data$revenue <- gsub('usd', '', merged_data$revenue)
# Remove parentheses and other special characters from the 'Revenue' column
merged_data$revenue <- gsub('[()]', '', merged_data$revenue)
# Replace 'to' with a hyphen '-' in the 'Revenue' column
merged_data$revenue <- gsub(' to ', '-', merged_data$revenue)


#TITLE
# Remove the specified strings from the 'job.title' column
merged_data$job.title <- gsub('\\[Active\\]|\\[Expired\\]|\\[DA\\]|\\(USA\\)|\\s*\\(.*\\)', '', merged_data$job.title)
# Create the 'Seniority' column using the presence of 'Senior' in 'Job Title'
merged_data$seniority <- ifelse(grepl("\\b(?:Sr\\.?|Senior)\\b", merged_data$job.title, 
                               ignore.case = TRUE), "Senior", "Junior")


#SALARY
# Remove the '(Glassdoor est.)' from 'Salary Estimate' column
merged_data$salary.estimate <- gsub(" \\(Glassdoor est.\\)", "", merged_data$salary.estimate)
merged_data$salary.estimate <- gsub("Employer Provided Salary:", "", merged_data$salary.estimate)
# Split 'Salary Estimate' into 'min_salary_estimate' and 'max_salary_estimate' columns
salary_split <- strsplit(merged_data$salary.estimate, "-")
merged_data$min_salary_estimate <- sapply(salary_split, `[`, 1)
merged_data$max_salary_estimate <- sapply(salary_split, `[`, 2)
# Remove any non-digit characters from 'Min_salary_estimate' and 'Max_salary_estimate' columns
merged_data$min_salary_estimate <- gsub("\\D", "", merged_data$min_salary_estimate)
merged_data$max_salary_estimate <- gsub("\\D", "", merged_data$max_salary_estimate)
# Convert 'min_salary_estimate' and 'max_salary_estimate' to numeric values
merged_data$min_salary_estimate <- as.numeric(merged_data$min_salary_estimate)
merged_data$max_salary_estimate <- as.numeric(merged_data$max_salary_estimate)
# Multiply 'min_salary_estimate' and 'max_salary_estimate' by 1000
merged_data$min_salary_estimate <- merged_data$min_salary_estimate * 1000
merged_data$max_salary_estimate <- merged_data$max_salary_estimate * 1000
# Drop the original salary.estimate
merged_data <- subset(merged_data, select = -salary.estimate)
# Calculate the average salary estimate and store it in 'Avg_salary_estimate' column
merged_data$avg_salary_estimate <- (merged_data$min_salary_estimate + merged_data$max_salary_estimate) / 2
# Convert 'avg_salary_estimate' to integer type
merged_data$avg_salary_estimate <- as.integer(merged_data$avg_salary_estimate)


write.csv(merged_data, "cleaned_rawdata.csv", row.names = FALSE)

