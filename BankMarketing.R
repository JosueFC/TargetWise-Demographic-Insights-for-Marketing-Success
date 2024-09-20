#Load packages
library(readr)
library(ggplot2)
library(dplyr)

#Load data
BankData <- read.csv("C:/Users/Josue/Downloads/archive/bank-additional-full.csv", sep=";")

#Data Exploration
hist(BankData$age, main="Distribution of Age", xlab="Age", col="lightblue")

#Exploratory analysis of categorical data: 
ggplot(BankData, aes(x=job)) + geom_bar(fill="lightblue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Job Distribution")
ggplot(BankData, aes(x=education)) + geom_bar(fill="lightgreen") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education Distribution")

#Calculation of the response rate
response_rate <- BankData %>%
  group_by(contact, day_of_week) %>%
  summarise(total = n(),                # Count total contacts
            positive_responses = sum(y == "yes"),  # Count positive responses
            response_rate = positive_responses / total)  # Calculate response rate

#Create a bar chart for response rate by day of the week and contact method
ggplot(response_rate, aes(x=day_of_week, y=response_rate, fill=contact)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Response Rate by Contact Method and Day of Week",
       x="Day of the Week",
       y="Response Rate") +
  theme_minimal()

# Group by job type and calculate response rate
job_response_rate <- BankData %>%
  group_by(job) %>%
  summarise(total_contacts = n(),                         # Total contacts per job type
            positive_responses = sum(y == "yes"),   # Count of positive responses
            response_rate = positive_responses / total_contacts)  # Response rate

# Bar plot of response rate by job type
ggplot(job_response_rate, aes(x=reorder(job, -response_rate), y=response_rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Response Rate by Job Type", x="Job Type", y="Response Rate") +
  theme(axis.text.x = element_text(angle=90, hjust=1))  # Rotate x-axis labels for readability

#Chi square
job_response_table <- table(BankData$job, BankData$y)
chisq.test(job_response_table)

#Regression
# Convert response to binary (1 for 'yes', 0 for 'no')
BankData$response_binary <- ifelse(BankData$y == "yes", 1, 0)

# Ensure demographic variables are factors (if they are not already)
BankData$job <- as.factor(BankData$job)
BankData$marital <- as.factor(BankData$marital)
BankData$education <- as.factor(BankData$education)
BankData$default <- as.factor(BankData$default)
BankData$housing <- as.factor(BankData$housing)
BankData$loan <- as.factor(BankData$loan)
BankData$contact <- as.factor(BankData$contact)
BankData$day_of_week <- as.factor(BankData$day_of_week) 

# Build the logistic regression model with demographic factors
logit_model <- glm(response_binary ~ age + job + marital + education + default + housing + loan + contact + day_of_week,
                   data = BankData, family = binomial)

# Plot predicted probabilities vs age, colored by job
data$predicted_response <- predict(logit_model, type = "response")

# Define custom colors for jobs, with specific colors for students and retired
custom_colors <- c("student" = "red",          # Highlight students in red
                   "retired" = "blue",         # Highlight retired in blue
                   "blue-collar" = "gray70",   # Muted colors for others
                   "entrepreneur" = "gray70",
                   "housemaid" = "gray70",
                   "management" = "gray70",
                   "self-employed" = "gray70",
                   "services" = "gray70",
                   "technician" = "gray70",
                   "unemployed" = "gray70",
                   "unknown" = "gray70")

# Scatter plot of age vs predicted probability, with custom colors for job
ggplot(data, aes(x = age, y = predicted_response, color = job)) +
  geom_point() +
  scale_color_manual(values = custom_colors) +  # Apply the custom colors
  labs(title = "Predicted Probability of Positive Response by Age and Job",
       x = "Age", y = "Predicted Probability of Positive Response") +
  theme_minimal()
