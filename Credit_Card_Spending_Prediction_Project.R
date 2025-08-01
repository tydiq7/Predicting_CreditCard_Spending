# upload database 
cc_data <- read.csv("C:\\Users\\Arav Kanyal\\Downloads\\archive\\BankChurners.csv")

# upload libraries
library(dplyr)
library(caTools)

#clean up data

  #remove unnecessary columns and values
View(cc_data)
names(cc_data)
cc_data <- cc_data[, -c(1:2, 4, 8:13, 16:20, 22:23)]
cc_data <- cc_data[cc_data$Education_Level != "Unknown",]
cc_data <- cc_data[cc_data$Marital_Status != "Unknown",]

  #consolidate excess dummy variable levels
cc_data$Marital_Status <- ifelse(cc_data$Marital_Status == "Married", "Married", "Single")
cc_data$Education_Level <- case_when(
  cc_data$Education_Level %in% c("Uneducated", "High School") ~ "No Degree",
  cc_data$Education_Level %in% c("College", "Graduate") ~ "Degree",
  cc_data$Education_Level %in% c("Post-Graduate", "Doctorate") ~ "Postgrad"
)

  #factor dummy variables
cc_data$Education_Level <- as.factor(cc_data$Education_Level)
is.factor(cc_data$Education_Level)
cc_data$Marital_Status <- as.factor(cc_data$Marital_Status)
is.factor(cc_data$Marital_Status)

#split data into train and test
set.seed(2)
train_data <- sample(nrow(cc_data), size = 0.7 * nrow(cc_data))
train <- cc_data[train_data, ]
test <- cc_data[-train_data, ]

#create linear regression model
reg_model <- lm(Avg_Utilization_Ratio ~., data = train)
summary(reg_model)

#predicting test data with model
predict <- predict(reg_model, test)
head(predict)

#comparing predicted data vs. actual data
plot(test$Avg_Utilization_Ratio, type = "l", lty = 1.8, col = "red")
lines(predict, type = "l", col = "blue")

#finding accuracy with root mean squared error
rmse <- sqrt(mean((test$Avg_Utilization_Ratio - predict)^2))
rmse

#compare to baseline rmse (prediction with mean of test data)
mean_val <- mean(test$Avg_Utilization_Ratio)
baseline_predict <- rep(mean_val, nrow(test))

# RMSE of baseline
baseline_rmse <- sqrt(mean((test$Avg_Utilization_Ratio - baseline_predict)^2))
baseline_rmse
