library(car)

# Load the Bfox dataset
data("Bfox")


bfox <- Bfox
head(bfox)

# Split the data into train and test datasets
set.seed(123)
train_index <- sample(1:nrow(bfox), 0.7 * nrow(bfox))
train <- bfox[train_index, ]
test <- bfox[-train_index, ]

# Fit a supervised linear regression model
lm_model <- lm(debt ~ ., data = train)
summary(lm_model)

# Fit a KNN regression model
library(caret)
knn_model <- knnreg(debt ~ ., data = train, k = 5)
summary(knn_model)

# Predict the debt variable in the test data
lm_pred <- predict(lm_model, test)
knn_pred <- predict(knn_model, test)

# Get the fit indices of the predicted models
lm_r2 <- 1 - sum((test$debt - lm_pred)^2) / sum((test$debt - mean(test$debt))^2)
lm_mse <- mean((test$debt - lm_pred)^2)
lm_rmse <- sqrt(lm_mse)

knn_r2 <- 1 - sum((test$debt - knn_pred)^2) / sum((test$debt - mean(test$debt))^2)
knn_mse <- mean((test$debt - knn_pred)^2)
knn_rmse <- sqrt(knn_mse)

# Print the fit indices
print(paste("Linear regression R^2:", lm_r2))
print(paste("Linear regression MSE:", lm_mse))
print(paste("Linear regression RMSE:", lm_rmse))

print(paste("KNN regression R^2:", knn_r2))
print(paste("KNN regression MSE:", knn_mse))
print(paste("KNN regression RMSE:", knn_rmse))



#data()

