#a
data("Arrests")
set.seed(07)

train_indices <- sample(1:nrow(Arrests), 0.8 * nrow(Arrests))

train_data <- Arrests[train_indices, ]
test_data <- Arrests[-train_indices, ]


train_data$released <- factor(train_data$released)
test_data$released <- factor(test_data$released, levels = levels(train_data$released))

logreg_model <- glm(released ~ colour + age + sex + employed + citizen, data = train_data, family = binomial)

#b
Fit Naive Bayes model
nb_model <- naive_bayes(released ~ colour + age + sex + employed + citizen, data = train_data)

logreg_pred <- predict(logreg_model, newdata = train_data, type = "response")
logreg_pred <- ifelse(logreg_pred > 0.5, "Yes", "No")

logreg_cm <- confusionMatrix(logreg_pred, train_data$released)
logreg_sensitivity <- logreg_cm$byClass[["Sensitivity"]]
logreg_specificity <- logreg_cm$byClass[["Specificity"]]

nb_pred <- predict(nb_model, newdata = train_data, type = "raw")
nb_pred <- ifelse(nb_pred$Yes > nb_pred$No, "Yes", "No")

nb_cm <- confusionMatrix(nb_pred, train_data$released)
nb_sensitivity <- nb_cm$byClass[["Sensitivity"]]
nb_specificity <- nb_cm$byClass[["Specificity"]]

#c

test_logreg_pred <- predict(logreg_model, newdata = test_data, type = "response")
test_logreg_pred <- ifelse(test_logreg_pred > 0.5, "Yes", "No")

test_nb_pred <- predict(nb_model, newdata = test_data, type = "raw")
test_nb_pred <- ifelse(test_nb_pred$Yes > test_nb_pred$No, "Yes", "No")

test_logreg_cm <- confusionMatrix(test_logreg_pred, test_data$released)
test_logreg_sensitivity <- test_logreg_cm$byClass[["Sensitivity"]]
test_logreg_specificity <- test_logreg_cm$byClass[["Specificity"]]

test_nb_cm <- confusionMatrix(test_nb_pred, test_data$released)
test_nb_sensitivity <- test_nb_cm$byClass[["Sensitivity"]]
test_nb_specificity <- test_nb_cm$byClass[["Specificity"]]
