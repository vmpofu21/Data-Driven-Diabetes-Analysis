'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

'NEAREST NEIGHBOR'

'knn_dataset <- decisionT_dataset[, sapply(decisionT_dataset, is.numeric)]
names(knn_dataset)
attach(knn_dataset)
View(knn_dataset)'

library(class)
library(kknn)
library(tidymodels)
'used the decisionT_dataset'
exclude_vars <- c("blood_glucose_level")
decisionT_dataset <- decisionT_dataset[, !names(decisionT_dataset) %in% exclude_vars]
names(decisionT_dataset)

knn_dataset_parts <- decisionT_dataset |>
  initial_split(prop = 0.8)

KNNtraining <- knn_dataset_parts |> training()
KNNtesting <- knn_dataset_parts |> testing()

names(KNNtraining)
names(KNNtesting)
str(KNNtraining$diabetes)
str(KNNtesting$diabetes)
KNNtraining <- KNNtraining |> mutate(diabetes = as.factor(diabetes))
KNNtesting <- KNNtesting |> mutate(diabetes = as.factor(diabetes))
levels(KNNtraining$diabetes)
levels(KNNtesting$diabetes)

#BOOK CODE
mod_knn <- nearest_neighbor(neighbors = 5, mode = "classification") |>
  set_engine("kknn", scale = TRUE) |>
  fit(diabetes ~ race + bmi + age + hbA1c_level + heart_disease, data = KNNtraining)

pred <- KNNtesting |>
  bind_cols(
    predict(mod_knn, new_data = KNNtesting, type = "class")) |>
  rename(diabetes_knn = .pred_class)

pred |>
  conf_mat(diabetes, diabetes_knn)

pred |>
  accuracy(diabetes, diabetes_knn)


#PETE'S   CODE

predict3= kknn(diabetes~race + bmi + age + hbA1c_level + heart_disease, train = KNNtraining, test=KNNtesting ,k=3, distance = 2)
summary(predict3)
guesses=fitted(predict3)
guesses
table(guesses,KNNtesting$diabetes)

# Confusion Matrix
conf_matrix <- table(guesses, KNNtesting$diabetes)
conf_matrix

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

#misclassidfication rate
misclassification_rate <- 1 - accuracy
misclassification_rate

library(ggplot2)
library(yardstick)
librar
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

#SVM
'Can we classify an individual as diabetic or non-diabetic based on their health metrics and demographic data?'
library(e1071)
?svm

attach(decisionT_dataset)
names(decisionT_dataset)

decisionT_dataset$diabetes <- as.factor(decisionT_dataset$diabetes)

m=svm(diabetes ~ bmi + hbA1c_level,data=decisionT_dataset)
m
summary(m)
plot(m, decisionT_dataset, bmi ~ hbA1c_level)
conf_matrix <- table(predict(m,decisionT_dataset),diabetes)
conf_matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
m$SV


lm=svm(diabetes ~ bmi + hbA1c_level,data=decisionT_dataset, kernel="linear")
plot(lm, decisionT_dataset, bmi ~ hbA1c_level)
conf_matrix2 <- table(predict(lm,decisionT_dataset),diabetes)
accuracy2 <- sum(diag(conf_matrix2)) / sum(conf_matrix2)
accuracy2
lm$SV

#SVM models with two predictors

plot(hbA1c_level, bmi, 
     col = ifelse(diabetes == "Yes", "red", "blue"), 
     pch = 19, 
     ylab = "BMI", 
     xlab = "hbA1c Levels", 
     main = "Scatter Plot of BMI vs hbA1c Levels")

# Add legend
legend("topright", legend = c("Diabetes: Yes", "Diabetes: No"), 
       col = c("red", "blue"), pch = 19)


m2=svm(diabetes ~ bmi + hbA1c_level,data=decisionT_dataset)
plot(m2, decisionT_dataset, bmi ~ hbA1c_level)

m3=svm(diabetes ~ bmi + hbA1c_level,data=decisionT_dataset, kernel="linear")
plot(m3, decisionT_dataset, bmi ~ hbA1c_level)







