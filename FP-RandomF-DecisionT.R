library(dplyr)
attach(clean_dataset)
View(clean_dataset)

decisionT_dataset <- clean_dataset %>%
  mutate(race = case_when(
    `race:AfricanAmerican` == 1 ~ "AfricanAmerican",
    `race:Asian` == 1 ~ "Asian",
    `race:Caucasian` == 1 ~ "Caucasian",
    `race:Hispanic` == 1 ~ "Hispanic",
    `race:Other` == 1 ~ "Other",
    TRUE ~ "Unknown" # Optional: handle unexpected cases
  )) %>%
  # Drop the original race columns
  select(-`race:AfricanAmerican`, -`race:Asian`, -`race:Caucasian`, -`race:Hispanic`, -`race:Other`)

names(decisionT_dataset)

# Ensure 'race' is a factor
decisionT_dataset$race <- as.factor(decisionT_dataset$race)


decisionT_dataset <- decisionT_dataset %>%
  mutate(diabetes = case_when(
    diabetes == "0" ~ "No",
    diabetes == "1" ~ "Yes"
  ))
View(decisionT_dataset)
decisionT_dataset <- (na.omit(decisionT_dataset))
dim(decisionT_dataset)
attach(decisionT_dataset)
names(decisionT_dataset)
'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
#NULL MODELS

index=sample(1:nrow(decisionT_dataset),.8*nrow(decisionT_dataset))
trainingNULL=decisionT_dataset[index,]
testingNULL=decisionT_dataset[-index,]

nrow(trainingNULL)
nrow(testingNULL)
library(rpart)

# Ensure the outcome variable is a factor
trainingNULL$diabetes <- as.factor(trainingNULL$diabetes)

# Check the structure to confirm
str(trainingNULL$diabetes)

#install.packages("kernlab")
library(kernlab)
mod_null <- svm_linear(mode = "classification") |>
  set_engine("kernlab") |>
  fit(diabetes ~ 1, data = trainingNULL)

pred <- testingNULL %>%
  select(diabetes) %>%               
  bind_cols(
    predict(mod_null, new_data = testingNULL, type = "class")  
  ) %>%
  rename(diabetes_null = .pred_class)  

pred$diabetes <- as.factor(pred$diabetes)  # Assuming pred is a dataframe containing diabetes

accuracy(pred, truth = diabetes, estimate = diabetes_null)

confusion_null <- pred |>
  conf_mat(truth = diabetes, estimate = diabetes_null)
confusion_null


'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
#DECISION TREES
'1. Can we classify an individual as diabetic or non-diabetic based on specific health metrics and demographic data? 
'
#want to predict diabetic or non-diabetic status 

#split data

index=sample(1:nrow(decisionT_dataset),.8*nrow(decisionT_dataset))
trainingDT=decisionT_dataset[index,]
testingDT=decisionT_dataset[-index,]

nrow(trainingDT)
nrow(testingDT)

library(rpart)
?rpart

T=rpart(diabetes~ race + bmi + age + hbA1c_level + heart_disease ,data=trainingDT)
T

#visualize
#install.packages("rpart.plot")
library(rpart.plot)
?rpart.plot
rpart.plot(T)
head(predict(T,testingDT))

guesses=predict(T,testingDT, type = "class")
conf_matrix3=table(guesses,testingDT$diabetes)

accuracy <- sum(diag(conf_matrix3)) / sum(conf_matrix3)
accuracy

prop.table(table(guesses,testingDT$diabetes))

#how many Questions...
'T=rpart(Sex~.,data=training,maxdepth=1)
rpart.plot(T)'

'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
'RANDOM FOREST
'#VARIABLES used and depth() 

randomF_dataset <- decisionT_dataset %>%
  mutate(diabetes = case_when(
    diabetes == "No" ~ "0",
    diabetes == "Yes" ~ "1"
  ))
View(randomF_dataset)
randomF_dataset <- (na.omit(randomF_dataset))
dim(randomF_dataset)
attach(randomF_dataset)
names(randomF_dataset)

exclude_vars <- c("blood_glucose_level")
randomF_dataset <- randomF_dataset[, !names(randomF_dataset) %in% exclude_vars]
names(randomF_dataset)

#RANDOM FOREST...wisdom of the crowd

T1=rpart(diabetes~.,data=randomF_dataset)
T2=rpart(diabetes~ smoking_history  + bmi + hbA1c_level, data=randomF_dataset)
T3=rpart(diabetes~ race + age + blood_glucose_level + hypertension, data=randomF_dataset)
rpart.plot(T1)
rpart.plot(T2)
rpart.plot(T3)
summary(T2)

'plot(age,bmi,  col = ifelse(gender == "Female", "pink", "blue"))
'
#install.packages("randomForest")
library(randomForest)

?randomForest

randomF_dataset$diabetes <- as.factor(randomF_dataset$diabetes)

RF1=randomForest(diabetes~.,data=randomF_dataset,ntree=50)
RF1

#visualizing many trees via common questions...

#install.packages("randomForestExplainer")
library(randomForestExplainer)

?min_depth_distribution
min_depth_distribution(RF1)

plot_min_depth_distribution(min_depth_distribution(RF1))

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







