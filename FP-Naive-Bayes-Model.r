clean_dataset <- (na.omit(diabetes_dataset))
dim(clean_dataset)
attach(clean_dataset)
names(clean_dataset)

install.packages("e1071")
#install.packages("e1071",dep=TRUE)

library(e1071)
help(naiveBayes)

#bloodGlucoseLevel = BGL
exclude_vars <- c("blood_glucose_level")
no_BGL_dataset <- clean_dataset[, !names(clean_dataset) %in% exclude_vars]
names(no_BGL_dataset)



##NAIVE BAYES
"Given that a patient has a higher bmi, has a heart disease and they are older, what is the probability that the patient actually has diabetes?"
'1. Whats the probability that a person has diabetes given that they have a bmi >=28.4 , heart_disease = 1, age>=47'

#Split into testing and training data
index=sample(1:nrow(no_BGL_dataset),.8*nrow(no_BGL_dataset))
train=no_BGL_dataset[index,]
dim(train)

test=no_BGL_dataset[-index,]
dim(test)

#classify diabetes
model=naiveBayes(train,train$diabetes)
model


#Check certain conditional probabilities bmi, heart_disease and age:

table(train$diabetes)
prop.table(table(train$diabetes))

#CHECKS with model
model$apriori

sum(test$diabetes=="0")
sum(train$diabetes=="1")

sum(train$diabetes=="0")/nrow(train)
sum(train$diabetes=="1")/nrow(train)

'PROBABILITIES'
'
isDiabetic = 1 -> isD
notDiabetic = 0 -> notD
bmi >= 28.4 -> B
heart_disease = 1 -> HD
age >= 47 ->  A'

#P(isD)
sum(train$diabetes=="1")/nrow(train)
#P(notD)
sum(train$diabetes=="0")/nrow(train)


#P(isD | B)
sum(train$diabetes=="1" & train$bmi>=28.4 )/sum(train$bmi>=28.4 )
#P(notD | B)
sum(train$diabetes=="0" & train$bmi>=28.4 )/sum(train$bmi>=28.4 )


#P(isD | B | HD)
sum(train$diabetes=="1" & train$bmi>=28.4 & train$heart_disease =="1")/sum(train$bmi>=28.4 & train$heart_disease =="1")
#P(notD | B | HD)
sum(train$diabetes=="0" & train$bmi>=28.4 & train$heart_disease =="1")/sum(train$bmi>=28.4 & train$heart_disease =="1")


#P(isD | B | HD | A)
sum(train$diabetes=="1" & train$bmi>=28.4 & train$heart_disease =="1" & train$age >= 47 )/sum(train$bmi>=28.4  & train$heart_disease =="1" & train$age >= 47)
#P(notD | B | HD | A)
sum(train$diabetes=="0" & train$bmi>=28.4 & train$heart_disease =="1" & train$age >= 47 )/sum(train$bmi>=28.4  & train$heart_disease =="1" & train$age >= 47)


#LOOK AT TEST DATA:

dim(test)

predict(model,test)

#How many correct
table(predict(model,test)==test$diabetes)

#confusion matrix
table(predict(model,test),test$diabetes)

#prop correct...classification rate
table(predict(model,test)==test$diabetes)/length(test$diabetes)

probabilities <- predict(model, test, type = "raw")
predictions_custom <- ifelse(probabilities[, "1"] > 0.4, 1, 0)
table(predictions_custom, test$diabetes)

#TOGETHER
'(sum(train$diabetes=="1"&train$bmi>=28.4)/sum(train$diabetes=="1"))*(sum(train$diabetes=="1")/nrow(train))/(sum(train$bmi>=28.4)/nrow(train))
'

'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

'1. Whats the probability that a person has diabetes given that: smoking_history , race and location'


'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'





