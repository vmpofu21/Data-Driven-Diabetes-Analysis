'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

#4. Multinomial Regression - How do race, age, location, smoking history, and gender influence the likelihood of having heart disease? - Vanessa

library(nnet)

clean2_dataset <- (na.omit(diabetes_dataset))
dim(clean2_dataset)
attach(clean2_dataset)
names(clean2_dataset)
View(clean2_dataset)


# Set seed for reproducibility
set.seed(123)

# Define the dataset
clean2_dataset <- clean2_dataset

# Get row indices for each label in your dataset
indexA <- sample(which(clean2_dataset$smoking_history == "current"), round(0.8 * length(which(clean2_dataset$smoking_history == "current"))))
indexB <- sample(which(clean2_dataset$smoking_history == "never"), round(0.8 * length(which(clean2_dataset$smoking_history == "never"))))
indexC <- sample(which(clean2_dataset$smoking_history == "former"), round(0.8 * length(which(clean2_dataset$smoking_history == "former"))))

# Combine indices for training
train_indices <- c(indexA, indexB, indexC)

# Create training dataset (80%)
trainML <- clean2_dataset[train_indices, ]

# Create testing dataset (remaining 20%)
test_indices <- setdiff(seq_len(nrow(clean2_dataset)), train_indices)
testML <- clean2_dataset[test_indices, ]

# View dimensions of training and testing datasets
dim(trainML) # Check dimensions of the training dataset
dim(testML)  # Check dimensions of the testing dataset

# Optionally, view datasets
View(trainML)
View(testML)




#use the multinom function from the nnet package to estimate 
# a multinomial logistic regression model.
library(nnet)
?nnet

mlog <- multinom(smoking_history ~ 1, data = trainML) #accuracy of 0.5467788

# View the summary of the null model
summary(mlog)

#need to get p-values from scratch
z = summary(mlog)$coefficients/summary(mlog)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
#predicted labels and corresponding probabilities
predict(mlog, newdata =testML,"probs") 
predict(mlog, newdata =testML) 
#table summary
table(predict(mlog, newdata =testML)==testML$smoking_history)
#predicted labels and corresponding probabilities
predicted_smoking <- predict(mlog, newdata = testML)
confusion_matrix <- table(testML$smoking_history, predicted_smoking)
confusion_matrix
table(predict(mlog, newdata =testML)==testML$smoking_history)
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy


mlog3 <- multinom(
  smoking_history ~`race:AfricanAmerican` + `race:Asian` +
    `race:Caucasian` + `race:Hispanic` + `race:Other` + bmi + blood_glucose_level +
    hypertension + heart_disease + diabetes, 
  data = trainML
)
#summaries and coeffiecents
summary(mlog3)
#need to get p-values from scratch
z = summary(mlog3)$coefficients/summary(mlog3)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
#predicted labels and corresponding probabilities
predict(mlog3, newdata =testML,"probs") 
predict(mlog3, newdata =testML) 
#table summary
table(predict(mlog3, newdata =testML)==testML$smoking_history)
#predicted labels and corresponding probabilities
predicted_smoking <- predict(mlog3, newdata = testML)
confusion_matrix <- table(testML$smoking_history, predicted_smoking)
confusion_matrix
table(predict(mlog3, newdata =testML)==testML$smoking_history)
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
######################################################################################################################################

'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'






