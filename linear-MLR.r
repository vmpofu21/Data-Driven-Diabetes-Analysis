##THE DATA SET
install.packages("readr")
library(readr)
diabetes_dataset <- read_csv("Downloads/diabetes_dataset.csv")
View(diabetes_dataset)
attach(diabetes_dataset)
names(diabetes_dataset)

##LIBRARIES AND PACKAGES
library(corrplot)
library(scatterplot3d)
library(plotly)

'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
# DATA CLEANING
library(dplyr)

# Replace values using mutate and case_when
diabetes_dataset <- diabetes_dataset %>%
  mutate(smoking_history = case_when(
    smoking_history == "No Info" ~ NA,
    smoking_history == "ever" ~ "current",
    smoking_history == "not current" ~ "former",
    TRUE ~ smoking_history # Keep original value if no match
  ))

View(diabetes_dataset)
dim(diabetes_dataset)
dim(na.omit(diabetes_dataset))


# Clean dataset
clean_dataset <- (na.omit(diabetes_dataset))
dim(clean_dataset)

# Rename variables for easier handling
'colnames(clean_dataset)[colnames(clean_dataset) == "race:AfricanAmerican"] <- "race_AfricanAmerican"
colnames(clean_dataset)[colnames(clean_dataset) == "race:Asian"] <- "race_Asian"
colnames(clean_dataset)[colnames(clean_dataset) == "race:Caucasian"] <- "race_Caucasian"
colnames(clean_dataset)[colnames(clean_dataset) == "race:Hispanic"] <- "race_Hispanic"
colnames(clean_dataset)[colnames(clean_dataset) == "race:Other"] <- "race_Other"'

attach(clean_dataset)
names(clean_dataset)



'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

#####MODELS AND QUESTIONS

#1. Linear Regression - Is there a linear relationship between hbA1c_level and bmi? 

m1=lm(hbA1c_level~bmi)
m1
summary(m1)
cor(bmi, hbA1c_level)

?predict
#predict(m1)

confint(m1)
confint(m1,level=.99) ##explain these also

ggplot(clean_dataset, aes(x = hbA1c_level, y = bmi)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "HbA1c vs BMI",
       x = "HbA1c",
       y = "BMI") 


'Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.1586679  0.0192823  267.53   <2e-16 ***
bmi         0.0142700  0.0006612   21.58   <2e-16 ***
Multiple R-squared:  0.007204,	Adjusted R-squared:  0.007189 
'

'• Intercept - when bmi is 0 (theoretical, not practical), the expected HbA1c level is 5.16.
 • bmi - for every 1-unit increase in bmi, HbA1c increases by approximately 0.0143 units, holding other factors constant.
 • The bmi coefficient is highly statistically significant (p < 0.001), meaning bmi is strongly associated 
   with HbA1c levels. However, the small coefficient suggests bmi has a relatively weak individual effect on HbA1c.
 • The model explains only 0.72% of the variance in HbA1c levels, this is very low, indicating that bmi 
   alone is not a strong predictor of hbA1c_level.
'
'INTERPRETATION'
'While bmi is a statistically significant predictor of HbA1c levels, it explains only a 
small fraction of the variability in HbA1c. This suggests other factors 
(e.g., age, blood_glucose_level) are likely more important in predicting HbA1c levels.
The relationship is positive but weak, as seen by the small coefficient for bmi'


'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'

#2. Multiple Regression - Which variables provide the best prediction of diabetes risk?
# Obviously blood_glucose_level does, so we take it out immediately

multi_reg1 <- lm(data=clean_dataset,diabetes~. -(blood_glucose_level) ) #Adjusted R-squared:  0.2733 
summary(multi_reg1)
drop1(multi_reg1,test="F")

summary(lm(data=clean_dataset,diabetes~.-(blood_glucose_level+location))) #Adjusted R-squared:  0.2733  
summary(lm(data=clean_dataset,diabetes~.-(blood_glucose_level+location+year))) #Adjusted R-squared:  0.2733 
summary(lm(data=clean_dataset,diabetes~.-(blood_glucose_level+location+year+`race:AfricanAmerican`+
             `race:Asian` + `race:Caucasian`+ `race:Hispanic` + `race:Other`+smoking_history))) #Adjusted R-squared:  0.2733 
summary(lm(data=clean_dataset,diabetes~.-(blood_glucose_level+location+year+`race:AfricanAmerican`+
       `race:Asian` + `race:Caucasian`+ `race:Hispanic` + `race:Other`+gender+smoking_history))) #Adjusted R-squared:  0.2724

summary(lm(data=clean_dataset,diabetes~.-(blood_glucose_level+location+year+`race:AfricanAmerican`+
  `race:Asian` + `race:Caucasian`+ `race:Hispanic` + `race:Other`+gender+smoking_history+age)))#Adjusted R-squared:  0.253 


table(clean_dataset$location)
table(clean_dataset$`race:Other`)
table(clean_dataset$smoking_history)


'CONCLUSION'
'• age, hypertension, heart disease, bmi, hbA1c_level, and blood_glucose_level  are significant predictors of 
diabetese because they have a p-value of <0.001.
 • The model explains approximately 39% of the variability in diabetes, indicating that while these predictors are significant, 
other unmeasured factors also contribute to diabetes risk.
 • The results from this analysis helps implement preventive health strategies such as targeted screening for older adults, and individuals
with higher bmi or heart_disease:
          • Older adults: Age is strongly associated with diabetes risk, so screening programs might prioritize this group.
          • Individuals with higher BMI: Since BMI is significant, promoting healthy weight and active lifestyles can help reduce risk.' 


'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'


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






