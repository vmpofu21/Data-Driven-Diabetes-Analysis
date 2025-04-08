'------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
#LOGISTIC REGRESSION
#split data
names(decisionT_dataset)
#decisionT_dataset <- subset(decisionT_dataset, select = -bmi_cat)
colnames(decisionT_dataset)  # Should no longer include "bmi_cat"

index=sample(1:nrow(decisionT_dataset),.8*nrow(decisionT_dataset))
trainLOG=decisionT_dataset[index,]
dim(trainLOG)

testLOG=decisionT_dataset[-index,]
dim(testLOG)
View(testLOG)

m=glm(data=trainLOG,diabetes~ bmi + age + hbA1c_level + heart_disease,,family="binomial")
summary(m)
#logodds
predict(m,newdata = testLOG)

#probability
p=predict(m,newdata = testLOG,type="response")
p
#confusion matrix

table(testLOG$diabetes,ifelse(p>=.5,"Yes","No"))

mean(testLOG$diabetes==ifelse(p>=.5,"Yes","No"))

library(ggplot2)
library(caret)

# Generate the confusion matrix
conf_mat <- table(testLOG$diabetes, ifelse(p >= 0.5, "Yes", "No"))

# Convert the matrix into a data frame for ggplot
conf_mat_df <- as.data.frame(as.table(conf_mat))
colnames(conf_mat_df) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix
# Add a new column to indicate correct or incorrect classification
conf_mat_df$Classification <- ifelse(conf_mat_df$Actual == conf_mat_df$Predicted, "Correct", "Incorrect")

# Plot the confusion matrix with different colors for correct and incorrect classifications
ggplot(conf_mat_df, aes(x = Predicted, y = Actual, fill = Classification)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), size = 5, color = "white") +
  scale_fill_manual(values = c("Correct" = "lightblue", "Incorrect" = "maroon")) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
  theme_minimal()
