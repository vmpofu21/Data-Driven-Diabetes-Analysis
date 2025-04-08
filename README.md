# Data-Driven-Diabetes-Analysis

## Project Overview

This project explores a diabetes dataset from Kaggle to identify relationships between various health indicators and the onset of diabetes. The goal was to analyze meaningful data to gain insights into factors contributing to diabetes risk and build predictive models. Initially, we considered questions about the likelihood of diabetes based on gender, BMI, heart disease, and hypertension. However, our analysis led us to focus on more specific questions:

* Is there a linear relationship between BMI and HbA1c levels?
* Which variables provide the best prediction of diabetes risk?
* Given a patient's BMI, heart disease status, and age, what is the probability of having diabetes?
* How do health indicators and demographic factors differ across identified clusters, revealing high-risk or unique subgroups?
* Can we classify an individual as diabetic or non-diabetic based on their health metrics and demographic data?

## Data

The dataset contains over 100,000 data points and sixteen variables, including BMI, blood glucose levels, age, smoking history, gender, race, hypertension, and heart disease.

**Strengths:** Large size, mix of numerical and categorical variables.

**Weaknesses:** Required significant cleaning and preprocessing, including handling missing values and inconsistent representations (e.g., race represented in multiple binary columns).

**Data Wrangling:**

* Removed the `blood_glucose_level` variable due to its direct predictive nature.
* Cleaned the `smoking_history` variable by consolidating categories.
* Combined multiple binary race columns into a single categorical `race` column.
* Converted the `diabetes` variable to a binary format (0: No, 1: Yes).

## Analysis and Modeling

We employed various machine learning techniques to answer our research questions:

**1. Linear Regression:**

* Question: Is there a linear relationship between BMI and HbA1c levels?
* Model: `hbA1c_level = 5.15 + 0.014 * bmi`
* Result: A weak but statistically significant positive relationship was found, with BMI explaining only 0.72% of the variance in HbA1c levels.

**2. Multiple Linear Regression:**

* Question: Which variables provide the best prediction of diabetes risk?
* Model: `diabetes = -0.8737 + 0.04278 * genderFemale + 0.06210 * genderMale + 0.002389 * age + 0.09691 * hypertension + 0.1280 * heart_disease + 0.006293 * bmi + 0.1128 * hbA1c_level`
* Result: The model with gender, age, hypertension, heart disease, BMI, and HbA1c level as predictors had an Adjusted R-squared of 0.2733.

**3. Naïve Bayes:**

* Question: Given that a patient has a higher BMI, has heart disease, and is older, what is the probability that the patient has diabetes or not?
* Result: The overall probability of diabetes was 11%. For individuals with BMI ≥ 28.4, heart disease, and age ≥ 47, the probability of diabetes increased to 44%. The model achieved an accuracy of 99.72%, likely influenced by class imbalance.

**4. K-Means Clustering:**

* Question: How do health indicators and demographic factors differ across the identified clusters?
* Result: Four distinct clusters were identified:
    * **Cluster 1 (Older Group):** Highest prevalence of hypertension (20.8%), heart disease (14.4%), and high HbA1c levels (5.8), indicating a high risk.
    * **Cluster 2 (Young, Healthy Group):** Younger population with low prevalence of hypertension and heart disease, representing an opportunity for preventative care.
    * **Cluster 3 (Young-Middle-Aged Group):** Low prevalence of heart disease but relatively high BMI (29.64), suggesting potential future risks.
    * **Cluster 4 (Young Middle-Aged Group):** Combination of obesity (BMI 30.14) and elevated HbA1c levels (5.64), indicating an emerging risk.

**5. Classification Models:**

* Question: Can we classify an individual as diabetic or non-diabetic based on their health metrics and demographic data?
* We compared the performance of several models against a **Null Model** (accuracy: 89.0%).

| Classification Model | Accuracy |
| :------------------- | :------- |
| Null Model           | 89.0%    |
| Nearest Neighbor     | 92.4%    |
| Logistic Regression  | 93.15%   |
| SVM                  | 93.2%    |
| Decision Trees       | 94.0%    |
| Random Forest        | 93.79%   |

* **Decision Trees** achieved the highest accuracy (94.0%), with HbA1c level being identified as a key predictor.

## Conclusion

All the classification models outperformed the Null Model, demonstrating their ability to predict diabetes status. The Decision Tree model showed the best performance. Our analysis highlights the significant association between health indicators like BMI, age, HbA1c levels, hypertension, and heart disease with diabetes.

## Potential Future Work

Future work could explore time-series techniques to analyze trends in blood glucose levels or predict diabetes-related hospitalizations, requiring longitudinal patient data. However, the machine learning techniques applied in this project effectively addressed our research questions.