DAS Group 27 project 2：Analysis of income influencing factors

Project objectives:
Based on census data, a generalized linear model (GLM) was used to analyze the key factors that affect whether an individual's annual income exceeds $50k.

Data set:
File : 'dataset_27.csv'
Variable Description :
'Income' : target variable (binary: >50k or <=50k)
'Age', 'Education', 'Occupation', etc. : predictors

Analysis process:
1. Data cleaning: dealing with missing values and outliers (see 'data_clean.r').
2. Exploratory analysis: Visualization of the relationship between income distribution and variables(see 'Analysis' folder)).
3. Modeling: Logistic regression model construction and optimization (main analysis file 'Group_27_Analysis.qmd').
4. Results  : Summary of significant factors and Policy recommendations (PDF).
