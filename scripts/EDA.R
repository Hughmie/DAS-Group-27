library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)
library(car) 

clean_data <- read.csv("D:/Data Analysis Skill/git-repo/git-repo/DAS-Group-27/datasets/cleaned_dataset27.csv")

#EDA
# Variable distribution check
str(clean_data)
summary(clean_data)

# Visualize income distribution
ggplot(clean_data, aes(x = Income)) + 
  geom_bar(fill = c("skyblue", "salmon")) +
  labs(title = "Income Distribution")

# Relationship between categorical variables and income
lapply(c("Education", "Occupation", "Sex", "Marital_Status"), 
       function(var) {
         ggplot(clean_data, aes_string(x = var, fill = "Income")) +
           geom_bar(position = "fill") +
           coord_flip()
       })

# Check category balance
prop.table(table(clean_data$Income)) # Sampling method should be considered in case of imbalance

# Establish GLM model
# Full variable model
clean_data <- clean_data %>%
  mutate(
    Income = ifelse(Income == ">50K", 1, 0)  # Converts a character type to a numeric type
  )

# Verify the conversion result
table(clean_data$Income)

# Logistic regression model (dependent variable is binary)）
model <- glm(Income ~ Age + Education + Marital_Status + Occupation + Sex + Hours_PW + Nationality,
             data = clean_data, 
             family = binomial(link = "logit"))
#glm.fit:The fitting probability works out to be either zero or one
summary(model)

# Check multicollinearity (VIF < 5 is acceptable)
vif(model)

library(janitor)
#occupation
clean_data %>%
  tabyl(Occupation, Income) %>% 
  adorn_percentages("row") %>% 
  filter(`0` == 1 | `1` == 1)
# Key Findings:
# Occupations Priv-house-serv (private household services) exist in complete separation, with all practitioners earning <=50k (100% for column 0).
# Impact:
# This category causes the logistic regression model to fail to converge (warning glm.fit: The probability of fit is a value of zero or one) because it is a perfect predictor of income outcomes.

clean_data %>% 
  tabyl(Occupation, Income) %>% 
  head()

# Education
clean_data %>%
  tabyl(Education, Income) %>% 
  adorn_percentages("row") %>% 
  filter(`0` == 1 | `1` == 1)

clean_data %>% 
  tabyl(Education, Income) %>% 
  head()

# Marital_Status
clean_data %>%
  tabyl(Marital_Status, Income) %>% 
  adorn_percentages("row") %>% 
  filter(`0` == 1 | `1` == 1)

clean_data %>% 
  tabyl(Marital_Status, Income) %>% 
  head()

# Sex
clean_data %>%
  tabyl(Sex, Income) %>% 
  adorn_percentages("row") %>% 
  filter(`0` == 1 | `1` == 1)

clean_data %>% 
  tabyl(Sex, Income) %>% 
  head()

# Nationality
clean_data %>%
  tabyl(Nationality, Income) %>% 
  adorn_percentages("row") %>% 
  filter(`0` == 1 | `1` == 1)

clean_data %>% 
  tabyl(Nationality, Income) %>% 
  head()

# Box plot of Hours_PW and revenue
ggplot(clean_data, aes(x = Income, y = Hours_PW)) +
  geom_boxplot()

# Box plot of Age and revenue
ggplot(clean_data, aes(x = Income, y = Age)) +
  geom_boxplot()

