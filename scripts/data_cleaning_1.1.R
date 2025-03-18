library(dplyr)

# load data
data = read.csv("D:\\git\\GitHub\\DAS-Group-27\\datasets\\cleaned_dataset27.csv")

# inspect data
str(data)
sapply(data, unique)
glimpse(data)

# find the categories should be merged
table(data$Nationality, data$Income)


# merge categories in Education
data <- data %>%
  mutate(Education = case_when(
    Education %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th") ~ "Basic Education",
    Education %in% c("10th", "11th", "12th", "HS-grad") ~ "High School",
    Education %in% c("Some-college", "Assoc-acdm", "Assoc-voc") ~ "College Education",
    Education == "Bachelors" ~ "Bachelors",
    Education %in% c("Masters", "Doctorate", "Prof-school") ~ "Advanced Education"
  ))

  ))
# merge categories in Marital_Status
data <- data %>%
  mutate(Marital_Status = case_when(
    Marital_Status %in% c("Married-AF-spouse", "Married-civ-spouse") ~ "Married",
    Marital_Status == "Never-married" ~ "Never-married", 
    Marital_Status %in% c("Divorced", "Married-spouse-absent", "Separated", "Widowed") ~ "Divorced/Spouse Absent/Separated/Widowed"
  ))

# merge categories in Occupation
data <- data %>%
  mutate(Occupation = case_when(
    Occupation %in% c("Exec-managerial", "Prof-specialty", "Tech-support") ~ "White-Collar", 
    Occupation %in% c("Craft-repair", "Machine-op-inspct", "Transport-moving") ~ "Blue-Collar", 
    Occupation %in% c("Handlers-cleaners", "Other-service", "Priv-house-serv", "Protective-serv") ~ "Service", 
    Occupation == "Adm-clerical" ~ "Adm-clerical",
    Occupation == "Sales" ~ "Sales", 
    Occupation == "Farming-fishing" ~ "Farming-fishing"
  ))

# merge categories in Nationality
data <- data %>%
  mutate(Nationality = case_when(
    Nationality == "United-States" ~ "United-States", 
    TRUE ~ "Others"
  ))

# save data
write.csv(data, "datasets\\cleaned_dataset27_1.1.csv", row.names = FALSE)