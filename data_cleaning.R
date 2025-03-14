# load data
data = read.csv("dataset27.csv")

# inspect data
str(data)
summary(data)
sapply(data, unique)

# delete commas
# convert ? to NA
data <- data %>%
  mutate_all(~sub(",$", "", .)) %>%
  mutate_all(~na_if(., "?"))

# inspect again
sapply(data, unique)
str(data)

# find that the type of Age and Hour_PW are char,
# then convert to num
data <- data %>%
  mutate_at(vars(Age, Hours_PW), as.numeric)
str(data)

# query and delete missing values
colSums(is.na(data))
data <- na.omit(data)
str(data)

# save data
write.csv(cleaned_data, "datasets/cleaned_dataset27.csv", row.names = FALSE)
