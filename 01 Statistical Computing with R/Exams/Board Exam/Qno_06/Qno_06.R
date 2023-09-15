#a
set.seed(7916007)
Age <- sample(18:99, 150, replace = TRUE)
sex <- sample(c("male", "female"), 150, replace = TRUE)
education_levels <- sample(c("No_education", "Primary", "Secondary", "Beyond_Secondary"), 150, replace = TRUE)
Socio_Eco_Status <- sample(c("Low", "Middle", "High"), 150, replace = TRUE)
BMI <- runif(150, min = 14, max = 38)

data <- data.frame(Age, sex, education_levels, Socio_Eco_Status, BMI)

# Convert to factors
data$sex <- factor(data$sex, levels = c("male", "female"))
data$education_levels <- factor(data$education_levels, levels = c("No_education", "Primary", "Secondary", "Beyond_Secondary"))
data$Socio_Eco_Status <- factor(data$Socio_Eco_Status, levels = c("Low", "Middle", "High"))

head(data)


library(ggplot2)

# b
# Sub-divided bar diagram of BMI by sex
ggplot(data, aes(x = sex, y = BMI, fill = Socio_Eco_Status)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Sub-divided Bar Diagram of BMI by Sex and Socio_Eco_Status",
       x = "Sex", y = "BMI")

#' Interpretation:
#' Sub-divided Bar Diagram of BMI by Sex and Socio_Eco_Status shows that we 
#' have two categories in x axis as male and female and BMI of 3 category people
#' are shown and we get from the plat that Female with High socio economic
#' status has BMI where as same category male has low BMI. 

# Sub-divided bar diagram of BMI by Socio_Eco_Status
ggplot(data, aes(x = Socio_Eco_Status, y = BMI, fill = sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Sub-divided Bar Diagram of BMI by Socio_Eco_Status and Sex",
       x = "Socio_Eco_Status", y = "BMI")
#' Interpretation:
#' Sub-divided Bar Diagram of BMI by Socio_Eco_Status and Sex shows that we 
#' have two categories in x axis as male and female and BMI of 3 category people
#' are shown and we get from the plat that Female with High socio economic
#' status has BMI where as same category male has low BMI. As simillar to above plot.

# c

# Multiple bar diagram of age variable with sex and education_levels variables
ggplot(data, aes(x = sex, y = Age, fill = education_levels)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Multiple Bar Diagram of Age by Sex and Education Levels",
       x = "Sex", y = "Age")

#' Interpretation:
#' Here we get female with No-education and Primary education level has higer Age. 
#' Where as same but opposite for male.

# d
# Box plot of age
ggplot(data, aes(x = "", y = Age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age", x = NULL, y = "Age")

# Box Plot shows that for age the median is between 75 and 50 and Minimum value is 25 and max is almost 100.

# Box plot of BMI
ggplot(data, aes(x = "", y = BMI)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI", x = NULL, y = "BMI")

# Box Plot shows that for BMI the median is between 30 and 20 and Minimum value is 15 and max is above 35.
# Here the median BMI is almost 25.


# Histogram of age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

# Histogram of Age shows the data of Age looks like a Normal which is not a 
# Skewed distribution. 

# Histogram of BMI
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency")

# Histogram of BMI shows the data of Age looks like a Normal which is not a 
# Skewed distribution. We can use Mean value for further analysis. 

