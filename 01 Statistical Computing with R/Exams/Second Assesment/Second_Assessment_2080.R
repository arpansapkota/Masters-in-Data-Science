## SECOND ASSESSMENT

#' 6 Do the following in R using ggplot2 package 

#' a. Create a dataset with the following variables: age (18-99 years), sex (male/female), educational levels
#' (No education/Primary/ Secondary/Beyond Seondary), socio- economic status (Low, Middle,High) and body
#' mass index (14-38) with random 100 cases of each variable. 
#' b. Create a line chart of age variable using ggplot2 and interpret. 
#' c. Create scatter plot of age and body mass index variable using ggplot and interpert
#' d. Create classes of body mass index variable as: <18, 18-24, 25-30, 30+ abd show it as pie chart using ggplot
#' e. Create classes of age variable as <15, 15-59 and 60+ and show it as bar diagram using ggplot.


#Create the dataset
set.seed(07)
age <- sample(18:99, 100, replace = TRUE)
sex <- sample(c("male", "female"), 100, replace = TRUE)
educational_levels <- sample(c("No education", "Primary", "Secondary", "Beyond Secondary"), 100, replace = TRUE)
socioeconomic_status <- sample(c("Low", "Middle", "High"), 100, replace = TRUE)
body_mass_index <- runif(100, 14, 38)
data <- data.frame(age, sex, educational_levels, socioeconomic_status, body_mass_index)
head(data)

library(ggplot2)
# Line chart of age variable
ggplot(data, aes(x = age)) +
  geom_line(aes(y = ..count..), stat = "bin") +
  labs(title = "Line Chart of Age Variable",
       x = "Age",
       y = "Count")


#Interpretation of the line chart: The line chart displays the trend of age across the dataset. The x-axis represents the age values ranging from 18 to 99, and the y-axis represents the count (frequency) of individuals in the dataset with each age. The line connects the points corresponding to each age, showing how the frequency of individuals changes with age.

# Scatter plot of age and body mass index variable
ggplot(data, aes(x = age, y = body_mass_index)) +
  geom_point() +
  labs(title = "Scatter Plot of Age and Body Mass Index",
       x = "Age",
       y = "Body Mass Index")

#Interpretation of the scatter plot: The scatter plot shows the relationship between age and body mass index. Each point represents an individual in the dataset, and its position on the plot is determined by the age (x-axis) and body mass index (y-axis) values. The plot helps visualize any potential patterns or correlations between age and body mass index.


# Create classes of body mass index variable
data$BMI_class <- cut(data$body_mass_index, breaks = c(0, 18, 24, 30, Inf), labels = c("<18", "18-24", "25-30", "30+"))

# Pie chart of body mass index classes
ggplot(data, aes(x = "", fill = BMI_class)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Body Mass Index Classes",
       fill = "BMI Class")

#Interpretation of the pie chart: The pie chart represents the distribution of individuals across different classes of body mass index (BMI). Each segment of the pie chart corresponds to a BMI class: "<18", "18-24", "25-30", and "30+". The size of each segment reflects the proportion of individuals in the dataset belonging to that BMI class.


# Create classes of age variable
data$age_class <- cut(data$age, breaks = c(0, 15, 59, Inf), labels = c("<15", "15-59", "60+"))

# Bar diagram of age classes
ggplot(data, aes(x = age_class)) +
  geom_bar() +
  labs(title = "Bar Diagram of Age Classes",
       x = "Age Class",
       y = "Count")

#Interpretation of the bar diagram: The bar diagram shows the distribution of individuals across different classes of age. The x-axis represents the age classes ("<15", "15-59", "60+"), and the y-axis represents the count (frequency) of individuals in each age class. The height of each bar corresponds to the number of individuals belonging to the respective age class.

