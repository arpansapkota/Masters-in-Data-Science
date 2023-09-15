#Explain following data types in R with examples
#1) Integer variable is different than number variable
#3) Categorical variable is different than factor variable
#•) Date variable is different than Date as well as time variable
#Explain these terms with examples for R
#a) Getting multi-way table with array
#5) Creating class intervals of continuous variable
#8) Missingness vs nothingness
#Explain the followings with examples for R
#1) Reference range based on mean
#b) Reference range based on median
#c) Outliers and extreme values
#Explain the following in R with example:
#a) Nodes and edges
#b) Diameter
#c) Edge density

#Q6. Open the R studio and do the followings with R script and knit HTML output
#a) What happens when 4L is multiplied by 3.2?
#b) What happens when 4L is multiplied by 2L?
#c) Define blood with 0, 0, A, A, B, B and check its type and attributes with your
#d) Define x with 1,2, NA.8,3,NA,3 and get its mean with or without pipes.
#e) Get the first and sixth elements of x using sub-setting codes and its explanation

# a) What happens when 4L is multiplied by 3.2?
result_a <- 4L * 3.2
result_a

# b) What happens when 4L is multiplied by 2L?
result_b <- 4L * 2L
result_b

# c) Define blood with 0, 0, A, A, B, B and check its type and attributes
blood <- factor("O","O", "A", "A", "B", "B")
class(blood)
attributes(blood)

# d) Define x with 1,2, NA.8,3,NA,3 and get its mean with or without pipes
x <- c(1, 2, NA, 8, 3, NA, 3)
mean(x, na.rm = TRUE)
# or using pipes
library(dplyr)
x %>%
  mean(na.rm = TRUE)

# e) Get the first and sixth elements of x using sub-setting codes and its explanation
first_element <- x[1]
sixth_element <- x[6]
first_element
sixth_element


#Q7. Do the following in R studio and with R script to knit HTML output
#a) Define an object "rating" with 9, 2, 5, 8, 6, 1, 3, 2, 8, 4, 6, 8, 7, 1, 2, 6, 10, 5,
#b) Replicate the given table obtained from SPSS software for the rating object in R


#Q8. Use the "air qualits" data as AO to do following in R Studio with R script to knit HTML outpat
#a) Replace missing values of Ozone variable with the best measure of central tendency
#b) Create a Date vanable in AQ using Month and Das variable for vear 2022.
#c) Create line plot of Ozone" variable with Date" as the row index and interpret it carefully
#d) Get class intervals of the cleaned Ozone variable using range. its square root and zero rounding
#e) Get frequencs distribution (n and %) of Ozone variable class intervals and interpret it carefulls

data(airquality)

# Replace missing values with the mean
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)


# Create the "Date" variable
airquality$Date <- as.Date(paste("2022", airquality$Month, airquality$Day, sep = "-"))


# Line plot 
plot(airquality$Date, airquality$Ozone, type = "l", xlab = "Date", ylab = "Ozone")

#The line plot of the 'Ozone' variable shows the variations in ozone levels over time. There is a noticeable increase in ozone levels during certain periods, indicating potential pollution episodes or atmospheric conditions. 


# Class intervals of Ozone variable
class_intervals <- c(range(airquality$Ozone), sqrt(range(airquality$Ozone)), 0)
class_intervals <- unique(round(class_intervals, digits = 0))
class_intervals


# Frequency distribution of Ozone variable class intervals
frequency_table <- table(cut(airquality$Ozone, breaks = class_intervals, include.lowest = TRUE))
frequency_table <- as.data.frame(frequency_table)
names(frequency_table) <- c("Class Intervals", "Frequency")
frequency_table$Percentage <- frequency_table$Frequency / sum(frequency_table$Frequency) * 100
frequency_table


#Q9. Do the following in R Studio with tidyverse package using R Script to knit HTML output
#a) Define a tibblehaving country. vear, cases and population variables with 100 random data each
#b) Transform the cases variable as log of cases (LnCase) and population variable as log of population (LnPop)
#c) Create scatterplots of 1. Cases and population, 2 InCase and population, 3. Cases and LnPop and 4 LnCase LnPop in a single graph window with base R plot code and interpret it carefully.

library(tidyverse)

# Generate random data
set.seed(100)
data <- tibble(
  country = sample(c("A", "B", "C", "D", "E"), 100, replace = TRUE),
  year = sample(2000:2020, 100, replace = TRUE),
  cases = runif(100, 100, 10000),
  population = runif(100, 1000000, 10000000)
)


# Transform variables using log function
data <- data %>%
  mutate(LnCase = log(cases),
         LnPop = log(population))

# Scatterplot of Cases and Population
plot(data$cases, data$population, xlab = "Cases", ylab = "Population",
     main = "Scatterplot of Cases and Population")

# Scatterplot of LnCase and Population
plot(log(data$cases), data$population, xlab = "LnCase", ylab = "Population",
     main = "Scatterplot of LnCase and Population")

# Scatterplot of Cases and LnPop
plot(data$cases, log(data$population), xlab = "Cases", ylab = "LnPop",
     main = "Scatterplot of Cases and LnPop")

# Scatterplot of LnCase and LnPop
plot(log(data$cases), log(data$population), xlab = "LnCase", ylab = "LnPop",
     main = "Scatterplot of LnCase and LnPop")


Interpretation:
  
  #The scatterplot of Cases and Population shows the relationship between the number of cases and the population. It helps visualize any potential patterns or trends.
  #The scatterplot of LnCase and Population displays the relationship between the logarithm of cases and the population. Taking the logarithm of the cases can help stabilize the variance and better understand the relationship.
  #The scatterplot of Cases and LnPop illustrates the relationship between the cases and the logarithm of the population. This combination can help identify patterns or associations between the variables.
  #The scatterplot of LnCase and LnPop represents the relationship between the logarithm of cases and the logarithm of the population. This transformation can provide a different perspective on the relationship, allowing for more accurate comparisons between data points.
  

#Q9. OR quesn
#Load the "graph" package in R studio and do the basic SNA as follows with R script and HTML output
#a) Define g as graph object with (1,2.2,3.3.4.4.1) as its elements
#b) Plot g with node color as green, node size as 30, link color as red and link size as 5 and interpret it
#c) Plot the g as undirected arguments and interpret it carefully
#d) Plot g with seven nodes and interpret it carefully
#e) Get degree. closeness and betweenness of g and interpret them carefulls.
  
library(igraph)

# Define g as a graph object
g <- graph(c(1, 2, 2, 3, 3, 4, 4, 1))


# Plot g with customized attributes
plot(g, vertex.color = "green", vertex.size = 30, edge.color = "red", edge.width = 5)


#Interpreton: Explain output

# Plot g as an undirected graph
plot(g, vertex.color = "green", vertex.size = 30, edge.color = "red", edge.width = 5, directed = FALSE)


#Interpret the undirected plot:
#When the graph is plotted as undirected, the directionality of the links is ignored, and all links are represented

# Plot g with seven nodes
plot(g, vertex.color = "green", vertex.size = 30, edge.color = "red", edge.width = 5, n = 7)

#Interpret the plot with seven nodes:
#By specifying n = 7, we plot the graph with only seven nodes. If the original graph had more than seven nodes, only the first seven nodes will be displayed. 


# Degree 
degree(g)

# Closeness
closeness(g)

# Betweenness 
betweenness(g)


#The degree of a node represents the number of connections it has to other nodes in the graph. 

#Closeness centrality measures how easily a node can access other nodes in the graph. Nodes with high closeness centrality are more central and have shorter paths to other nodes.

#Betweenness centrality quantifies the extent to which a node lies on the shortest paths between other nodes. Nodes with high betweenness centrality act as bridges or intermediaries in the network.


#Q10. open R and then go to help and manual if pdf and open : an inrto to R"
#import this pdf file inR using :pdftools" package
#perform pre processing and crate 'corpus' afterwARDS
#find most frequent terms and create histogram of the most frequebt
#create word cloud of the corpus color it usinf rainbow or R color brewer package.skeleton(perform topic modelling and intrepret the result carefully)

library(pdftools)
library(tm)
library(wordcloud)
library(RColorBrewer)

pdf_text <- pdf_text("/Users/arpan/desktop/Intro to R.pdf")

# Convert into corpus
corpus <- Corpus(VectorSource(pdf_text))


# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)

#Most frequent terms
freq_terms <- findFreqTerms(dtm, lowfreq = 100)

# Create a histogram 
hist(freq_terms, main = "Histogram of Most Frequent Terms", xlab = "Terms", ylab = "Frequency")



# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Get word frequencies
word_freq <- rowSums(as.matrix(tdm))

# Create a word cloud with rainbow colors
wordcloud(names(word_freq), word_freq, colors = rainbow(length(word_freq)))

#topic model:
dtm <- DocumentTermMatrix(corpus)



#Q10. OR
#Use the cleaned "AQ" file in R studio and do as follows with R Scripts and HTML outputs
#a) Get reference range of *Ozone" variable using mean and standard deviation
#b) Plot histogram of Ozone" variable and show the outliers of "Ozone" with reference range limits
#c) Get reference range of "Ozone" variable using median and inter-quartile range
#d) Plot boxplot of Ozone" variable and show the outliers of *Ozone" with reference range limits
#e) Write a summary of the results obtained from the histogram and boxplot

# Load the dataset
data(airquality)
AQ <- airquality

# Set reference range using mean and standard deviation
ozone_mean <- mean(AQ$Ozone, na.rm = TRUE)
ozone_sd <- sd(AQ$Ozone, na.rm = TRUE)
reference_range_mean <- c(ozone_mean - ozone_sd, ozone_mean + ozone_sd)
reference_range_mean

# Plot histogram with outliers based on mean and standard deviation
hist(AQ$Ozone, main = "Histogram of Ozone", xlab = "Ozone", ylab = "Frequency", col = "steelblue", border = "black")
abline(v = reference_range_mean, col = "red", lty = "dashed")
outliers_mean <- subset(AQ, Ozone < reference_range_mean[1] | Ozone > reference_range_mean[2])
points(outliers_mean$Ozone, rep(0, nrow(outliers_mean)), col = "red", pch = 20)

# Set reference range using median and inter-quartile range
ozone_median <- median(AQ$Ozone, na.rm = TRUE)
ozone_iqr <- IQR(AQ$Ozone, na.rm = TRUE)
reference_range_median <- c(ozone_median - 1.5 * ozone_iqr, ozone_median + 1.5 * ozone_iqr)
reference_range_median

# Plot boxplot with outliers based on median and inter-quartile range
boxplot(AQ$Ozone, main = "Boxplot of Ozone", ylab = "Ozone", col = "lightblue", border = "black")
abline(h = reference_range_median, col = "red", lty = "dashed")
outliers_median <- subset(AQ, Ozone < reference_range_median[1] | Ozone > reference_range_median[2])
points(rep(1, nrow(outliers_median)), outliers_median$Ozone, col = "red", pch = 20)

# Summary of results
# Reference Range of Ozone using mean and standard deviation
reference_range_mean[1]
reference_range_mean[2]

#Reference Range of Ozone using median and inter-quartile range
reference_range_median[1]
reference_range_median[2]


#Summary of Results
print(summary(AQ$Ozone))
print(outliers_mean)
print(outliers_median)
