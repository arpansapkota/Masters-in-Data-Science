# a
rating <- c(9,2,5,8,6,1,3,2,8,4,6,8,7,1,2,6,10,5,6,9,6,2,4,7)

##b. 
# frequency
frequency <- table(rating)
# percentage
percentage <- frequency / sum(frequency) * 100

# Removing NA 
valid_ratings <- rating[!is.na(rating)]
# Valid percentages
valid_percentages <- table(valid_ratings) / length(valid_ratings) * 100
# cumm percent
cum_percent <- cumsum(percentage)

#Binding
finaltable <- cbind(frequency,percentage,valid_percentages,cum_percent)

total <- c(colSums(finaltable[,-4]), NA)

finaltable <- round(finaltable, 2)

finaltable <- rbind(finaltable, total)

#olumn name
colnames(finaltable) <- c("Frequency", "Percent", "Valid Percent", 
                     "Cumulative Percent")

finaltable

