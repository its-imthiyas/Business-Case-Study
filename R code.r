#Part 1
#Task 1 - Weighted Sum Method
#Install Packages
install.packages("./MCDA_0.1.0.tar.gz")
install.packages(c('Rglpk','triangle','plyr','ggplot2','glpkAPI','combinat'))
install.packages('MCDA') 
library('MCDA')

#Data Preparation
dataWSM <- read.csv('/Users/Imthiyas/Library/CloudStorage/OneDrive-UniversityofLeeds/MSc Data Science & Analytics/Semester 1/Business Analytics & Decision Science/Courseworks/Robot_Info.csv')
# Insert new column by estimating Weights
dataWSM$Weight <- c(0.0667, 0.2, 0.1333, 0.2667, 0.3333)
performanceTable <- dataWSM
performanceTable

#Weighted Sum
performanceTable <- data.frame(t(performanceTable))
colnames(performanceTable) <- performanceTable[1,]
performanceTable <- performanceTable[2:5,]
performanceTable <- sapply(performanceTable,as.numeric)
performanceTable

#Criteria
weights <- dataWSM[,c(1,6)] # Retain only column that is related to alternative characteristic 
weights <- data.frame(t(weights))
weights <- as.numeric(weights[2,])
weights

#Inversion - Min to Max
performanceTable[,4] <- performanceTable[,4]^-1 
performanceTable

#Normalization
performanceTable <- performanceTable/colSums(performanceTable)[col(performanceTable)] 
performanceTable

#Final Calculation
overall1 <- weightedSum(performanceTable, weights) 
names(overall1) <- colnames(dataWSM[,c(2:5)])
overall1

#Visualization
#Figure 1
bp <- barplot(overall1, main="Figure 1: WSM Score for different Robot Prototypes", xlab="Robot Prototype", ylab ="WSM Score" , ylim = c(0, max(overall1) + 0.03), col=c("#ea5545","#edbf33","#bdcf32", "#27aeef"), cex.main=0.8, cex.lab=1, cex.axis=0.4, cex.names=1)
legend("topleft", legend=names(overall1), fill=c("#ea5545","#edbf33","#bdcf32", "#27aeef"), cex=0.5)
text(x = bp, y = overall1, label = round(overall1, 6), pos = 3, cex = 0.8, col = "black")


#Part 2
#Install Packages
install.packages("corrgram")
library(dplyr)
library(ggplot2)
library(corrgram)
library(readr)
install.packages("tidyverse")
library(tidyverse)

#Data Preparation
transactions_customer <- read_csv("/Users/Imthiyas/Library/CloudStorage/OneDrive-UniversityofLeeds/MSc Data Science & Analytics/Semester 1/Business Analytics & Decision Science/Courseworks/Transactions_Customer.csv")
options(width = 700) #Adding more columns to the output
head(transactions_customer) #No need of dummification as all the variables are numeric

#Data Exploration - Looking for Missing Values
# Check if there are any null values in the dataframe
any_null_values <- any(is.na(transactions_customer))
# Print the result
print(any_null_values)

#Task 1 - Exploring the data for positive and negative impacts on Revenue (Target Variable)
cor(transactions_customer)
corrgram(transactions_customer)

#Making a better Correlation Matrix Plot
# Install and load the ggcorrplot package
install.packages("ggcorrplot")
library(ggcorrplot)
# Calculate correlation matrix
correlation_matrix <- cor(transactions_customer)
# Create a correlation plot with correlation coefficients
#Figure 2
ggcorrplot(correlation_matrix, lab = TRUE, colors = c("#ea5545", "white", "#6D9EC1")) +
ggtitle("Figure 2: Correlation Matrix") +
theme(plot.title = element_text(hjust = 0.5, size = 15))


#Seen Voucher, Estimated Income, Advertisement Channel
#Assumption 1: Linear relationship between the variables
dataplot <- transactions_customer
dataplot$Seen_Voucher <- as.factor(dataplot$Seen_Voucher) 
dataplot$Advertisement_Channel <- as.factor(dataplot$Advertisement_Channel)

#Figure 3 for Boxplot for Seen Vouchers & Advertisement Channel have Variance
ggplot(data=dataplot) + geom_boxplot((aes(Seen_Voucher,Revenue,color=Advertisement_Channel)))+
labs(title = "Figure 3: Boxplot - Seen Voucher and Revenue with Advertisement Channel",,x = "Seen Voucher",y = "Revenue") +
theme(plot.title = element_text(hjust = 0.5, size = 12), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "top")
#Seen Voucher gets more revenue across.
ggplot(data=dataplot) + geom_boxplot((aes(Seen_Voucher,Revenue)))+
labs(title = "Seen Voucher and Revenue with Advertisement Channel", x = "Seen Voucher",y = "Revenue")


#Figure 4 for Scatterplot for Estimated Income have Variance
ggplot(data=dataplot) + geom_point(aes(Estimated_Income,Revenue,color=Advertisement_Channel))+
labs(title = "Figure 4: Scatter Plot - Estimated Income and Revenue with Advertisement Channel",x = "Estimated Income",y = "Revenue") +
theme(plot.title = element_text(hjust = 0.5, size = 12), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "top")


#Task - 2: Fitting a linear regression model to predict Revenue
#Fitting Model: This is for better way of advertising
dataplot$Seen_Voucher <- as.factor(dataplot$Seen_Voucher) 
dataplot$Advertisement_Channel <- as.factor(dataplot$Advertisement_Channel)
model <- lm(Revenue ~ ., data=dataplot)
summary(model)

#Predicted vs Actual Revenue
set.seed(123) #Using the same seed to get the same random numbers
datasplit <- transactions_customer
datasplit$id <- 1:nrow(datasplit)
dim(datasplit)
trainingdata <- datasplit %>% sample_n(40*7)
testdata <- anti_join(datasplit, trainingdata, by = 'id')
print(dim(trainingdata))
dim(testdata)
modeltrainsplit <- lm(Revenue ~ ., data=trainingdata)
prediction <- predict(modeltrainsplit, newdata = testdata)
sqrt(mean((testdata$Revenue - prediction)^2)) #RMSE = 13.2412

# Create a scatterplot
plot(testdata$Revenue, prediction, xlim = c(40,140) ,ylim = c(40,140),xlab = "Actual Revenue", ylab = "Predicted Revenue", main = "Figure 6: Actual vs Predicted Revenue", col ="#6D9EC1", pch=16, cex.main=1, cex.lab=0.8, cex.axis=0.7)
# Add a line to the plot
abline(lm(prediction ~ testdata$Revenue), col = "maroon")

