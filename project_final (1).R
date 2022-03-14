#-------------------------------------------------------------------------------------------------------------------------------------------
# Final Project ---------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

#Loading libraries
library(dplyr)
library(readr)
library(mgsub)
library(flextable)
library(crosstable)
library(knitr)
library(ggplot2)
library(psych)
library(RColorBrewer)
library(ggpubr)
dev.off()

#Importing the dataset
final_project <- read_csv("final_project.csv")
View(final_project)

#-------------------------------------------------------------------------------------------------------------------------------------------
#Data Cleaning:-----------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

# 1. Dropping unwanted columns 
m1_data=select(final_project, -c(Row_ID,Order_ID,Order_Date,Ship_Date,
                                 Customer_ID,Customer_Name,City,
                                 Postal_Code,Product_ID,Product_Name,State))
View(m1_data)

# 2. Altering necessary records in dataset
y=mgsub(m1_data$Market, c("EU", "APAC","EMEA"), c("Europe", "Asia Pacific","Emirates"))
# Creating final data frame
m1_data$Market <-y
View(m1_data)

#-------------------------------------------------------------------------------------------------------------------------------------------
#Analysis-----------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

# Extracting Sample for T-Test
n <- 20
sample_data<- m1_data[sample(nrow(m1_data), size = n, replace = TRUE),]
View(sample_data)

#Correlation plot
pairs(~Sales + Profit + Shipping_Cost + Quantity+Profit, data = sample_data,col = rep(1:4, each = 10))

# Linear Regression
# 1. Does sales influence profit?
# Scatter plot to display correlation between Profits (Dependent) and Sales (Independent)
ggplot(sample_data,aes(Profit,Sales)) +
    geom_point() +
    ggtitle("Scatter plot for Profit VS Sales ") +  
    geom_smooth(method='lm')

#Regression model for Profit and Sales
model1 <- lm(Profit ~ Sales, data = sample_data)
summary(model1)

# 2.Does discount have an influence on sales?
# Scatter plot to display correlation between Sales (Dependent) and Discount (Independent)
ggplot(sample_data,aes(Sales,Discount)) +
    geom_point() +
    theme_minimal() +
    ggtitle("Scatter plot for Sales VS Discount ") +  
    geom_smooth(method='lm')

#Regression model for Sales and Discount.
model2 <- lm(Sales ~ Discount, data = sample_data)
summary(model2)

#3.Do profit and discount have an influence on sales?
# Scatter plot to display correlation between Sales (Dependent), Discount (Independent) and Profit(Independent) .
ggplot(sample_data,aes(Sales,Discount+Profit)) +
    geom_point() +
    theme_minimal() +
    ggtitle("Scatter plot for Sales VS Discount and Profit ") +  
    geom_smooth(method='lm')

#Regression model for Sales, Discount and Profit.
model3 <- lm(Sales ~ Discount+Profit, data = sample_data)
summary(model3)
