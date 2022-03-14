#-------------------------------------------------------------------------------------------------------------------------------------------
# Milestone Project 2 ---------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

#Library data :- The following packages are required for the execution of below tasks.
library(dplyr)
library(readr)
library(mgsub)
library(flextable)
library(crosstable)
library(knitr)
library(ggplot2)
library(psych)
library(RColorBrewer)
dev.off()

#Importing the dataset
final_project <- read_csv("final_project.csv")
View(final_project)


#-------------------------------------------------------------------------------------------------------------------------------------------
#Data Cleaning:-----------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------

#Preparation of data frame for analysis
# 1. Dropping unwanted columns 
m1_data=select(final_project, -c(Row_ID,Order_ID,Order_Date,Ship_Date,
                                 Customer_ID,Customer_Name,City,
                                 Postal_Code,Product_ID,Product_Name,State))
names(m1_data)
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

sample_bkp = sample_data
sample_bkp

# Checking distribution of Sample
#1. Sales
sample_df <- data.frame(sample_data)
#ggplot to show the density curve
ggplot(sample_df, aes(x = sample_data$Sales)) +
    stat_function(
    fun = dnorm,
    args = with(sample_df, c(mean = mean(sample_data$Sales), sd = sd(sample_data$Sales)))) +
    scale_x_continuous("Normal Distribution of Sale Sample") + scale_y_continuous(" ")

#Q-Q plot to display normal distribution.
ggqqplot(sample_data$Sales, ylab = "Sales",
         ggtheme = theme_minimal())

#2. Profit
#ggplot to show the density curve
ggplot(sample_df, aes(x = sample_data$Profit)) +
    stat_function(
    fun = dnorm,
    args = with(sample_df, c(mean = mean(sample_data$Profit), sd = sd(sample_data$Profit)))) +
    scale_x_continuous("Normal Distribution of Profit Sample") + scale_y_continuous(" ")

#Q-Q plot to display normal distribution.
qqnorm(sample_data$Profit)
qqline(sample_data$Profit)

#3. Shipping Cost
#ggplot to show the density curve
ggplot(sample_df, aes(x = sample_data$Shipping_Cost )) +
    stat_function(
    fun = dnorm,
    args = with(sample_df, c(mean = mean(sample_data$Shipping_Cost), sd = sd(sample_data$Shipping_Cost)))) +
    scale_x_continuous("Normal Distribution of Shipping Cost") + scale_y_continuous(" ")

#Q-Q Plot to display normal distribution.
ggqqplot(sample_data$Shipping_Cost, ylab = "Shipping Cost",
         ggtheme = theme_minimal())

#Average Number of Sales for 2013
sale_13 = dplyr::filter(sample_df, Year=="2013")
mean_13sale=mean(sale_13$Sales)
knitr::kable(mean_13sale,col.names = "Average sales during 2013")

#Extracting 2014 data 
sale_14=dplyr::filter(m1_data, Year=="2014")
# Hypothesis testing to check if the average sale in 2014 is greater than 2013 at 95% Confidence Interval
t.test(sale_14$Sales, mu=mean_13sale, alternative = "great",conf.level = 0.95)

#Obtaining descriptive statistics for sales per Market.
describeBy(sample_df$Sales, 
           group=sample_df$Market,skew=FALSE)

# Maximum Number of Sales in Various Market
max_sale<-tapply(m1_data$Sales, m1_data$Market, sum)
b1=barplot(max_sale,
           main="Maximum Sales in Various Market",
           ylim = c(0,3800000),
           ylab= "Maximum Sale",
           xlab= "Market",
           col= "slateblue",
           cex.names=0.7,
           cex.axis=0.7)
text(y=max_sale,
     b1,
     max_sale,
     cex=0.8,
     pos=3)

# Filter Profit data for Asia Pacific and Europe Market
mar_ap = dplyr::filter(sample_df, Market=="Asia Pacific")
ap_df<- data.frame(mar_ap$Profit)
mar_eu = dplyr::filter(sample_df, Market=="Europe")
eu_df <- data.frame(mar_eu$Profit)
print(ap_df)
print(eu_df)

# Checking if the average profit for Asia Pacific and Europe Market different?
t.test(ap_df,eu_df, alternative="two.sided",conf.level = 0.95)

#Plot to display profit densities in Asia Pacific and Europe.
dta_A <- density(ap_df$mar_ap.Profit, na.rm = TRUE)
dta_B <- density(eu_df$mar_eu.Profit, na.rm = TRUE)
plot(dta_A, col = "purple", main = "Profit densities in Asia Pacific and Europe Market", 
     ylim = c(0, max(dta_A$y,dta_B$y)))
     lines(dta_B, col = "dark blue")

#Shipping costs distribution for each shipping mode.
sample_df%>%
    ggplot(aes(x=Shipping_Cost, fill=Ship_Mode)) +
    geom_density() +
    labs(x= "Shipping Cost",y="Density",subtitle="Shipping costs distribution for each shipping mode")

# Extracting Standard Class and Second class data from sample.
sc_fc = dplyr::filter(sample_df, Ship_Mode=="Standard Class")
knitr::kable(sc_fc$Shipping_Cost,col.names = "Standard-Class Shipping costs")
sc_sc = dplyr::filter(sample_df, Ship_Mode=="Second Class")
knitr::kable(sc_sc$Shipping_Cost,col.names = "Second-Class Shipping costs")

#Hypothesis testing to check if second class have shipping costs lesser than first class.
t.test(sc_sc$Shipping_Cost,sc_fc$Shipping_Cost, alternative="less", conf.level = 0.95)

