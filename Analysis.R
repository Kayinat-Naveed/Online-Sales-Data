#install package
install.packages("haven")

#import tables 
Details <- read.csv("C:/Users/kayinat/Desktop/Msc/Semmester2/StatisticsApplication/Details.csv", header=TRUE)
View(Details)

Gender_Data<- read.csv("C:/Users/kayinat/Desktop/Msc/Semmester2/StatisticsApplication/Gender_Data.csv", header=TRUE)
View(Gender_Data)

Orders<- read.csv("C:/Users/kayinat/Desktop/Msc/Semmester2/StatisticsApplication/Orders.csv", header=TRUE)
View(Orders)

# merge orders and gender table using name column 
merged_table <- merge(Orders, Gender_Data, by.x = "CustomerName", by.y = "Name",all.x = TRUE) 
merged <- merge(merged_table, Details)



#create a unique id 
merged$un<-1
merged$id <- cumsum(merged$un)

#check variables types 
class(merged$Order.ID)
class(merged$CustomerName)
class(merged$Order.Date)
class(merged$State)
class(merged$City)
class(merged$Gender)
class(merged$Amount)
class(merged$Profit)
class(merged$Quantity)
class(merged$Category)
class(merged$Sub.Category)
class(merged$PaymentMode)
class(merged$id)

install.packages("dplyr")
install.packages("data.table")


#rename the variables
library(dplyr)
library(data.table)
merged <- rename(merged,"Order_Id"="Order.ID")
merged$Order_Id



#remove N/A values from column gender
library(data.table)
data_na <- merged[is.na(merged$Gender),]
Online_Sales <-merged[!is.na(merged$Gender),]

library(data.table)
Online_Sales$Gender <- as.factor(Online_Sales$Gender)
class(Online_Sales$Gender)

library(data.table)
Online_Sales$Quantity <- as.integer(Online_Sales$Quantity)



#delete column 
library(data.table)
Online_Sales <- select(Online_Sales,-Order.ID)

# Plot a graph which gender spends more on online shopping
install.packages("plotly")  
library(plotly)
ggplot(Online_Sales, aes(x= factor(Gender), y= Amount))+
  geom_bar(stat = "summary", fun = "sum",fill= "chocolate")+
  labs(title = "Amount spent by Gender", x = "Gender", y = "Total Spending")+
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))

# Plot a graph which gender order more products 
library(plotly)
ggplot(Online_Sales, aes(x= factor(Gender), y= Quantity))+
  geom_bar(stat = "summary", fun = "sum",fill= "chocolate")+
  labs(title = "Products Order by Gender", x = "Gender", y = "Total Products")+
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female"))

install.packages("DescTools")
library("DescTools") 
# Frequency table 
Freq(Online_Sales$Gender)

install.packages("gmodels")
library("gmodels")
# cross table for each products ordered by gender
CrossTable(Online_Sales$Category,
           Online_Sales$Gender, digits=2, prop.r = FALSE, prop.c =
             FALSE, prop.t = FALSE, prop.chisq = FALSE)

# descriptive statistics
install.packages("psych")
library("psych")
# descriptive statistics for overall table 
describe(Online_Sales$Amount)
library("psych")
# descriptive statistics by gender group 
describeBy(Online_Sales$Amount, group = Online_Sales$Gender)

#t test for amount spent by genders 
amount_male <- Online_Sales$Amount[Online_Sales$Gender == 0]
amount_female <- Online_Sales$Amount[Online_Sales$Gender == 1]
T_Test_spending <- t.test(amount_male, amount_female, var.equal = TRUE)
T_Test_spending
confidence_interval <- T_Test_spending$conf.int
confidence_interval
#t-test for quantity ordered by genders 
order_male <- Online_Sales$Quantity[Online_Sales$Gender == 0]
order_female <- Online_Sales$Quantity[Online_Sales$Gender == 1]
T_Test_Ordering <- t.test(order_male, order_female, var.equal = TRUE, conf.level = 0.95)
T_Test_Ordering
confidence_interval <- T_Test_Ordering$conf.int
confidence_interval



ols_amount <- lm(Amount ~ Gender, data = Online_Sales)
plot(Amount ~ Gender, data=Online_Sales, 
     xlab = "Gender", 
     ylab = "Amount", 
     main = "Amount Spend By Each Gender", 
     pch  = 20, 
     cex  = 2, 
     col  = "grey") 
abline(ols_amount, lwd = 3, col = "darkorange")
summary(ols_amount)

ols_order <- lm(Quantity ~ Gender, data = Online_Sales)
plot(Quantity ~ Gender, data=Online_Sales, 
     xlab = "Gender", 
     ylab = "Order Quantity", 
     main = "Quantity Ordered By Each Gender", 
     pch  = 20, 
     cex  = 2, 
     col  = "grey") 
abline(ols_order, lwd = 3, col = "darkorange")
summary(ols_order)
