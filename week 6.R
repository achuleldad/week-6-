?beavers
str(beaver2)
View(beaver2)
# The beaver dataset contains data on body temp of 4 women
#every 10 mins over the day for demo purpose
#we want to examine the differenece in average body temperature
#body temperature is affected by activity
#First we need to ensure the data is in the correct order
#activ should be a factor
#temp is numerical

H0: Body temperature is not affected by activity
H1: Body temperature is affected by activity


#I'm copying the data to a datafram'
#This is not a neccesary step

beavers_data <- beaver2
beavers_data
str(beavers_data)

beavers_data$activ <- factor(beavers_data$activ, labels = c("no","yes"))
beavers_data
str(beavers_data)
install.packages("ggplot2")
library(ggplot2)
#range(beavers_data$temp)

?hist()
ggplot(beavers_data, aes(x=temp))+geom_histogram()+theme_bw()
ggplot(beavers_data,aes(x=temp))+geom_histogram(breaks = seq(36,38,.2))+theme_bw()+labs(x="temp", y ="Activity")+scale_y_continuous(breaks = seq(0,60,5))

#lets look at the corellation bbtw both of these variables
#to evaluate the strength of the relationship
#and whether it is negative or positive

#we can use the libraries to help improve 
#the chart. Also includes correlations between variables \

install.packages("psych")
library(psych)

install.packages("lattice")
library("lattice")
windows(20,10)
attach(beavers_data)
histogram(~temp | activ,  data = beavers_data, main = "Distribution of beaver activity data",
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
detach(beavers_data)
attach(beavers_data)
windows(16,20)
qqnorm(temp)
qqline(temp, col= ("red"))

opar <- par(no.readonly = TRUE)
windows(20,10)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1,2))

str(beavers_data)
with(beavers_data,{
     qqnorm(temp[activ == "yes"],
            main = "Beavers active data")
     qqline(temp[activ == "Yes"])})




normality_test <- shapiro.test(beavers_data$temp)
normality_test
normality_test$p.value
