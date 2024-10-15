#Read in the dataframe file
govData <- read.csv('Homework1_Bonds.csv')

#Look at first couple rows
head(govData)

#Find total number of rows
totalRows <- nrow(govData)

#Find total number of bonds that were carried and defeated

sum(govData$Result=="Carried")
sum(govData$Result=="Defeated")

#overall percentage of support in all bonds across all governments
overallSupportPercent<-(sum(govData$Result=="Carried")/totalRows)*100
print(overallSupportPercent)

#Percentage of support in City government by dividing CITY and carried by total CITY bonds
sum(govData$Result=="Carried" & govData$Type=="CITY")/(sum(govData$Type=="CITY"))*100

#Percentage of support in County government by dividing COUNTY and carried by total COUNTY bonds
sum(govData$Result=="Carried" & govData$Type=="COUNTY")/(sum(govData$Type=="COUNTY"))*100

#Percentage of support in ISD government by dividing IST and carried by total ISD bonds
sum(govData$Result=="Carried" & govData$Type=="ISD")/(sum(govData$Type=="ISD"))*100

#Percentage of support in WD government by dividing WD and carried by total WD bonds
sum(govData$Result=="Carried" & govData$Type=="WD")/(sum(govData$Type=="WD"))*100

#add new variable called Votes_Total
govData$VotesTotal<-govData$VotesFor + govData$VotesAgainst

#The highest number of votes for a bond (regardless of support or not)
max(govData$VotesTotal)

#Subset with only bonds with 100 or more total votes
hundredPlus <- govData[govData$VotesTotal >= 100,]

#Create new variable that shows percentage of support for each bond
hundredPlus$Support_Rate <- (hundredPlus$VotesFor/hundredPlus$VotesTotal)*100

#Create histogram of above variable as it is quantitative
hist(hundredPlus$Support_Rate,main='Distribution of Support Percentage',xlab='Percentage of Support',col='#78B7D0',xlim=c(0,100),ylim=c(0,800))

#Calculate mean and standard deviation of the support percentage
sd(hundredPlus$Support_Rate)
mean(hundredPlus$Support_Rate)

#Find least and most support rate
min(hundredPlus$Support_Rate)
max(hundredPlus$Support_Rate)
#Find quartiles for the support rate variable
quantile(hundredPlus$Support_Rate, prob=c(.25,.5,.75), type=1)

#Make scatterplot showing relationship between bond price and support rate
plot(hundredPlus$Amount,hundredPlus$Support_Rate,main='Price and Support Percentage',xlab='Price',ylab='Support Percentage',pch=20)
#Show line of best fit
abline(lm(hundredPlus$Support_Rate~hundredPlus$Amount),col='blue')

#Calculate correlation coefficient
cor(hundredPlus$Amount, hundredPlus$Support_Rate)
