#Date: 31/03/2021
#Author: Shruti Gupta

#loading packages
library(tidyr)
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)

#setting working directory
setwd("/Users/shrutinikhilagupta/Desktop")

#loading of dataset
marketingData <- read.csv("marketing_data.csv", 
                          header = TRUE)

#displaying the first 6 rows
head(marketingData)

#summarising the data
summary(marketingData)

#conversions of the variables
#Dt_Customer
marketingData$Dt_Customer<-as.Date(parse_date_time(marketingData$Dt_Customer,"mdy"))

#income
marketingData$Income<-as.numeric(marketingData$Income%>%gsub("[$,]","",.))

#calculating current age of the customer
marketingData<-mutate(marketingData, Age=as.integer(format(Sys.Date(),"%Y"))-marketingData$Year_Birth)

#number of years as member
marketingData<-mutate(marketingData, membershipYears=as.integer(format(Sys.Date(),"%Y"))-as.integer(format(marketingData$Dt_Customer,"%Y")))

#summary after conversion and creating age group
summary(marketingData)

#checking for missing values
sum(is.na(marketingData))
avgIncome<-mean(marketingData[,5], na.rm = TRUE)
marketingData[is.na(marketingData)]<-avgIncome
summary(marketingData$Income)


#visualisation
#removing outliers
marketingData<-marketingData%>%filter(.,Age<90)
summary(marketingData$Age)
marketingData<-marketingData%>%filter(.,Income<200000)
#Bar Plot
Incomeageplot<-marketingData%>%ggplot(aes(x=Age,y=mean(Income)))+
  geom_col(aes(fill=Education))+ 
  scale_y_continuous(labels=scales::dollar)+
  ggtitle("Average Income by Age")+
  theme(legend.title=element_blank())
Incomeageplot

#bar for the frequency number of membership years
numberofyearsplot<-marketingData%>%ggplot(aes(x=membershipYears, y=length(membershipYears)))+
  geom_col(aes(fill=Marital_Status))+
  ggtitle("Frequenvy of Membership Years")+
  theme(legend.title=element_blank())
numberofyearsplot

#total Spending
marketingData<-marketingData%>%mutate(marketingData,
                                      Totalspent= MntWines+ MntFruits+ MntMeatProducts+ MntFishProducts+ MntSweetProducts+ MntGoldProds)

#total acceptance of campaign
marketingData<-marketingData%>%mutate(marketingData,
                                      TotalcampaignsAccepted= AcceptedCmp1+ AcceptedCmp2 +AcceptedCmp3+ AcceptedCmp4+ AcceptedCmp5+ Response)

#Total Dependence
marketingData<-marketingData%>%mutate(marketingData,
                                      Totaldependents= Kidhome+ Teenhome)

#total Purchase
marketingData<-marketingData%>%mutate(marketingData,
                                      Totalpurchases= NumDealsPurchases+ NumCatalogPurchases+ NumStorePurchases+ NumWebPurchases)

Totalpurchasemaritalstatusplot<-marketingData%>%ggplot(aes(x=Marital_Status,y=Totalpurchases))+
  geom_col(aes(fill=Income))+ 
  scale_y_continuous(labels=scales::dollar)+
  ggtitle("Total Purchase by Marital Status")
Totalpurchasemaritalstatusplot

Totalspentmaritalstatusplot<-marketingData%>%ggplot(aes(x=Marital_Status,y=Totalspent))+
  geom_col(aes(fill=Income))+ 
  scale_y_continuous(labels=scales::dollar)+
  ggtitle("Total Spent by Marital Status")
Totalspentmaritalstatusplot

cor(marketingData$Income,marketingData$Totalspent)
IncomeTotalspent<-marketingData%>%ggplot(aes(x=Income,y=Totalspent))+
  geom_point(aes(color=Age))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Spent by Income")
IncomeTotalspent

cor(marketingData$Income,marketingData$Totalpurchases)
IncomeTotalpurchase<-marketingData%>%ggplot(aes(x=Income,y=Totalpurchases))+
  geom_point(aes(color=Age))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Purchase by Income")
IncomeTotalpurchase

cor(marketingData$Totalspent,marketingData$Totalpurchases)
purchasesVSspent<-marketingData%>%ggplot(aes(x=Totalspent,y=Totalpurchases))+
  geom_point(aes(color=Income))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Purchase by Total Spent")
purchasesVSspent

cor(marketingData$Age,marketingData$Totalspent)
AgeTotalspent<-marketingData%>%ggplot(aes(x=Age,y=Totalspent))+
  geom_col(aes(fill=Marital_Status))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Spent by Age")
AgeTotalspent

cor(marketingData$Age,marketingData$Totalpurchases)
AgeTotalpurchase<-marketingData%>%ggplot(aes(x=Age,y=Totalpurchases))+
  geom_col(aes(fill=Marital_Status))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Purchase by Age")
AgeTotalpurchase

cor(marketingData$Age,marketingData$TotalcampaignsAccepted)
AgeTotalcampaignsAccepted<-marketingData%>%ggplot(aes(x=Age,y=TotalcampaignsAccepted))+
  geom_col(aes(fill=Marital_Status))+ 
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)+
  ggtitle("Total Campaign Acceptance by Age")
AgeTotalcampaignsAccepted

totaldependendsplot<-marketingData%>%ggplot(aes(x=Totaldependents))+
  geom_bar(aes(fill=Marital_Status))+
  ggtitle("frequency of Totaldependents")
totaldependendsplot

#correlation/heatmap visualisation
#install.packages("corrplot")
library(corrplot)
marketingDatanum<-select_if(marketingData,is.numeric)%>%select(-ID,Year_Birth)
corrplot(cor(marketingDatanum))

AcceptanceCountry<-marketingData%>%ggplot(aes(x=Country,y=TotalcampaignsAccepted))+
  geom_col(aes(color=membershipYears))+
  ggtitle("Total Campaign by Country")
AcceptanceCountry

#regression for predicting total purchases, total spent, total acceptance
#recoding
unique(marketingData$Education)
unique(marketingData$Marital_Status)
unique(marketingData$Country)

library(car)
marketingData<-marketingData%>%mutate(marketingData,
                                      EducationRecode= recode(Education,
                                                               "'Graduation'=1;'Master'=2;'2n Cycle'=3;'PhD'=4",
                                                               as.numeric = TRUE))
marketingData<-marketingData%>%mutate(marketingData,
                                      MartialStatusRecode= recode(Marital_Status,
                                                               "c('Single','YOLO','Alone')=1;'Married'=2;'Divorced'=3;'Together'=4;'Widow'=5;'Absurd'=6",
                                                               as.numeric = TRUE))
marketingData<-marketingData%>%mutate(marketingData,
                                      CountryRecode= recode(Country,
                                                                    "'SP'=1;'CA'=2;'US'=3;'AUS'=4;'GER'=5;'IND'=6;'SA'=7;'ME'=8",
                                                                    as.numeric = TRUE))

#total spend
totalspendreg<-lm(Totalspent~MntWines+ MntFruits+ MntMeatProducts+ MntFishProducts+ MntSweetProducts+ MntGoldProds+ CountryRecode+ Income+ MartialStatusRecode,marketingData)
summary(totalspendreg)
plot(x=marketingData$Totalspent,y=predict(totalspendreg), type = 'p', main = "Predicted Total Spent Vs Actual Total Spent")


#total purchases
totalpurchasesreg<-lm(Totalpurchases~NumDealsPurchases+ NumCatalogPurchases+ NumStorePurchases+ NumWebPurchases+ CountryRecode+ Income+ MartialStatusRecode,marketingData)
summary(totalpurchasesreg)
plot(x=marketingData$Totalpurchases,y=predict(totalpurchasesreg), type = 'p', main = "Predicted Total Purchase Vs Actual Total Purcahse")

#total acceptance
totalcampaignsAcceptedreg<-lm(TotalcampaignsAccepted~ AcceptedCmp1+ AcceptedCmp2 +AcceptedCmp3+ AcceptedCmp4+ AcceptedCmp5+ Response+ CountryRecode+ Age+ MartialStatusRecode,marketingData)
summary(totalcampaignsAcceptedreg)
plot(x=marketingData$TotalcampaignsAccepted,y=predict(totalcampaignsAcceptedreg), type = 'p', main = "Predicted Total Capaigns Acceptance Vs Actual Total Campign Acceptance")

#writing the final file
write.csv(marketingData,file = "marketing_data_1.csv")
