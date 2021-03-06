######################################################################################################################
# I: BASICS OF R (APPENDIX A)
######################################################################################################################

########################### A. Working with variables and lists ######################################################
a<-5                                        #sest a=5
a                                           #view value of a
b=6                                         #set b=6
a+b                                         #view value of a+b
a=c(1,3,5,7,9)                              #define a as a list of five values
a                                           #view values of a
a+b                                         #view values of a+b
a[1]+b                                      #view value of sum of first item in list a and b
c=cbind(a, a+b)                             #create a 2 column table consisting of the original a list and sum of a+b
c                                           #view c
colnames(c)=c('a','a+b')                    #assign column names for c
colnames(c)=c('a',paste('a+b(b=',b,')'))
c
d=c(1,2,3,4,5)                              #create new list d
a+d                                         #view values of sum of a and d

########################### B. Conditions and loops ##################################################################
if (Sys.time()<'2018-01-25 19:00:00 CST') 
{
  print('Nope, nope. Not break time yet!')
} else { 
  print("Sure - let's take a break!'")      #double quotes because single quote in text
}

for (i in (1:5))                            #simple loop to print squares of numbers from 1 to 5
{
  print(i^2)
}

for (i in (1:5))                            #slightly more involved loop to print numbers 1 to 5, values in list a, 
{                                           #and squares of values in list a
  print(c(i, a[i], a[i]^2))
}

########################### C. Working with files ####################################################################
#setwd('FILEPATH')                          #change FILEPATH and '/', '\ ' in path specification
#setwd('C:/Users/Shaila/Dropbox/Classes/Social Analytics/Social Analytics Book/Supporting Files/Data/For R Exercises')

#read data files into data franes
Bankruptcies=read.csv('Bankruptcies.csv', header=T)
LandLocked=read.csv('Landlocked.csv', header=T)

Bankruptcies$Population                     #view population
LandLocked$State                            #view states

attach(Bankruptcies)                        #so we don't have to reference table/data frame name
attach(LandLocked)                          #use attach() to skip referencing the dataframe

Population                                  #like this, not Bankruptcies$Population
State

#JOIN Bankruptcies and LandLocked ON Bankruptcies$State=Landlocked$State
StateData=merge(Bankruptcies, LandLocked, by.x='State', by.y='State')

#compute BkrptPer100
StateData$BkrptPer100=StateData$Bankruptcies/StateData$Population*100

#obtain average BkrptPer100 for landlocked and non-landlocked states
aggregate(StateData$BkrptPer100, by=list(StateData$LandLocked), FUN=mean)

#obtain variance BkrptPer100 for landlocked and non-landlocked states
aggregate(StateData$BkrptPer100, by=list(StateData$LandLocked), FUN=var)

#is the difference statistically significant?
t.test(StateData$BkrptPer100 ~ StateData$LandLocked)

#output StateData dataframe to StateBankruptcies.csv file
write.csv(StateData,file='StateBankruptcies.csv', quote=FALSE)

#####################################################################################################################
# II: INSTALL AND LOAD PACKAGES WE WILL USE
#####################################################################################################################

install.packages('Hmisc')
install.packages('data.table')
install.packages('aplpack')
install.packages('psych')
install.packages('ggplot2')
install.packages('igraph')
install.packages('tm')
install.packages('SnowballC')
install.packages('syuzhet')
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('RColorBrewer')
install.packages('reshape2')
install.packages('NLP')
install.packages('openNLP')
install.packages('RCA')
install.packages('topicmodels')
install.packages('streamR')
install.packages('twitteR')
install.packages('Rfacebook')

library(Hmisc)			#will need only first five today
library(data.table)		
library(aplpack)		
library(psych)			
library(ggplot2)
#library(igraph)
#library(tm)
#library(SnowballC)
#library(syuzhet)
#library(wordcloud)
#library(wordcloud2)
#library(RColorBrewer)
#library(reshape2)
#library(NLP)
#library(openNLP)
#library(RCA)
#library(topicmodels)
#library(streamR)
#library(twitteR)
#library(Rfacebook)

#################################################################################################################
# III: VISUALIZING AND ANALYZING STOCK PRICE DATA (APPENDIX B)
#################################################################################################################

########################### Set up our data #####################################################################
#setwd('FILEPATH')                          #change FILEPATH and '/', '\ ' in path specification

AXP = read.csv('AXP.csv', header=TRUE)
AXP=cbind(AXP, Symbol='AXP')
YUM = read.csv('YUM.csv', header=TRUE)
YUM=cbind(YUM, Symbol='YUM')

StockPrices=rbind(AXP,YUM)                  #because StockPrices combines AXP and YUM data, output will
#differ from that in book

head(StockPrices)
tail(StockPrices)

StockPrices$Date=strptime(StockPrices$Date,'%m/%d/%Y')

attach(StockPrices)

StockPrices$Volatility=High-Low              #though we attached the data frame, we still must declare a new
#data frame column by referencing the data frame

attach(StockPrices)

StockPrices$BadDay=ifelse(Volatility>=mean(Volatility),TRUE,FALSE)

StockPrices$Era=ifelse(format(as.Date(Date),'%Y')<1999,'PreDotCom', 
                       ifelse(format(as.Date(Date),'%Y')<2002,'Bubble','DotCom'))

attach(StockPrices)

table(BadDay) #Check the frequency distribution of data in BadDay column

StockPrices$Month=format(as.Date(Date), '%b')

########################### Visualize our data ##################################################################

attach(StockPrices)

table(Month, BadDay)

sapply(StockPrices[,c(2:7,9)],mean)          #need c function because don't have continuous numeric columns

cbind(sapply(StockPrices[,c(2:7,9)],mean), sapply(StockPrices[,c(2:7,9)],median), sapply(StockPrices[,c(2:7,9)],sd))

aggregate(Volatility, by=list(Month), FUN=mean)

hist(Volatility, col='blue')

#barplot of only AXP closing prices in 2016
barplot(subset(Close, Symbol=='AXP' & format(as.Date(Date), '%Y')==2016), col='green', xlab='Trading Days', ylab='Stock Price ($s)', main='Daily AXP Closing Prices for 2016', ylim=c(0,100))

#scatterplots of closing prices by year for AXP and YUM
plot(subset(format(as.Date(Date), '%Y'),Symbol=='AXP'), subset(Close, Symbol=='AXP'), xlab='Year', ylab='Closing Prices', main='AXP Closing Prices by Year' , col='lightblue')
plot(subset(format(as.Date(Date), '%Y'),Symbol=='YUM'), subset(Close, Symbol=='YUM'), xlab='Year', ylab='Closing Prices', main='YUM Closing Prices by Year' , col='red')

#funky multi-dimensional view of stock price data
ForFaces=aggregate(StockPrices[,3:5], by=list(format(as.Date(Date), '%Y')), FUN=mean)
faces(cbind(ForFaces$Low, ForFaces$Low, 0, ForFaces$High, ForFaces$High, ForFaces$Close, ForFaces$Close, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), labels=ForFaces$Group.1, face.type=2, fill=FALSE)

ForFaces=subset(ForFaces,ForFaces$Group.1>=2009)
faces(cbind(ForFaces$Low, ForFaces$Low, 0, ForFaces$High, ForFaces$High, ForFaces$Close, ForFaces$Close, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), labels=ForFaces$Group.1, face.type=2, fill=FALSE)

boxplot(Volatility ~ Era, col='pink', xlab='Era', ylab='Volatility')

########################### Statistical Analysis ###############################################################

cor(Close, Volatility)                        #correlation between two variables
cor(StockPrices[c(5,7,9)])                    #correlation between multiple variables from the data frame
#if all variables were numeric, we could do cor(StockPrices)
#for correlations among all variables

rcorr(as.matrix(StockPrices[c(5,7,9)]))       #from Hmisc package - provides significance of correlations

rcorr(as.matrix(subset(StockPrices,Symbol=='AXP')[c(5,7,9)]))
rcorr(as.matrix(subset(StockPrices,Symbol=='YUM')[c(5,7,9)]))

#depict correlations visually as scatterplot matrices
pairs(subset(StockPrices,Symbol=='AXP')[c(5,7,9)], col='lightblue')
pairs(subset(StockPrices,Symbol=='YUM')[c(5,7,9)], col='red')

#t-tests for difference in continuous variable across bivariate variable
t.test(Volume ~BadDay)                        #one-column t-test
t.test(Volume ~Symbol)
t.test(Open, Close, paired=T)                 #paired (two-column) t-test

summary(aov(Volume ~BadDay*Symbol))           #ANOVA - for differences across more than two levels of one 
#grouping variable or across more than one grouping variable

#regression - for continuous dependent and independent variables
summary(lm(Volatility ~ Volume + Symbol + as.numeric(format(as.Date(Date), '%Y'))))

########################### Make pretty pictures ###############################################################

#using ggplot
ggplot(StockPrices,aes(Volatility)) + geom_histogram(color='lightgrey', fill='blue') + labs(x='Volatility',y='Frequency')
#with custom bins specification
ggplot(StockPrices,aes(Volatility)) + geom_histogram(color='lightgrey', fill='blue',bins=20) + labs(x='Volatility',y='Frequency')
#with specification of x-axis intervals
ggplot(StockPrices,aes(Volatility)) + geom_histogram(color='lightgrey', fill='blue', breaks=seq(0, 6, by=.25)) + labs(x='Volatility',y='Frequency')

#calculate average volatility by era
AvgVolatility=aggregate(Volatility, by=list(Era), FUN=mean)
ggplot(AvgVolatility, aes(Group.1,x)) + geom_bar(stat='identity', fill='firebrick') + labs(x='Era', y='Average Volatility')

#display multiple graphs on screen
par(mfrow=c(2,2))                             #partition our screen into 2 rows x 2 columns
plot(subset(format(as.Date(Date), '%Y'),Symbol=='AXP'), subset(Close, Symbol=='AXP'), xlab='Year', ylab='Closing Prices', main='AXP Closing Prices by Year' , col='lightblue')
plot(subset(format(as.Date(Date), '%Y'),Symbol=='YUM'), subset(Close, Symbol=='YUM'), xlab='Year', ylab='Closing Prices', main='YUM Closing Prices by Year' , col='red')
boxplot(subset(Volatility,Symbol=='AXP') ~ subset(Era,Symbol=='AXP'), col='lightblue', xlab='Era', ylab='Volatility', main='AXP Volatility by Era')
boxplot(subset(Volatility,Symbol=='YUM') ~ subset(Era,Symbol=='YUM'), col='red', xlab='Era', ylab='Volatility', main='YUM Volatility by Era')
