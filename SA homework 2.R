library(Hmisc)			
library(data.table)		
library(aplpack)		
library(psych)			
library(ggplot2)

yelp = read.csv('YelpReviews.csv')
head(yelp)
tail(yelp)

yelp$Date=strptime(yelp$Date,'%m/%d/%Y')

attach(yelp)
# Extracting Year from the Date column
yr = as.numeric(format(as.Date(Date),'%Y'))


hist(Votes.Useful, col = 'purple', xlim = c(0,25), ylim = c(0,12000))
hist(Votes.Funny, col = 'yellow',xlim = c(0,25))
hist(Votes.Cool, col = 'magenta', xlim = c(0,25))

hist(adj, col = 'light green')
hist(negate,col = 'green')
#hist(yr, color = 'light blue')

#Barplot of % of words that depict negative emotion
barplot(negemo, col='green', xlab='Words depict negative emotion', 
        ylab='Percentage (%)', main='Percentage of Words in a 
        review that depict negative emotion', ylim=c(0,100))
hist(negemo, col = 'grey')

#Barplot of % of words that depict positive emotion
barplot(posemo, col='blue', xlab='Words depict positive emotion', 
        ylab='Percentage (%)', main='Percentage of Words in a 
        review that depict positive emotion', ylim=c(0,100))
hist(posemo, col = 'yellow', ylim = c(0,5000))

#Scatter plot of number of useful votes vs. % of words used that depict negative emotion
plot(Votes.Useful,negemo,type = 'p', xlab = 'Useful votes',
     ylab = '% Words depicting Negative Emotion')

#Scatter plot of number of cool votes vs. % of words used that depict negative emotion
scatter.hist(Votes.Cool,negemo, xlab = 'Cool Votes',
             ylab = '% Words depicting Negative Emotion')

#Scatter plot of number of cool votes vs. % of adjectives used
scatter.hist(Votes.Cool,adj, xlab = 'Cool Votes',
             ylab = 'Adjectives used')
#Scatter plot of Useful votes vs. numbers of stars
plot(Votes.Useful,Stars, type = 'p')

#Scatter plot of % words that depict positive emotion throughout the year
plot(posemo,yr,xlab = '% Words describing positive emotion', ylab = 'Year', ylim = c(2004,2015))

#Adding Positive Tone variable
yelp$PositiveTone = posemo/(posemo + negemo)
#Adding Tone variable
yelp$Tone = ifelse(posemo>negemo,'Positive',ifelse(negemo>posemo,'Negative','Neutral'))
#Adding TotalVotes variable
yelp$TotalVotes = Votes.Useful+Votes.Funny+Votes.Cool

attach(yelp)

hist(TotalVotes, xlim = c(0,60), col = 'light blue',breaks = 70)

#Some statistics of Total Votes
max(TotalVotes) #361
mean(TotalVotes) #2.033
median(TotalVotes) #1
sd(TotalVotes)

#Categorize successful post: 
yelp$Successful = ifelse(TotalVotes<3,'No','Yes')

attach(yelp)

table(Successful)

x= as.data.frame(table(Tone,Successful))
View(x)

rcorr(as.matrix(subset(x,Successful=='Yes')))
                
x1=subset(x,Successful=='Yes')
hist(subset(x,Successful=='Yes')$Freq,col='light blue',add=T)
hist(subset(x,Successful=='No')$Freq, col ='light pink', xlim=c(0,10000))

boxplot(PositiveTone ~ Successful, col='pink', xlab='Successful reviews', 
        ylab ='Positve Tone')
boxplot(PositiveTone ~ Tone, col='light blue', xlab='Tone', 
        ylab ='Positive Tone')

boxplot(drives ~ Successful, col='yellow', xlab='Successful reviews', 
        ylab ='% of words referencing different drives')
boxplot(drives ~ Tone, col='light green', xlab='Tone', 
        ylab ='% of words referencing different drives')
#aggregate(Successful,by=list(Tone),FUN=mean)

#Model 1
cor(yelp[c(12:17,25)])

rcorr(as.matrix(yelp[c(12:17,25)]))

rcorr(as.matrix(subset(yelp,Successful=='Yes')[c(12:17,25)]))
rcorr(as.matrix(subset(yelp,Successful=='No')[c(12:17,25)]))

pairs(subset(yelp,Successful=='Yes')[c(12:17,25)],col='green')
pairs(subset(yelp,Successful=='No')[c(11:17,25)],col='red')

# Model 2
cor(yelp[c(12:13,16:18,23,25)])

rcorr(as.matrix(yelp[c(12:13,16:18,23,25)]))

rcorr(as.matrix(subset(yelp,Successful=='Yes')[c(12:13,16:18,23,25)]))
rcorr(as.matrix(subset(yelp,Successful=='No')[c(12:13,16:18,23,25)]))

pairs(subset(yelp,Successful=='Yes')[c(12:13,16:18,23,25)],col='green')
pairs(subset(yelp,Successful=='No')[c(12:13,16:18,23,25)],col='red')


