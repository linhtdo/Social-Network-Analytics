library(tm)

# Read csv file of frequent terms
Rem = read.csv('Rem.csv')

#Create Corpus
RemCorp = Corpus(VectorSource(Rem$words))

inspect(RemCorp)

# Create Document Term Matrix
RemDTM = DocumentTermMatrix(RemCorp)
View(as.matrix(RemDTM))

findFreqTerms(RemCorp,75)

# Create a Term Document Matrix
RemTDM = TermDocumentMatrix(RemCorp)
View(as.matrix(RemTDM))


# Get Term Frequencies
# Find terms that occur at least five times in the corpus/DTM
findFreqTerms(RemDTM, 75)


#retain terms that occur in at least 5% of documents
RemDTM = removeSparseTerms(RemDTM, 0.95)
View(as.matrix(t(RemDTM)))
  # No data available??

#findFreqTerms(RemDTM, 5)

# Word Cloud Visualizations
library(wordcloud)

#create a wordcloud of terms that occur at least 50 times in the corpus
wordcloud(RemCorp, min.freq = 50)

#create a wordcloud of terms that occur at least 200 times in the corpus
wordcloud(RemCorp, min.freq = 200)


# Fancier word cloud
library(wordcloud2)

#wordcloud2 wants the data in a different orientation
RemSum=rowSums(as.matrix(TermDocumentMatrix(RemCorp, control=list(stopwords=T))))
#create a dataframe in which terms and frequencies are data columns
RemSum=as.data.frame(cbind(row.names(as.matrix(RemSum)),as.matrix(RemSum)))
#name the columns so they can be referenced
colnames(RemSum)=c('word','freq')

#ensure our data is of the correct data types because wordcloud2 is persnickety
RemSum$freq=as.numeric(as.character(RemSum$freq))
RemSum=as.list(RemSum)

letterCloud(RemSum, 'MONEY')           #create a wordcloud in the shape of a specified word


library(syuzhet)

#create a data frame from our RemCorp corpus 
RemDF=data.frame(text=sapply(RemCorp, as.character), stringsAsFactors = FALSE)
View(RemDF)
#get a sentiment value for the first document based on the default
#syuzhet algorithm
get_sentiment(as.character(RemDF$text[1]))
# =0?
#repeat for second document
get_sentiment(as.character(RemDF$text[2]))

#algorithms for each speech
get_sentiment(as.character(RemDF$text[1]), method = 'bing')

#RemPolarity=data.frame(Syuzhet=rbind(get_sentiment(as.character(RemDF$text[1]),method='syuzhet'), get_sentiment(as.character(RemDF$text[2]), method='syuzhet')))

