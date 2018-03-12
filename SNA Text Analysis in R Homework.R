library(tm)
# Read Crypto Tweets from file
Crypto = read.csv('Crypto.csv', header = T)
# Create a corpus from the tweets
CryptCorp = Corpus(VectorSource(Crypto$Posts))
inspect(CryptCorp)

# Create Document Term Matrix
CrypDTM = DocumentTermMatrix(CryptCorp)
View(as.matrix(CrypDTM))
View(as.matrix(t(CrypDTM)))
# Create a Term Document Matrix
CrypTDM = TermDocumentMatrix(CryptCorp)
View(as.matrix(CrypTDM))

#pre-process corpus, removing punctuatioon and numbers
CrypDTM = DocumentTermMatrix(CryptCorp, control = list(removePunctuation=T, removeNumbers=T))

#pre-process corpus, converting it to lowercase
CryptCorp = tm_map(CryptCorp, content_transformer(tolower))
CrypDTM = DocumentTermMatrix(CryptCorp, control = list(removePunctuation=T, removeNumbers=T))

View(as.matrix(CrypDTM))

# Get Term Frequencies
# Find terms that occur at least five times in the corpus/DTM
findFreqTerms(CrypDTM, 5)

# Removing custom stopwords
CryptCorp=tm_map(CryptCorp, removeWords, c('ago','all','and','come','about','a','an','the','over','for','but','so','to','are','you','that','with','https'))

CrypDTM = DocumentTermMatrix(CryptCorp, control = list(removePunctuation=T, removeNumbers=T))
findFreqTerms(CrypDTM, 5)

#retain terms that occur in at least 10% of documents
CrypDTM = removeSparseTerms(CrypDTM, 0.90)
View(as.matrix(t(CrypDTM)))

findFreqTerms(CrypDTM, 5)

#retain terms that occur in at least 5% of documents
CrypDTM = removeSparseTerms(CrypDTM, 0.95)
View(as.matrix(t(CrypDTM)))

findFreqTerms(CrypDTM, 5)

#sum the columns in the DTM and store in CryptTerms 
CryptTerms=colSums(as.matrix(CrypDTM))   
CryptTerms=sort(CryptTerms, decreasing=T) #sort in descending order
CryptTerms[1:100]	# view the 100 most frequently-occurring terms

# Word Cloud Visualizations
library(wordcloud)

#create a wordcloud of terms that occur at least 50 times in the corpus
wordcloud(CryptCorp, min.freq = 50)

#create a wordcloud of terms that occur at least 70 times in the corpus
wordcloud(CryptCorp, min.freq = 70)

# Customization and control using CryptTerms
wordcloud(words=names(CryptTerms), freq=CryptTerms, max.words=75, vfont=c('serif','bold italic'), colors=brewer.pal(8, 'Dark2'))

# Try Stemming:
CrypDTM = DocumentTermMatrix(CryptCorp, control = list(removePunctuation=T,removeNumbers=T, weighting=weightTfIdf, stemming=T))
CryptTerms = colSums(as.matrix(CrypDTM))
CryptTerms = sort(CryptTerms, decreasing = T)

par(mar=c(4,4,4,4))
wordcloud(words=names(CryptTerms), freq=CryptTerms, max.words=75, vfont=c('serif','bold italic'), colors=brewer.pal(8, 'Dark2'))


# Fancier word cloud
library(wordcloud2)

#wordcloud2 wants the data in a different orientation
CryptSum=rowSums(as.matrix(TermDocumentMatrix(CryptCorp, control=list(stopwords=T))))
#create a dataframe in which terms and frequencies are data columns
CryptSum=as.data.frame(cbind(row.names(as.matrix(CryptSum)),as.matrix(CryptSum)))
#name the columns so they can be referenced
colnames(CryptSum)=c('word','freq')
#ensure our data is of the correct data types because wordcloud2 is persnickety
CryptSum$freq=as.numeric(as.character(CryptSum$freq))
CryptSum=as.list(CryptSum)

letterCloud(CryptSum, 'CRYPTO')           #create a wordcloud in the shape of a specified word


#to create a wordcloud in the shape of a picture, we first must store the
#picture - aka, a mask - in the wordcloud2 path, then reference the 
#path so R knows where to find it
CryptPath = system.file('examples/btc.png', package='wordcloud2')
#create a picture wordcloud usingn our mask
wordcloud2(CryptSum, figPath=CryptPath, color= ifelse(CryptSum$freq>10,'red','darkblue'))
wordcloud2(CryptSum, figPath=CryptPath)
View(CryptSum)
