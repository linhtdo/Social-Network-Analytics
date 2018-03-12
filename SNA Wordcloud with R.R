######################################################################################################################
# I: LOADING AND PRE-PROCESSING TEXT DATA
######################################################################################################################

########################### A. Opening and Inspecting a Corpus #######################################################

#setwd('FILEPATH')                          #change FILEPATH and '/', '\ ' in path specification
#create a data frame with three documents
library(tm)
MyDF=data.frame(doc_id=c('Doc1', 'Doc2', 'Doc3'), text=c('This is a text.', 'This is another one.', 'And this is a third.'))
#create a corpus from the data frame using DataframeSource
MyCorp=Corpus(DataframeSource(MyDF))
inspect(MyCorp)				    #inspect the corpus

#create a corpus by scanning a website using VectorSource
MobyDickCorp = Corpus(VectorSource(scan('http://www.gutenberg.org/files/2701/2701-0.txt', what='character', sep='\n')))
inspect(MobyDickCorp)			    #display the first 1000 documents
content(MobyDickCorp)[1001:1010]	    #display specified document - here 1001-1010

ReyPosts=read.csv('WheresRey.csv',header=T) #read twitter posts from file
#create a corpus from the Posts column using VectorSource
ReyCorp=Corpus(VectorSource(ReyPosts$Posts))

SpeechCorp=Corpus(DirSource('Speeches'))    #create corpus from text files in directory using DirSource
inspect(SpeechCorp)

########################### B. Pre-process Corpus and Create DTM/TDM ###############################################

library(tm)

SpeechDTM=DocumentTermMatrix(SpeechCorp)    #create a document term matrix from the SpeechCorp corpus
View(as.matrix(SpeechDTM))		    #view the DTM
View(as.matrix(t(SpeechDTM)))		    #view the transpose of the DTM
SpeechTDM=TermDocumentMatrix(SpeechCorp)    #create a term document matrix from the SpeechCorp corpus
View(as.matrix(SpeechTDM))		    #notice that TDM is the same as t(DTM)

#pre-process corpus, removing punctuatioon and numbers
SpeechDTM=DocumentTermMatrix(SpeechCorp, control=list(removePunctuation=T, removeNumbers=T))
#pre-process corpus, converting it to lowercase
SpeechCorp=tm_map(SpeechCorp, content_transformer(tolower))
SpeechDTM=DocumentTermMatrix(SpeechCorp, control=list(removePunctuation=T, removeNumbers=T))

View(as.matrix(SpeechDTM))

########################### C. Get Term Frequencies ################################################################

findFreqTerms(SpeechDTM, 5)		    #find terms that occur at least five times in the corpus/DTM

#pre-process corpus, removing custom stopwords
SpeechCorp=tm_map(SpeechCorp, removeWords, c('ago','all','and','civil','come'))

#pre-process corpus while creating the DTM
SpeechDTM=DocumentTermMatrix(SpeechCorp, control=list(removePunctuation=T, removeNumbers=T))
findFreqTerms(SpeechDTM, 5)		    

#retain terms that occur in at least 75% of documents
SpeechDTM=removeSparseTerms(SpeechDTM, 0.25)
View(as.matrix(t(SpeechDTM)))		    #view the transpose of the DTM
findFreqTerms(SpeechDTM, 5)		    #did these change?

SpeechTerms=colSums(as.matrix(SpeechDTM))   #sum the columns in the DTM and store in SpeechTerms 
SpeechTerms=sort(SpeechTerms, decreasing=T) #sort in descending order
SpeechTerms[1:10]			    #view the ten most frequently-occurring terms

#change from the default TF weighting to a TfIdf weighting
#this down-weights commonly-used terms in the corpus
SpeechDTM=DocumentTermMatrix(SpeechCorp, control=list(removePunctuation=T, removeNumbers=T, weighting=weightTfIdf))
SpeechTerms=colSums(as.matrix(SpeechDTM))
SpeechTerms=sort(SpeechTerms, decreasing=T)
SpeechTerms[1:10]

######################################################################################################################
# II: WORDCLOUDS
######################################################################################################################

########################### A. Simple Wordclouds #####################################################################

library(wordcloud)

wordcloud(SpeechCorp, min.freq = 50)        #create a wordcloud of terms that occur at least 50 times in the corpus
#get some control over the wordcloud using the SpeechTerms data object
#and make it pretty
wordcloud(words=names(SpeechTerms), freq=SpeechTerms, max.words=75, vfont=c('serif','bold italic'), colors=brewer.pal(8, 'Dark2'))

#why I hate stemming! ==> Because you may end up with meaningless words
SpeechDTM=DocumentTermMatrix(SpeechCorp, control=list(removePunctuation=T, removeNumbers=T, weighting=weightTfIdf, stemming=T))
SpeechTerms=colSums(as.matrix(SpeechDTM))
SpeechTerms=sort(SpeechTerms, decreasing=T)

par(mar=c(2,2,2,2))
wordcloud(words=names(SpeechTerms), freq=SpeechTerms, max.words=75, vfont=c('serif','bold italic'), colors=brewer.pal(8, 'Dark2'))

########################### B. Comparison and Commonality Wordclouds ##################################################

#a simple comparison cloud of the two speeches
comparison.cloud(as.matrix(TermDocumentMatrix(SpeechCorp, control=list(removePunctuation=T, weighting=weightTfIdf))))

#pretty it up
comparison.cloud(as.matrix(TermDocumentMatrix(SpeechCorp, control=list(removePunctuation=T, weighting=weightTfIdf))), 
                 colors=brewer.pal(9,'Set1'), title.size= 1.5, vfont=c('serif','italic'),scale=c(2.5,.25), max.words = 500)

#a commonality cloud of terms appearing in both speeches
commonality.cloud(as.matrix(TermDocumentMatrix(SpeechCorp, control=list(removePunctuation=T))), colors='slateblue3')

########################### C. Fancy Wordclouds #######################################################################

#I had to re-install wordcloud2 from github to get it working today
install.packages('devtools')
library(devtools)
install_github("lchiffon/wordcloud2")

library(wordcloud2)

#wordcloud2 wants the data in a different orientation
SpeechSum=rowSums(as.matrix(TermDocumentMatrix(SpeechCorp, control=list(stopwords=T))))
#create a dataframe in which terms and frequencies are data columns
SpeechSum=as.data.frame(cbind(row.names(as.matrix(SpeechSum)),as.matrix(SpeechSum)))
#name the columns so they can be referenced
colnames(SpeechSum)=c('word','freq')
#ensure our data is of the correct data types because wordcloud2 is persnickety
SpeechSum$freq=as.numeric(as.character(SpeechSum$freq))
SpeechSum=as.list(SpeechSum)

letterCloud(SpeechSum, 'Inspire')           #create a wordcloud in the shape of a specified word

#to create a wordcloud in the shape of a picture, we first must store the
#picture - aka, a mask - in the wordcloud2 path, then reference the 
#path so R knows where to find it
LBPath = system.file('examples/LibertyBell.jpg', package='wordcloud2')
#create a picture wordcloud usingn our mask
wordcloud2(SpeechSum, figPath=LBPath, color= ifelse(SpeechSum$freq>10,'red','darkblue'))
wordcloud2(SpeechSum, figPath=LBPath)

######################################################################################################################
# III: SENTIMENT ANALYSIS
######################################################################################################################

library(syuzhet)

#create a data frame from our SpeechCorp corpus using the sapply() 
#function, which converts the corpus to a vector matrix, and then
#smooshing it into a data frame
SpeechDF=data.frame(text=sapply(SpeechCorp, as.character), stringsAsFactors = FALSE)
View(SpeechDF)
#get a sentiment value for the first document based on the default
#syuzhet algorithm
get_sentiment(as.character(SpeechDF$text[1]))
#repeat for second document
get_sentiment(as.character(SpeechDF$text[2]))

#make a SpeechPolarity consisting of sentiment scores based on four
#algorithms for each speech
get_sentiment(as.character(SpeechDF$text[1]), method = 'bing')

SpeechPolarity=data.frame(Syuzhet=rbind(get_sentiment(as.character(SpeechDF$text[1]),method='syuzhet'), get_sentiment(as.character(SpeechDF$text[2]), method='syuzhet')))
SpeechPolarity=data.frame(SpeechPolarity, Bing= rbind(get_sentiment(as.character(SpeechDF$text[1]), method='bing'), get_sentiment(as.character(SpeechDF$text[2]), method='bing')))
SpeechPolarity=data.frame(SpeechPolarity, AFinn= rbind(get_sentiment(as.character(SpeechDF$text[1]),method='afinn'), get_sentiment(as.character(SpeechDF$text[2]),method='afinn')))
SpeechPolarity=data.frame(SpeechPolarity, NRC= rbind(get_sentiment(as.character(SpeechDF$text[1]), method='nrc'), get_sentiment(as.character(SpeechDF$text[2]), method='nrc')))
View(SpeechPolarity)
#use the substr function, which is equivalent to Excel's MID function,
#and the nchar function, which is equivalent to Excel's LEN function,
#to display the speech name, rather than the document name
SpeechPolarity=data.frame(Speech=substr(as.character(row.names(SpeechDF)), 1, nchar(as.character(row.names(SpeechDF)))-4), SpeechPolarity)
SpeechPolarity				    #view the sentiment scores

#get emotion scores, rather than polar sentiment scores, and put them in a
#tidy data frame
get_nrc_sentiment(as.character(SpeechDF$text[1]))
SpeechEmot=rbind(get_nrc_sentiment(as.character(SpeechDF$text[1])), get_nrc_sentiment(as.character(SpeechDF$text[2])))
SpeechEmot=data.frame(Speech=substr(as.character(row.names(SpeechDF)),1, nchar(as.character(row.names(SpeechDF)))-4),SpeechEmot)
SpeechEmot

get_nrc_sentiment(as.character(ReyPosts$Posts[1]))
