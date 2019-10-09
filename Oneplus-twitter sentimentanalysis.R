setwd("C:/Users/admin/Desktop/R")

#Extracting Tweets using #hashtag "OnePlus 7". Credentails have been masked. 
#The tweets we are using is only applicable for Inida and taken on 16th May around 8:32 AM IST

#library(twitteR)
#library(RCurl)
#library(tm)
#library(wordcloud)
#consumer_key <- '###########################'
#consumer_secret <- '###############################################'
#access_token <- '##################################################'
#access_secret <-'###############################################'
#setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
#Oneplus_pre <- searchTwitter("OnePlus 7",n=10000, lang = "en")
#getwd()
#write.csv(df, file = "tweetsfile-on7.csv")




#Reading the file
oneplus_tweets <- read.csv("tweetsfile-on7.csv")
oneplus_tweets <- oneplus_tweets[, c(1,2)]
str(oneplus_tweets)
oneplus_tweets$text <- as.character(oneplus_tweets$text)

#Building a Corpus. Corpus is a collection of documents. 
#Each tweet is treated as document.
#iconv-Convert character vector between Encodings.
#VectorSource intrepets each element in tweet as document
library(tm)
oneplus_corpus<-iconv(oneplus_tweets$text,to="UTF-8")
oneplus_corpus<-Corpus(VectorSource(oneplus_corpus))
inspect(oneplus_corpus[1:5])
inspect(oneplus_corpus[9996:10000])

####################################
###########Data Cleaning############
####################################


#Convert all upper case to lower case
oneplus_corpus<-tm_map(oneplus_corpus,tolower)
inspect(oneplus_corpus[1:5])
inspect(oneplus_corpus[2124])

#Remove usernames
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)  
oneplus_corpus <- tm_map(oneplus_corpus, content_transformer(removeUsername))


#Remove Punctuation
oneplus_corpus<-tm_map(oneplus_corpus,removePunctuation)
inspect(oneplus_corpus[34])
inspect(oneplus_corpus[1:5])
inspect(oneplus_corpus[9996:10000])

#Remove Numbers
oneplus_corpus<-tm_map(oneplus_corpus,removeNumbers)
inspect(oneplus_corpus[1:5])
inspect(oneplus_corpus[9996:10000])

#Remove Stopwords
cleanset<- tm_map(oneplus_corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])
inspect(oneplus_corpus[9996:10000])

#Remove URLs
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
inspect(oneplus_corpus[9996:10000])

#Removing the most casual words
cleanset <- tm_map(cleanset, removeWords, c('rt', 'eduaubeedubu','manukumarjain','phoneradarblog','eduaubdedubud','oneplus','mrwhosetheboss','ive','onep','urvashirautela','ill','technicalguruji','teamoneplus','ishanagarwal','oneplusindia','oneplususa','oneplusuk','quiztimemorningswithamazon','geekyranjit', 'eduaubdedubub','eduaubdedubue','edu00a0u00bdedu00b3u00b9','eduaubdedubuaeduaubdedubuaeduaubdedubua','xdadevelopers','eduaubdedubu','eduaubdedubuaeduaubdedubuaeduaubdedubua','eduaubdedubua', 'oneplusseries','series','oneplusin','pro',
                                            'can','get','will','want','reply','onepluspro','mah',
                                            'tweet','upto','amazonin','one','plus','may','retweet',
                                            'win','just','free','guess','specs','mkbhd','india','new',
                                            'youtube','today','dont','video','need','via','silly','verge',
                                            'full','read','getting','time','oneplus','now','dont','flagship','phone'))


#Remove anything except the english language and space
removeNumPunct <- function(x) gsub("[^[:alnum:][:space:]]*", "", x)   
cleanset <- tm_map(cleanset, content_transformer(removeNumPunct))
inspect(cleanset[1:5])
inspect(cleanset[9996:10000])


#####Remove Single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
cleanset <- tm_map(cleanset, content_transformer(removeSingle))
writeLines(strwrap(cleanset[[3]]$content,60))


###Replacing words with correct ones
replaceWord <- function(cleanset, oldword, newword)
{
 tm_map(cleanset, content_transformer(gsub), pattern = oldword, replacement=newword , fixed=TRUE)
}

 backup -> cleanset
 
 gsub(cleanset ,"cha", "charge" )

cleanset<- replaceWord(cleanset, "cha", "charge")
cleanset<- replaceWord(cleanset, "launched", "launch")
cleanset<- replaceWord(cleanset, "launching", "launch")
cleanset<- replaceWord(cleanset, "oneplusserieslaunch", "launch")
cleanset<- replaceWord(cleanset, "onepluspopup", "popup")
cleanset<- replaceWord(cleanset, "popup", "pop")
cleanset<- replaceWord(cleanset, "gbgb", "gb")
cleanset<- replaceWord(cleanset, "cost", "price")
cleanset<- replaceWord(cleanset, "pricing", "price")
cleanset<- replaceWord(cleanset, "prices", "price")
cleanset<- replaceWord(cleanset, "lookss", "looks")
cleanset<- replaceWord(cleanset, "phones", "smartphones")
cleanset<- replaceWord(cleanset, "cameras", "camera")
cleanset<- replaceWord(cleanset, "iphone", "apple")
cleanset<- replaceWord(cleanset, "processor", "snapdragon")
cleanset<- replaceWord(cleanset, "processors", "snapdragon")
cleanset<- replaceWord(cleanset, "snapdragons", "snapdragon")
cleanset<- replaceWord(cleanset, "smartphones", "smartphone")
cleanset<- replaceWord(cleanset, "charging", "charge")
cleanset<- replaceWord(cleanset, "charges", "charge")
cleanset<- replaceWord(cleanset, "chargers", "charger")
cleanset<- replaceWord(cleanset, "qhd", "fluidamoled")
cleanset<- replaceWord(cleanset, "selfie", "pop")
cleanset<- replaceWord(cleanset, "galaxy", "samsung")

#Remove whitespace
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])
inspect(cleanset[9996:10000])


#Copy of Corpus
cleanset -> myCorpusCopy


###########################################################################

# Term document matrix - Converting features into rows and columns
tdmone <- TermDocumentMatrix(cleanset)
tdmone
tdmone <- as.matrix(tdmone)
tdmone[1:10, 1:20]

# Document Term Matrix - Converting features into rows and columns

dtm <- DocumentTermMatrix(cleanset)
dtm
dtm <- as.matrix(dtm)
dtm[1:10, 1:20]

#Get the frequency of the words
wordcount <- colSums(dtm)
wordcount <- sort(colSums(dtm), decreasing = TRUE)
wordcount <- subset(wordcount, wordcount>100)
View(wordcount)
barplot(wordcount,
        las = 2,
        col = rainbow(100))


p2
#WordCloud
library(wordcloud)
wordcount <- sort(colSums(dtm), decreasing = TRUE)
set.seed(222)
wordcloud(words=names(wordcount),
          freq=wordcount,
          max.words = 100,
          random.order = F,
          min.freq = 10,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(2, 1),
          rot.per = 0.7)

library(wordcloud2)
install.packages("wordcloud2")
wordcount <- data.frame(names(wordcount), wordcount)
colnames(wordcount) <- c('word', 'freq')
wordcloud2(wordcount,
           size = 0.4,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

#Find association with a specific keyword in the tweets - camera,battery,powers
#####Need to work on this associations################
tdmasso <- TermDocumentMatrix(cleanset)
dtmasso <- DocumentTermMatrix(cleanset)

 cordf1 <- t(data.frame(t(sapply(findAssocs(tdmasso,"battery",0.2),c))))
 barplot(t(as.matrix(cordf6)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "New",border = "black")
 cordf2 <- t(data.frame(t(sapply(findAssocs(tdmasso,"review",0.2),c))))
cordf3 <-  t(data.frame(t(sapply(findAssocs(tdmasso,"camera",0.2),c))))
 cordf4 <- t(data.frame(t(sapply(findAssocs(tdmasso,"screen",0.2),c))))
cordf5 <-  t(data.frame(t(sapply(findAssocs(tdmasso,"price",0.2),c))))
cordf6 <- t(data.frame(t(sapply(findAssocs(tdmasso,"pixel",0.2),c))))


##### Topic Modelling to identify latent/hidden topics using LDA technique

library(topicmodels)
rowTotals <- apply(dtmasso , 1, sum)
NullDocs <- dtmasso[rowTotals==0, ]
dtmasso   <- dtmasso[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  oneplus_tweets <- oneplus_tweets[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtmasso, k =4 )# find 5 topic
term <- terms(lda, 5) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

#k=4, terms= 5 

topics<- topics(lda)
topics<- data.frame(date=(oneplus_tweets$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")


####################################################################

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tm)


oneplus_tweets$text <- as.character(oneplus_tweets$text)

# Obtain sentiment scores
mysentiment <- get_nrc_sentiment(oneplus_tweets$text)
s -> mysentiment
tail(mysentiment)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot
senti <- sort(colSums(mysentiment), decreasing = TRUE)
barplot(senti,
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Oneplus Tweets')





