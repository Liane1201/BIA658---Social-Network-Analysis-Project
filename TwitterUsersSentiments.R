install.packages(c('tm','SnowballC','wordcloud','NLP','RColorBrewer'))
install.packages("textcat")
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(textcat)

tweets = read.csv("/Users/xinlian/Desktop/658/TwitterProject/Data.csv",stringsAsFactors = F)
tweets1 = tweets[!duplicated(tweets$text),]
tweets1_corpus = Corpus(VectorSource(tweets$text))
tweets1_corpus[[1]]
tweets1_corpus = tm_map(tweets1_corpus,content_transformer(tolower))

tweets1_corpus = tm_map(tweets1_corpus, removeNumbers) #remove numbers
tweets1_corpus = tm_map(tweets1_corpus, removePunctuation) #remove punctuation
tweets1_corpus = tm_map(tweets1_corpus,removeWords, c("duh","wharever","rt","ihonex",stopwords("english")))
tweets1_corpus = tm_map(tweets1_corpus, stripWhitespace) 

tweets1_corpus[[1]]$

tweets1_dtm = DocumentTermMatrix(tweets1_corpus, control = list(bounds = list(global = c(1, Inf))))
tweets1_dtm 
inspect(tweets1_dtm[1:10, sample(ncol(tweets1_dtm), 10)]) 


# Simple word cloud
findFreqTerms(tweets1_dtm, 1000) 
freq = data.frame(sort(colSums(as.matrix(tweets1_dtm)), decreasing=TRUE))#排序
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
