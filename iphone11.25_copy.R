library(tidyverse)
library(igraph)
library(stm)
library(tm)
library(twitteR)

iphone = read_csv('twitter11-25.csv')

# top retweets
top_retweet = iphone %>% arrange(desc(retweetCount))
top_retweet_names = top_retweet$text[1:200]

# analyze only top 200 retweets
tweet <- iphone %>% filter(text %in% top_retweet_names)

#remove useless charater columns
tweet1<-subset(tweet,select = -c(truncated))
tweet2<-subset(tweet1,select = -c(3:11))
tweet2<-subset(tweet1,select = -c(1))

#text cleasing
tweet2$text<-gsub("iphone","",tweet2$text)
tweet2$text<-gsub("iphonex","",tweet2$text)
tweet2$text<-gsub("iPhoneX","",tweet2$text)
tweet2$text<-gsub("iPhone X","",tweet2$text)
tweet2$text<-gsub("http[[:alnum:][:punct:]]*", "",tweet2$text)
tweet2$text<-gsub("RT","",tweet2$text)
tweet2$text<-gsub('\\p{So}|\\p{Cn}',"",tweet2$text,perl=T)
tweet2$text<-gsub("[^\x20-\x7E]","",tweet2$text)

tweet2$text

#=======
#Network of Frequent Terms
#=======

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library(igraph)
library(Rgraphviz)

myCorpus = Corpus(VectorSource(tweet2$text))
myCorpus = tm_map(myCorpus,content_transformer(tolower))
myCorpus = tm_map(myCorpus, content_transformer(removeNumbers)) #remove numbers
myCorpus = tm_map(myCorpus,removeWords, c("duh","wharever","rt","iphone","iphonex",stopwords("english")))
myCorpus = tm_map(myCorpus, removePunctuation) #remove punctuation
myCorpus = tm_map(myCorpus, stripWhitespace) 
myCorpus



#Build Term Document Matrix
tdm = TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
freq.terms<-findFreqTerms(tdm,lowfreq = 1)

dtm<-as.DocumentTermMatrix(tdm)
dtm

#install.packages("topicmodels")
library(topicmodels)
lda<-LDA(dtm,k=5) #find 3 topics
term<-terms(lda,10)#find 5 terms of every topic
term<-apply(term,MARGIN = 2, paste, collapse = ",")
term

topics<-topics(lda)
topics<-data.frame(date=as.Date(tweet1$created),topic = topics)
ggplot(topics,aes(date,fill=term[topic]))+geom_density(position='stack')

#===============
#Topic Modeling
#===============

#1.text preprocessing
tweet_meta = tweet2 %>% select(-text)
processed_text = textProcessor(tweet2$text, metadata = tweet_meta, stem = FALSE)
processed_text$docs.removed
out = prepDocuments(processed_text$documents, processed_text$vocab, processed_text$meta, lower.thresh = 3, upper.thresh = 1000) 
out$docs.removed
n_topics = 3
stm_fit = stm(documents = out$documents, vocab = out$vocab, K = n_topics)

labelTopics(stm_fit)
cloud(stm_fit, topic = 1)
cloud(stm_fit, topic = 3)

#=============
#Construct similar network of user 
#=============

topic_proportions = stm_fit$theta
colnames(topic_proportions) = paste("topic",1:n_topics,sep = "")
tweet_meta = tweet_meta[,'screenName']
tweet_meta = cbind(tweet_meta[1:179,],topic_proportions)

# for each user, compute his/her mean topic vector 计算每个user对topic的均值
user_topic = tweet_meta %>% group_by(screenName) #%>% summarise_at(vars(starts_with("topic")), mean)
#user_topic1<-subset(user_topic,select = -c(6:10))

# calculate cosine similarity between users
library('lsa')
cosine_sim = cosine(t(as.matrix(user_topic[,2:(n_topics+1)])))
sim = as.matrix(1/dist(as.matrix(user_topic[,2:(n_topics+1)])))
sim


# From similarity to adj matrix method 1. Using threshold
adj_matrix = sim > 100
colnames(adj_matrix) = user_topic$screenName
rownames(adj_matrix) = user_topic$screenName
user_graph = graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
user_graph = simplify(user_graph)

dev.off()
plot(user_graph, vertex.size = 3, layout = layout.fruchterman.reingold(user_graph))


set_top_n_to_one = function(v, n = 3){
  # given a vector, set top n values to 0, others to 0
  v[-order(v, decreasing = T)[1:n]] = 0
  v[order(v, decreasing = T)[1:n]] = 1
  return(v)
}

adj_matrix = matrix(NA, 179, 179)
for(i in 1:179){
  adj_matrix[i, ] = set_top_n_to_one(sim[i,])
}

adj_matrix
colnames(adj_matrix) = user_topic$screenName
rownames(adj_matrix) = user_topic$screenName
user_graph = graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
user_graph = simplify(user_graph)
delete.isolates<-function(graph,mode='all'){
  isolates<-which(degree(user_graph,mode = mode) <6 )
  delete.vertices(user_graph,isolates)
}
plot(user_graph, vertex.size = 3, layout = layout.fruchterman.reingold(user_graph))







