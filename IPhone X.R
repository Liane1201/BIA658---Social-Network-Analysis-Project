install.packages(c('tm', 'SnowballC', 'wordcloud'))
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)

tweets1 = read.csv("Data.csv", stringsAsFactors = F, row.names = 1)
#tweets2 = read.csv("Data11.3-2.csv", stringsAsFactors = F, row.names = 1)
tweets_total <- rbind(tweets1)
retweets <-tweets_total[which(tweets_total$isRetweet == 'TRUE'),]
noretweets <-tweets_total[which(tweets_total$isRetweet == 'FALSE'),]
retweets_uni <- data.frame(unique(retweets$text)) 
noretweets_uni <- data.frame(unique(noretweets$text))

noretweets_uni1 <- iconv(noretweets_uni, from = "latin1", to = "ascii", sub = "byte")

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
#定义两个function
count_matches <- function(string, matchto, description, sentiment = NA) {
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  descr <- NA
  cnt <- NA
  if (length(matches) != 0) {
    descr <- description[matches]
    cnt <- vec[matches]
  } 
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) & length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
  }
  return(df)
}

emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  
  texts %>% 
    lapply(count_matches, matchto = matchto, description = description, sentiment = sentiment) %>%
    bind_rows
  
}

wordFreqEmojis <- function(df, text = df$text, description = df$description, top = 5) {
  
  
  lapply(unique(description), function(x) {
    
    dat <- df %>% 
      filter(description == x)
    
    myCorpus <- Corpus(VectorSource(retweets_uni$text)) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(myCorpus)
    # find the sum of words in each Document
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    # collapse matrix by summing over columns
    freq <- colSums(as.matrix(dtm))
    # create sort order (descending)
    ord <- order(freq, decreasing=TRUE)
    
    list(emoji = rep(x, top), words = names(freq[ord][1:top]), frequency = freq[ord][1:top]) 
    
  }) %>% 
    bind_rows
  
}

emDict_raw <- read.csv2("emojis.csv") %>% 
  select(EN, ftu8, unicode) %>% 
  rename(description = EN, r.encoding = ftu8)

skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = as.u_char(unicode))

matchto <- emDict$r.encoding
description <- emDict$description

rank <- emojis_matching(noretweets_uni1, matchto, description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count)) %>%
  arrange(-n)

head(rank, 10)

retweets_uni$text<- sub(".*?:\\s+(.*)\\s+(https:.*)?", "\\1", retweets_uni$unique.retweets.text)
retweets_uni$text<- gsub("‰Ûª", "'", retweets_uni$text)
retweets_uni$text<- gsub("\n", "", retweets_uni$text)
retweets_uni$text<- gsub("&amp;", "", retweets_uni$text)
retweets_uni$text<- gsub("#[\\w]+(\\s)?", "", retweets_uni$text)
retweets_uni$text<- gsub("@[\\w]+\\b", "", retweets_uni$text)
#retweets_uni$text<- gsub("‰Û", "", retweets_uni$text)
#retweets_uni$text<- gsub("’Ê_’+\\b", "", retweets_uni$text)
#retweets_uni$unique.retweets.text[1:10]
retweets_uni$text[1:10]


retweets_uni_corpus = Corpus(VectorSource(retweets_uni$text))
retweets_uni_corpus = tm_map(retweets_uni_corpus, content_transformer(tolower)) 
retweets_uni_corpus = tm_map(retweets_uni_corpus, removePunctuation)
retweets_uni_corpus = tm_map(retweets_uni_corpus, stripWhitespace)
retweets_uni_corpus = tm_map(retweets_uni_corpus, removeNumbers)
retweets_uni_corpus = tm_map(retweets_uni_corpus, removeWords, c('iphone','phone','iphonex','x','apple','êü',stopwords('english')))
##ê,ü,ó,â,à,û,ª,á,è,î,ò,õ,ï,é

retweets_uni_corpus$content[1:10]

retweets_dtm = DocumentTermMatrix(retweets_uni_corpus, control = list(bounds = list(global = c(1, Inf))))
#retweets_dtm = removeSparseTerms(retweets_dtm, 0.95)
#inspect(retweets_dtm[1:100, sample(ncol(retweets_dtm), 100)]) 
findFreqTerms(retweets_dtm, 1000)
freq_re = data.frame(sort(colSums(as.matrix(retweets_dtm)), decreasing=TRUE))
wordcloud(rownames(freq_re), freq_re[,1], max.words=50, colors=brewer.pal(3, "Dark2"))

#retweets_dtm_tfidf = DocumentTermMatrix(retweets_uni_corpus, control = list(weighting = weightTfIdf))
#findFreqTerms(retweets_dtm_tfidf,1000)
#freq_tfidf =data.frame(sort(colSums(as.matrix(retweets_dtm_tfidf)), decreasing=TRUE))
#wordcloud(rownames(freq_tfidf), freq_tfidf[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

neg_words = read.table("negative-words.txt", header = F, stringsAsFactors = F)[, 1]
pos_words = read.table("positive-words.txt", header = F, stringsAsFactors = F)[, 1]

retweets_pos_dtm <- DocumentTermMatrix(retweets_uni_corpus, list(dictionary = pos_words))
#inspect(retweets_pos_dtm[1:100, sample(ncol(retweets_pos_dtm), 100)]) 
findFreqTerms(retweets_pos_dtm,100)
re_pos_freq = data.frame(sort(colSums(as.matrix(retweets_pos_dtm)), decreasing=TRUE))
wordcloud(rownames(re_pos_freq), re_pos_freq[,1], max.words=50, colors=brewer.pal(3, "Dark2"))

retweets_neg_dtm <- DocumentTermMatrix(retweets_uni_corpus,control = list(dictionary = neg_words))
inspect(retweets_neg_dtm[1:100, sample(ncol(retweets_neg_dtm), 100)]) 
findFreqTerms(retweets_neg_dtm,1000)
re_neg_freq = data.frame(sort(colSums(as.matrix(retweets_neg_dtm)), decreasing=TRUE))
wordcloud(rownames(re_neg_freq), re_neg_freq[,1], max.words=50, colors=brewer.pal(3, "Dark2"))


insertWords(dict)
