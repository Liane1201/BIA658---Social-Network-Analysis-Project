install.packages("rtweet")
library(rtweet)
install.packages("httpuv")
library(httr)
install.packages("ROAuth")
library(ROAuth)
install.packages("twitteR")
library(twitteR)


appname <- "Xinlian"
key<- "IUCHX3yPBcZW8bxJNivatrFfL"
secret<-"unsoeeQ8njN84y6YKlbcU7JExhHKj0qjs4VvqCf7GKRjRsOSxo"
Xinlian<- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
vignette("stream",package = "rtweet")
setup_twitter_oauth(key,secret)

data11.24 <- searchTwitter("IphoneX", lang = "en", n = 2000)
d11.24 = twListToDF(data11.24)
write.csv(d11.24, file = "/Users/xinlian/Desktop/data11.24.csv")
d_new <-cbind(data,data11.21)
length(data4)
d_total <-cbind(data4,data11.21, data11.23, data11.24)
