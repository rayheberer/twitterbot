library(twitteR)
library(httr)
library(magrittr)
library(tidytext)
library(dplyr)
library(tm)
library(tidyr)

consumer_key = "dWVyRgkoXtNukY3cHKT7Gq1lb"
consumer_secret = "Bmgb4N5xLiyF5oEAcJ2KwjniiFki7iescrUHUDCS7KHrPR4mOo"
access_token = "872426899271815168-P7O3oaadMoEMJ2runZeMXVZFmqH3dxi"
access_secret = "AZEnBvebNG8dvqwVkA24r6kFvf2L16eNtNipn1sO50uYl"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets = searchTwitter("@realDonaldTrump", n = 1000, lang = "en")
tweets = strip_retweets(tweets)
tweets.txt = sapply(tweets, function(x) x$getText())
tweets.txt = gsub("[^[:print:]]", "", tweets.txt)
tweets.txt = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets.txt)
tweets.txt = gsub("[^[:space:]]*…$", "", tweets.txt)

tweets.txt = tweets.txt %>% removePunctuation %>% removeNumbers %>% tolower

tweet_sentiments = function(twt) {
  tweet.df = data.frame(do.call(rbind, strsplit(twt, " ", fixed=TRUE)), stringsAsFactors = FALSE)
  tweet.tidy = tweet.df %>% gather() 
  tweet.tidy = tweet.tidy %>% mutate(index = 1:nrow(tweet.tidy)) %>% select(-key)
  
  sentiment = inner_join(get_sentiments("afinn"), tweet.tidy, by = c("word" = "value"))
}

tweet_sentiment = lapply(tweets.txt, tweet_sentiments)

tweet_scores = c()
for (i in 1:length(tweet_sentiment)) {
  if (length(tweet_sentiment[[i]]$score) == 0) {
    tweet_scores = c(tweet_scores, 0)
    next
  }
  tweet_scores = c(tweet_scores, sum(tweet_sentiment[[i]]$score))
}


tweets.df = data.frame(tweets = tweets.txt, score = tweet_scores)
