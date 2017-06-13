#!/usr/bin/Rscript


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


retweet = function(twt) {
  resource_url = "https://api.twitter.com/1.1/statuses/retweet/"
  tweet_id = twt$id
  POST(paste(resource_url, tweet_id, ".json", sep = ""))
}

tweet_sentiment = function(twt) {
  tweet.df = data.frame(do.call(rbind, strsplit(twt, " ", fixed=TRUE)), stringsAsFactors = FALSE)
  tweet.tidy = tweet.df %>% gather() 
  tweet.tidy = tweet.tidy %>% mutate(index = 1:nrow(tweet.tidy)) %>% select(-key)
  
  sentiment = inner_join(get_sentiments("afinn"), tweet.tidy, by = c("word" = "value"))
  
  if (nrow(sentiment) == 0) {
    sentiment = 0
  } else {
    sentiment = sum(sentiment$score) 
  }
  
  if (abs(sentiment) <= 3) {
    return("neutral")
  }
  
  if (sentiment < -3) {
    return("negative")
  }
  
  if (sentiment > 3) {
    return("positive")
  }
}

tweet_responses = function(twt) {
  return("You are wrong.")
}

twitterbot = function(handle) {
  
  tweets = searchTwitter("@realDonaldTrump", n = 100, lang = "en")
  tweets = strip_retweets(tweets) # if we want to only deal with tweets
  
  # Andrew's series of tweet text cleaner functions, but without the data.frame conversion
  tweets.txt = sapply(tweets, function(x) x$getText())
  tweets.txt = gsub("[^[:print:]]", "", tweets.txt)
  tweets.txt = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets.txt)
  tweets.txt = gsub("[^[:space:]]*…$", "", tweets.txt)
  
  tweets.txt = tweets.txt %>% removePunctuation %>% removeNumbers %>% tolower
  
  
  # set max_bot_actions to be equal to however many tweets and retweets the bot should make at most per cron task
  count = 0
  max_bot_actions = 3
  
  # main loop, will terminate early if bot takes max_bot_actions
  for (i in 1:length(tweets)) {
    if (tweet_sentiment(tweets.txt[[i]]) == "neutral") {
      next #move onto the next tweet if sentiment is indeterminate
    }
    
    else if (tweet_sentiment(tweets.txt[[i]]) == "negative") {
      retweet(tweets[[i]])
      count = count + 1
    }
    
    else {
      updateStatus(text = tweet_responses(tweets.txt[[i]]), inReplyTo = tweets[[i]]$id)
      count = count + 1
    }
    
    if (count == max_bot_actions) {
      break
    }
  }
}