# a wrapper function to tweet_sentiment() that interacts with the twitter API,
# responding to and creating events in the API

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
  POST(url = paste(resource_url, tweet_id, ".json" , sep = ""))
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

negative_response = function(twt) {
  responses = c("Incorrect.", "I disagree.", "You are wrong.")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep="")
  updateStatus(response, inReplyTo = twt$id)
}

affirmative_response = function(twt) {
  responses = c("I agree.", "You're so right!", "Truth.")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep="")
  updateStatus(response, inReplyTo = twt$id)
}

twitterbot = function(handle) {
  
  tweets = searchTwitter(handle, n = 100, lang = "en")
  tweets = strip_retweets(tweets)
  tweets.txt = sapply(tweets, function(x) x$getText())
  tweets.txt = gsub("[^[:print:]]", "", tweets.txt)
  tweets.txt = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets.txt)
  tweets.txt = gsub("[^[:space:]]*…$", "", tweets.txt)
  
  tweets.txt = tweets.txt %>% removePunctuation %>% removeNumbers %>% tolower
  
  
  count = 0
  max_bot_actions = 3
  
  for (i in 1:length(tweets)) {
    if (tweet_sentiment(tweets.txt[[i]]) == "neutral") {
      next
    }
    
    else if (tweet_sentiment(tweets.txt[[i]]) == "negative") {
      affirmative_response(tweets[[i]])
      count = count + 1
    }
    
    else {
      negative_response(tweets[[i]])
      count = count + 1
    }
    
    if (count == max_bot_actions) {
      break
    }
  }
}