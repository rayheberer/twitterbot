# a function that takes the text of a tweet as an input, determines its overall sentiment,
# and outputs a decision (that will hook into the twitter API) as to whether to retweet
# the tweet, ignore it, or respond to it

tweets = searchTwitter("@realDonaldTrump", n = 100, lang = "en")
tweets = strip_retweets(tweets) # if we want to only deal with tweets

# Andrew's series of tweet text cleaner functions, but without the data.frame conversion
tweets = sapply(tweets, function(x) x$getText())
tweets = gsub("[^[:print:]]", "", tweets)
tweets = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets)
tweets = gsub("[^[:space:]]*…$", "", tweets)

library(magrittr)
library(tidytext)
library(dplyr)
library(tm)

library(tidyr)


tweet.tidy = tweet.split %>% unnest_tokens(word, text)
#tweet = tweet %>% anti_join(stop_words)

tweet <- tweet %>% stripWhitespace
tweet <- tweet %>% content_transformer(tolower)
tweet <- tm_map(tweet, removePunctuation)
tweet <- tm_map(tweet, removeNumbers)


tweet = tweets[[1]]
tweet = tweet %>% removePunctuation %>% removeNumbers

tweet.split = tweet %>% strsplit(split = " ")
tweet.df <- data.frame(do.call(rbind, strsplit(tweet, " ", fixed=TRUE)), stringsAsFactors = FALSE)
tweet.tidy = tweet.df %>% gather() %>% mutate(index = 1:nrow(tweet.tidy)) %>% select(-key)
tweet.tidy = tweet.tidy %>% anti_join(stop_words)


sentiment = tweet.tidy %>% inner_join(get_sentiments("afinn"), by = c("value" = "word")) %>% 
  summarise(sentiment = sum(score)) %>% as.integer

test = c()
for (i in 1:length(tweets.txt)) {
  sentiments[i] = tweet_sentiment(tweets.txt[[i]])
}
test

twt = tweets.txt[[1]]

# START

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
  
  if (abs(sentiment) <= 4) {
    return("neutral")
  }
  
  if (sentiment < -4) {
    return("negative")
  }
}


