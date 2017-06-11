# a wrapper function to tweet_sentiment() that interacts with the twitter API,
# responding to and creating events in the API

library(twitteR)
library(httr)

consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

retweet = function(twt) {
  resource_url = "https://api.twitter.com/1.1/statuses/retweet/"
  tweet_id = twt$id
  POST(paste(resource_url, tweet_id, sep = ""))
}

twitterbot = function(handle) {
  
  tweets = searchTwitter(handle, n = 100, lang = "en")
  tweets = strip_retweets(tweets) # if we want to only deal with tweets
  
  # Andrew's series of tweet text cleaner functions, but without the data.frame conversion
  tweets = sapply(tweets, function(x) x$getText())
  tweets = gsub("[^[:print:]]", "", tweets)
  tweets = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets)
  tweets = gsub("[^[:space:]]*…$", "", twitter.text)
  
  # set max_bot_actions to be equal to however many tweets and retweets the bot should make at most per cron task
  count = 0
  max_bot_actions = 10
  
  # main loop, will terminate early if bot takes max_bot_actions
  for (i in 1:length(tweets)) {
    if (tweet_sentiment(tweets[[i]]) == "neutral") {
      next #move onto the next tweet if sentiment is indeterminate
    }
    
    else if (tweet_sentiment(tweets[[i]]) == "negative") {
      retweet(tweets[[i]])
      count = count + 1
    }
    
    else {
      updateStatus(text = tweet_responses(tweets[[i]]), inReplyTo = tweets[[i]]$id)
      count = count + 1
    }
    
    if (count == max_bot_actions) {
      break
    }
  }
}

dev.off()