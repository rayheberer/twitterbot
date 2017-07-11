# a wrapper function to tweet_sentiment() that interacts with the twitter API,
# responding to and creating events in the API

library(twitteR)
library(httr)
library(magrittr)
library(tidytext)
library(dplyr)
library(tm)
library(tidyr)

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
  
  if (sentiment > 4) {
    return("positive")
  }
}

negative_response = function(twt) {
  responses = c("Incorrect.", "I disagree.", "You are wrong.", 
                "That's ridiculous.", "No. Just no.",
                "This makes me want to vomit.", "I'm afraid you're mistaken.")
  sample(responses, size = 1)
}

affirmative_response = function(twt) {
  responses = c("I agree.", "You're so right!", "Truth.", 
                "Well said!", "That's spot on.", "Agreed.", "Agreed!",
                "We are in agreement.", "Finally, someone sane.")
  sample(responses, size = 1)
}


retweet = function(twt, comment=NULL) {
  url = paste0('https://twitter.com/', twt$screenName, '/status/', twt$id)
  updateStatus(paste(comment, url), bypassCharLimit = TRUE)
}

reply = function(twt, content) {
  content = paste0("@", twt$screenName, " ", content)
  updateStatus(content, inReplyTo = twt$id)
}

twitterbot_respond = function(handle) {
  
  tweets = searchTwitter(handle, n = 100, lang = "en")
  tweets = strip_retweets(tweets)
  tweets.txt = sapply(tweets, function(x) x$getText())
  tweets.txt = gsub("[^[:print:]]", "", tweets.txt)
  tweets.txt = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", tweets.txt)
  tweets.txt = gsub("[^[:space:]]*…$", "", tweets.txt)
  
  tweets.txt = tweets.txt %>% removePunctuation %>% removeNumbers %>% tolower
  
  
  count = 0
  max_bot_actions = 2
  
  for (i in 1:length(tweets)) {
    if (tweet_sentiment(tweets.txt[[i]]) == "neutral") {
      next
    }
    
    else if (tweet_sentiment(tweets.txt[[i]]) == "negative") {
      if (runif(1) > 0.7) {
        response = affirmative_response(tweets[[i]])
      } else {
        response = NULL
      }
      retweet(tweets[[i]], comment = response)
      count = count + 1
    }
    
    else {
      response = negative_response(tweets[[i]])
      reply(tweets[[i]], content = response)
      count = count + 1
    }
    
    if (count == max_bot_actions) {
      break
    }
  }
}

construct_sentence = function(sentence_type) {
  sentence = NULL
  
  if (sentence_type == "feeling") {
    emotions = c("frustrated", "annoyed", "depressed", "sad")
    connectors = c("because", "about the way", "since", "seeing how")
    objects_plural = c("people", "others")
    objects_singular = c("society", "the world")
    conditions_plural = c("are", "can be", "have the potential to be", "are so", "can be so")
    conditions_singular = c("is so", "can be so", "is truly", "is", "can be")
    adverbs = c("astoundingly", "incredibly", "amazingly", "ridiculously", "hilariously")
    quality = c("stupid", "shallow", "shortsighted", "misguided", "superficial")
    
    if (runif(1) > 0.5) {
      sentence = "feeling "
    }
    if (runif(1) > 0.5) {
      sentence = paste(sentence, sample(emotions, 1), sep = "")
      sentence = paste(sentence, sample(connectors, 1),
                     sample(objects_plural, 1), sample(conditions_plural, 1),
                     sample(adverbs, 1), sample(quality, 1))
    } else {
      sentence = paste(sentence, sample(emotions, 1), sep = "")
      sentence = paste(sentence, sample(connectors, 1),
                       sample(objects_singular, 1), sample(conditions_singular, 1),
                       sample(adverbs, 1), sample(quality, 1))
    }
    
    return(sentence)
  } 
  
  if (sentence_type == "sharing") {
    times = c("today", "recently", "this week", "lately")
    subjects = c("I have been", "I've been")
    verbs = c("trying to", "working hard to", "making an effort to", "taking the time to", "using free time to")
    nouns = c("think", "exercise", "practice my skills", "meditate", "hone my mind")
    adjectives = c("more.", "in my free time.", "when I get the chance.", "more intensely.")
    conclusions = c("Would recommend.", "It makes a difference.", "You should too.", "If only others did the same.")
    
    if (runif(1) > 0.3) {
      sentence = paste(sample(times, 1), sample(subjects, 1))
    } else {
      sentence = sample(subjects, 1)
    }

    sentence = paste(sentence, sample(verbs, 1), sample(nouns, 1), sample(adjectives, 1))
    
    if (runif(1) > 0.2) {
      sentence = paste(sentence, sample(conclusions, 1))
    }
    
    return(sentence)
  }
}

twitterbot_tweet = function() {
  moods = c("feeling", "sharing")
  mood = sample(moods, 1)
  status = construct_sentence(mood)
  likelihood = 0.4
  if (runif(1) < likelihood) {
    updateStatus(text = status)
  }
}