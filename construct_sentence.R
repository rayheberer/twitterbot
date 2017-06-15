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