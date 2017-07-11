retweet = function(twt, comment=NULL) {
  url = paste0('https://twitter.com/', twt$screenName, '/status/', twt$id)
  updateStatus(paste(comment, url), bypassCharLimit = TRUE)
}

reply = function(twt, content) {
  content = paste0("@", twt$screenName, " ", content)
  updateStatus(content, inReplyTo = twt$id)
}
