# @RobertRealGuy

RobertRealGuy is a twitterbot that attempts to stir up controversy by responding in an obnoxious way to people's tweets.

## Method

The twitterbot can be set to focus on tweets that contain a keyword, typically a twitter handle. For example, "@realDonaldTrump." Then, it determines the sentiment of tweets using the [tm package](https://cran.r-project.org/web/packages/tm/tm.pdf).

Depending on the sentiment, RobertRealGuy will respond either positively or negatively. Currently, it draws from a library of responses, but a method for dynamically generating tweets is in development.

## Deployment

RobertRealGuy tweets randomly according to a cron job on a remote AWS instance. The details of the crontab are in crontab.txt.