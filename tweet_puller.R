library(twitteR)

consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
#sigh i am garbage

today <- Sys.Date()
date <- today - 7

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets = twitteR::searchTwitter('#antibiotics', n = 1e3, since = paste(date), retryOnRateLimit = 1e3)
df = twitteR::twListToDF(tweets)
write.csv(df, file = paste(date, "twitter.csv", sep="_"),row.names=FALSE)

