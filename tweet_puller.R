library(twitteR)

consumer_key <- "uYqKgzdRZSaTiu9D26h8Li4EV"
consumer_secret <- "F3zjuVxlhEOlaicfT6oxqsXzOM6ZKFoWjEzBaAmL5PY5OGOLxN"
access_token <- "4105002613-QDipwyEJtgSQ1RcqsPQGUpHuThyo9o9Xoj5nVzT"
access_secret <- "F0TFyoxqCxmdku6qGqcTlGWmX9Zmobw9bJqaAxkgL5XmY"

today <- Sys.Date()
date <- today - 7

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets = twitteR::searchTwitter('#antibiotics', n = 1e3, since = date, retryOnRateLimit = 1e3)
df = twitteR::twListToDF(tw)
write.csv(df, file = paste(date, "twitter.csv", sep="_"),row.names=FALSE)
