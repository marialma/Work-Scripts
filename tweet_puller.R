rm(list=ls())
setwd("~/Dropbox/Maria Ma/Twitter")
library(rtweet)
library(dplyr)
library(tidytext)

date <- Sys.Date()

abx <- search_tweets("antibiotics", n=18000, include_rts = TRUE, retryonratelimit = TRUE)
abxcsv <- apply(abx,2,as.character)
write.csv(abxcsv, file = paste(date, "twitter.csv", sep="_"))

abx_nort <- search_tweets("antibiotics", n=18000, include_rts = FALSE, retryonratelimit = TRUE)


abxr <- search_tweets("#antibioticresistance", n=10000, include_rts = FALSE)

## #Antibiotics ----
unlist(abx_nort$hashtags) -> nort_htlist # unlisting since many tweets have multiple hashtags. 
hashtags <- nort_htlist %>% table() %>% as.data.frame()  # gather all hashtags
colnames(hashtags)[1] <- "HT"
hashtags <- as.data.frame(sapply(hashtags, tolower)) # all lowercase. next two lines combine all the vals
hashtags$Freq <- as.numeric(as.character(hashtags$Freq)) 
ht2 <- hashtags %>% group_by(HT) %>% summarise_all(funs(sum))
TopHT_antibiotics <- ht2 %>% arrange(Freq) %>% top_n(20)

## #AntibioticResistance ----
unlist(abxr$hashtags) -> nort_htlist
hashtags <- nort_htlist %>% table() %>% as.data.frame() 
colnames(hashtags)[1] <- "HT"
hashtags <- as.data.frame(sapply(hashtags, tolower))
hashtags$Freq <- as.numeric(as.character(hashtags$Freq))
ht2 <- hashtags %>% group_by(HT) %>% summarise_all(funs(sum))
TopHT_AMR <- ht2 %>% arrange(Freq) %>% top_n(20)



