library(data.table)
library(digest)
library(SnowballC)
library(tm)
library(syuzhet)

## Explore tweets

tweets <- searchTwitter('hitrecord', n=200)
head(tweetsdf)
length(tweets)

## strip retweets
tweets <- (strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE))

## convert to dataframe
tweetsdf <- twListToDF(tweets)
sentence <- as.vector(tweetsdf$text)
head(sentence)


# clean up sentences with R's regex-driven global substitute, gsub():
sentence <- gsub('[[:punct:]]', '', sentence) #remove all punctuation
sentence <- gsub('[[:cntrl:]]', '', sentence) #remove control characters
sentence <- gsub('\\d+', '', sentence) #remove digits
# and convert to lower case:
sentence <- tolower(sentence)

head(sentence)

#get emotions
words <- as.vector(sentence)
emotions <- get_nrc_sentiment(words)
emotions2 <- cbind(words, emotions) 

head(emotions2)


# score sentiment
score <- sum(emotions2$positive) - sum(emotions2$negative)

