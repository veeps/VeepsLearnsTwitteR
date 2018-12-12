library(data.table)
library(tm)
library(syuzhet)
library(ggplot2)

setwd("git/VeepsLearnsTwitteR")

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
score_log <- log(sum(emotions2$positive) + 0.5) - log(sum(emotions2$negative) - 0.5)



#################### Sentiment analysis using tidytext ####################
library(tidytext)
library(dplyr)

tweets <- searchTwitter('hitrecord', n=2000)


## strip retweets
tweets <- (strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE))


## convert to dataframe
tweets_df <- twListToDF(tweets)

## write to csv for future
write.csv(tweets_df, file = "hitrecord_tweets.csv")

##strip tweets from @hitrecord and @hitrecordjoe
tweets_df <- tweets_df[tweets_df$screenName != "hitRECord", ]
tweets_df <- tweets_df[tweets_df$screenName != "hitRECordJoe", ]


#tokenize text
text_df <- data_frame(line = 209, text = tweets_df$text)
tidy_text <- text_df %>%
  unnest_tokens(word, text)

#remove stop words 
data(stop_words)

clean_text <- tidy_text %>%
  anti_join(stop_words)


#remove link stuff
links <- c("https", "http", "t.co", "bit.ly", "hitrecord", "hitrecordjoe")
links_tb <- tibble( word = links )
clean_text <- clean_text %>%
  anti_join(links_tb)

#get word count
clean_text %>%
  count(word, sort = TRUE) 


#plot word count
clean_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()