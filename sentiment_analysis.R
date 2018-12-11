library(data.table)
library(digest)
library(SnowballC)
library(tm)
library(syuzhet)

## Explore tweets

tweets <- searchTwitter('hitrecord', n=100)
head(tweets)

## strip retweets
tweets <- (strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE))

## convert to dataframe
tweetsdf <- twListToDF(tweets)
sentence <- as.vector(tweetsdf$text)
head(sentence)


# clean up sentences with R's regex-driven global substitute, gsub():
sentence <- gsub('[[:punct:]]', '', sentence)
sentence <- gsub('[[:cntrl:]]', '', sentence)
sentence <- gsub('\\d+', '', sentence)
# and convert to lower case:
sentence <- tolower(sentence)

head(sentence)

#get emotions
words <- as.vector(sentence)
emotions <- get_nrc_sentiment(words)
emotions2 <- cbind(words, emotions) 


#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param pos.words vector of words of postive sentiment
#' @param neg.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores
#' @author Jefrey Breen <jbreen@cambridge.aero>
score.sentiment <- function(sentence, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores <- laply(sentence, function(sentence, pos.words, neg.words) {
    
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- is.na(pos.matches)
    neg.matches <- is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

score.sentiment(sentence)