library(twitteR)
library(data.table)
library(digest)
library(SnowballC)
library(tm)
library(syuzhet)
library(maps)
library(mapproj)

setwd("git/VeepsLearnsTwitteR")
## Twitter Authentication

if (!require("twitteR")) {
  install.packages("twitteR", repos="http://cran.rstudio.com/") 
  library("twitteR")
}
consumer_key <- "Fg1Hj8iXy6DjfLU8rO2h1tW6y"
consumer_secret <- "W4bwfep1ELehFcH1LDODahnidRcJVbz8LZSDZidaOlyg9NW42q"
access_token <- "281326158-EE628YRKt5yOr0KpPEEBgDlGCg7eT6Q2eonsAM75"
access_secret <- "1JJJ9cRYQwSS8Zk1cahgE94j5DWn8h00j0ibLKtzubmM8"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

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

########################## MAP 

## Download user data

create_self <- getUser("create_self")
create_self_follower_IDs<-create_self$getFollowers(retryOnRateLimit=180)
length(create_self_follower_IDs)
head(create_self_follower_IDs)
## Turn list into data table

create_self_followers_df = rbindlist(lapply(create_self_follower_IDs,as.data.frame))
create_self_followers_df<-subset(create_self_followers_df, location!="") #remove blanks
create_self_followers_df$location<-gsub("%", " ",create_self_followers_df$location) #remove % signs
#check
head(create_self_followers_df$location, 10)

#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")


#specify what I'm extracting from Googlemap API

geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyCqszyMHF8F00_mvzZbkqAHgmmGUepXuaE")
}

#geocode user locations
geocode_results<-sapply(create_self_followers_df$location, geocode_apply, simplify = F)
length(geocode_results)
class(geocode_results)

##cleaning geocode
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results_a<-geocode_results[condition_a] #remove everything without status OK
length(geocode_results_a)

condition_b <- lapply(geocode_results, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1") #remove everything with status ANY
geocode_results<-geocode_results[condition_b2]
length(geocode_results)


## clean misformatting
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")
results_b<-lapply(geocode_results, as.data.frame)


results_c <-lapply(results_b,function(x) subset(x, select=c("results.formatted_address",
                                                           "results.geometry.location")))

results_d<-lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))

results_e<-rbindlist(results_d)

#add in original location column
results_f<-results_e[,Original_Location:=names(results_d)]

#write results into CSV for future
write.csv(results_f, file="VeepsTwitterFollowers.csv")


#filter for US locations only
american_results<-subset(results_f,
                         grepl(", USA", results_f$Location)==TRUE)


#generate blank map
albers_proj<-map("state", proj="albers", param=c(39, 45), 
                 col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=FALSE, resolution=1)

#add points to map
points(mapproject(american_results$lng, american_results$lat), col=NA, bg="#D3D3D3", pch=21, cex=1.0)