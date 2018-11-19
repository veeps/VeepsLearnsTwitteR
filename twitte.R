library(twitteR)
library(data.table)
library(digest)
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
head(tweetsdf)

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
  geocode(x, source = "google", output = "all", api_key="AIzaSyCPd31U6X6JG-h5I2ZoDeXy0eAb_XgJIDk")
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
