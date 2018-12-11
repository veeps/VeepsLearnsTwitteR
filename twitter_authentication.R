library(twitteR)
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