library(maps)
library(mapproj)


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