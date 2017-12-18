library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(streamR)

# parse rihanna_us_tweets.json file
rihanna_us_tweets <- parseTweets("rihanna_us_tweets.json") 

# select some columns
rihanna_us_tweets = rihanna_us_tweets %>% select(text, location, followers_count, geo_enabled, 
                                                 user_created_at, time_zone, place_lat, place_lon)

# remove na values for lon/lat coordinates
rihanna_us_tweets.df <- filter(rihanna_us_tweets, place_lat != "NaN" | place_lon != "NaN")

# remove values outside the US
rihanna_us_tweets.df <- filter(rihanna_us_tweets.df, place_lat >= 19.50)
rihanna_us_tweets.df <- filter(rihanna_us_tweets.df, place_lat <= 64.85)
rihanna_us_tweets.df <- filter(rihanna_us_tweets.df, place_lon >= -161.76)
rihanna_us_tweets.df <- filter(rihanna_us_tweets.df, place_lon <= -68.01)

# export csv
write.csv(rihanna_us_tweets.df, "rihanna_us_tweets.csv", row.names = FALSE)

# read csv
rihanna_us_tweets.df <- read_csv('rihanna_us_tweets.csv')
rihanna_us_tweets.df <- data.frame(rihanna_us_tweets.df)

# plot map with leaflet
map <- leaflet(rihanna_us_tweets.df) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; 
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
map %>% addCircles(~place_lon, ~place_lat, popup=rihanna_us_tweets.df$place_lon, weight = 3, radius=30, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 

# now i'll do the same with lady gaga tweets. comments from rihanna tweet mapping apply
taylorswift_us_tweets <- parseTweets("taylorswift_us_tweets.json") 

taylorswift_us_tweets = taylorswift_us_tweets %>% select(text, location, followers_count, geo_enabled, 
                                                   user_created_at, time_zone, place_lat, place_lon)

taylorswift_us_tweets.df <- filter(taylorswift_us_tweets, place_lat != "NaN" | place_lon != "NaN")

taylorswift_us_tweets.df <- filter(taylorswift_us_tweets.df, place_lat >= 19.50)
taylorswift_us_tweets.df <- filter(taylorswift_us_tweets.df, place_lat <= 64.85)
taylorswift_us_tweets.df <- filter(taylorswift_us_tweets.df, place_lon >= -161.76)
taylorswift_us_tweets.df <- filter(taylorswift_us_tweets.df, place_lon <= -68.01)

write.csv(taylorswift_us_tweets.df, "taylorswift_us_tweets.csv", row.names = FALSE)

taylorswift_us_tweets.df <- read_csv('taylorswift_us_tweets.csv')
taylorswift_us_tweets.df <- data.frame(taylorswift_us_tweets.df)

map <- leaflet(taylorswift_us_tweets.df) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; 
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
map %>% addCircles(~place_lon, ~place_lat, popup=taylorswift_us_tweets.df$place_lon, weight = 3, radius=30, 
                 color="#4bc4ff", stroke = TRUE, fillOpacity = 0.8) 

# now i'll do the same with justin bieber tweets. comments from rihanna tweet mapping apply
justinbieber_us_tweets <- parseTweets("justinbieber_us_tweets.json") 

justinbieber_us_tweets = justinbieber_us_tweets %>% select(text, location, followers_count, geo_enabled, 
                                                         user_created_at, time_zone, place_lat, place_lon)

justinbieber_us_tweets.df <- filter(justinbieber_us_tweets, place_lat != "NaN" | place_lon != "NaN")

justinbieber_us_tweets.df <- filter(justinbieber_us_tweets.df, place_lat >= 19.50)
justinbieber_us_tweets.df <- filter(justinbieber_us_tweets.df, place_lat <= 64.85)
justinbieber_us_tweets.df <- filter(justinbieber_us_tweets.df, place_lon >= -161.76)
justinbieber_us_tweets.df <- filter(justinbieber_us_tweets.df, place_lon <= -68.01)

write.csv(justinbieber_us_tweets.df, "justinbieber_us_tweets.csv", row.names = FALSE)

justinbieber_us_tweets.df <- read_csv('justinbieber_us_tweets.csv')
justinbieber_us_tweets.df <- data.frame(justinbieber_us_tweets.df)

map<- leaflet(justinbieber_us_tweets.df) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; 
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
map%>% addCircles(~place_lon, ~place_lat, popup=justinbieber_us_tweets.df$place_lon, weight = 3, radius=30, 
                 color="#00ff22", stroke = TRUE, fillOpacity = 0.8) 

# now i'll do the same with kendrick lamar tweets. comments from rihanna tweet mapping apply
kendricklamar_us_tweets <- parseTweets("kendricklamar_us_tweets.json") 

kendricklamar_us_tweets = kendricklamar_us_tweets %>% select(text, location, followers_count, geo_enabled, 
                                                             user_created_at, time_zone, place_lat, 
                                                             place_lon)

kendricklamar_us_tweets.df <- filter(kendricklamar_us_tweets, place_lat != "NaN" | place_lon != "NaN")

kendricklamar_us_tweets.df <- filter(kendricklamar_us_tweets.df, place_lat >= 19.50)
kendricklamar_us_tweets.df <- filter(kendricklamar_us_tweets.df, place_lat <= 64.85)
kendricklamar_us_tweets.df <- filter(kendricklamar_us_tweets.df, place_lon >= -161.76)
kendricklamar_us_tweets.df <- filter(kendricklamar_us_tweets.df, place_lon <= -68.01)

write.csv(kendricklamar_us_tweets.df, "kendricklamar_us_tweets.csv", row.names = FALSE)

kendricklamar_us_tweets.df <- read_csv('kendricklamar_us_tweets.csv')
kendricklamar_us_tweets.df <- data.frame(kendricklamar_us_tweets.df)

map <- leaflet(kendricklamar_us_tweets.df) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, 
           <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; 
           Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
map %>% addCircles(~place_lon, ~place_lat, popup=kendricklamar_us_tweets.df$place_lon, 
                   weight = 3, radius=30, color="#ff0000", stroke = TRUE, 
                   fillOpacity = 0.8) 
