library(tidytext)
library(knitr)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)

##Sentiment Analysis: Rihanna

#parse Rihanna tweets
rihanna_tweets <- parseTweets("rihanna_tweets.json")

#filter out non-english words
rihanna_tweets <- filter(rihanna_tweets, lang == "en")

#save into a dataframe
text_df <- data_frame(line = 1:length(rihanna_tweets$text), text = rihanna_tweets$text)

# unnest the individual words in tweets
rihanna_words <- text_df %>% 
  unnest_tokens(word, text)

# load stop words data set
data(stop_words)

# remove stop words from rihanna tweets data
rihanna_words <- rihanna_words %>%
  anti_join(stop_words)

# filter out additional common words that we don't want
rihanna_words <- filter(rihanna_words, word != "t.co")
rihanna_words <- filter(rihanna_words, word != "https")
rihanna_words <- filter(rihanna_words, word != "feat")
rihanna_words <- filter(rihanna_words, word != "rt")
rihanna_words <- filter(rihanna_words, word != "post")
rihanna_words <- filter(rihanna_words, word != "malone")
rihanna_words <- filter(rihanna_words, word != "o0qejapfl2")
rihanna_words <- filter(rihanna_words, word != "swift")
rihanna_words <- filter(rihanna_words, word != "rihanna")
rihanna_words <- filter(rihanna_words, word != "finah")
rihanna_words <- filter(rihanna_words, word != "4qx5argemb")

rihannacount <- rihanna_words %>%
  count(word, sort = TRUE)


#load bing sentiment data
bing <- get_sentiments("bing")

#sentiment analysis of rihanna tweets with bing
bing_word_counts <- rihanna_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

#create word cloud with sentiment info
png("rihannacloud.png", width=6, height=4, units="in", res=275)
rihanna_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
dev.off()

#plot positive vs. negative words
rihannaplot1 <- ggplot(bing_word_counts, aes(x = sentiment)) + 
  geom_bar(fill = "#ffa500") + labs(x = "sentiment", y = "number of words",
           title = "Rihanna Word Sentiments")

#load afinn sentiment data
afinn <- get_sentiments("afinn")

afinn_word_counts <- rihanna_words %>%
  inner_join(afinn) %>%
  count(word, score, sort = TRUE)

#sentiment analysis with afinn 
afinn_tweets <- rihanna_words %>% 
  inner_join(afinn) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

# graph of tweet sentiments
rihannaplot2 <- ggplot(afinn_tweets, aes(index, sentiment, fill = sentiment),
                       title = "Rihanna Word Sentiments") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

rihannaplot3 <- ggplot(afinn_word_counts, aes(x = score)) + 
  geom_bar(fill = "#ffa500") + labs(x = "score", y = "number of words",
           title = "Rihanna Word Sentiments")

##Sentiment Analysis: Taylor Swift

#do the same for taylor swift. comments from rihanna sentiment analysis apply.
taylorswift_tweets <- parseTweets("taylorswift_tweets.json")

taylorswift_tweets <- filter(taylorswift_tweets, lang == "en")

text_df <- data_frame(line = 1:length(taylorswift_tweets$text), text = taylorswift_tweets$text)

taylorswift_words <- text_df %>% 
  unnest_tokens(word, text)

taylorswift_words <- taylorswift_words %>%
  anti_join(stop_words)

taylorswift_words <- filter(taylorswift_words, word != "t.co")
taylorswift_words <- filter(taylorswift_words, word != "https")
taylorswift_words <- filter(taylorswift_words, word != "feat")
taylorswift_words <- filter(taylorswift_words, word != "rt")
taylorswift_words <- filter(taylorswift_words, word != "swift")
taylorswift_words <- filter(taylorswift_words, word != "taylorswift")
taylorswift_words <- filter(taylorswift_words, word != "taylor")

tswiftcount <- taylorswift_words %>%
  count(word, sort = TRUE)

bing_word_counts <- taylorswift_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

png("tswiftcloud.png", width=6, height=4, units="in", res=300)
taylorswift_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), scale=c(4,.50), title.size = 2,
                   max.words = 100)
dev.off()


tswiftplot1 <- ggplot(bing_word_counts, aes(x = sentiment)) + 
  geom_bar(fill = "#4bc4ff") + labs(x = "sentiment", y = "number of words",
           title = "Taylor Swift Word Sentiments")

afinn_tweets <- taylorswift_words %>% 
  inner_join(afinn) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

tswiftplot2 <- ggplot(afinn_tweets, aes(index, sentiment, fill = sentiment),
                      title = "Taylor Swift Word Sentiments") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

afinn_word_counts <- taylorswift_words %>%
  inner_join(afinn) %>%
  count(word, score, sort = TRUE)

tswiftplot3 <- ggplot(afinn_word_counts, aes(x = score)) + 
  geom_bar(fill = "#4bc4ff") + labs(x = "score", y = "number of words",
           title = "Taylor Swift Word Sentiments")

#do the same for taylor swift. comments from rihanna sentiment analysis apply.
justinbieber_tweets <- parseTweets("justinbieber_tweets.json")

justinbieber_tweets <- filter(justinbieber_tweets, lang == "en")

text_df <- data_frame(line = 1:length(justinbieber_tweets$text), text = justinbieber_tweets$text)

justinbieber_words <- text_df %>% 
  unnest_tokens(word, text)

justinbieber_words <- justinbieber_words %>%
  anti_join(stop_words)

justinbieber_words <- filter(justinbieber_words, word != "t.co")
justinbieber_words <- filter(justinbieber_words, word != "https")
justinbieber_words <- filter(justinbieber_words, word != "feat")
justinbieber_words <- filter(justinbieber_words, word != "rt")
justinbieber_words <- filter(justinbieber_words, word != "justin")
justinbieber_words <- filter(justinbieber_words, word != "bieber")
justinbieber_words <- filter(justinbieber_words, word != "justinbieber")

jbiebercount <- justinbieber_words %>%
  count(word, sort = TRUE)

bing_word_counts <- justinbieber_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

png("jbiebercloud.png", width=6, height=4, units="in", res=300)
justinbieber_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
dev.off()


jbieberplot1 <- ggplot(bing_word_counts, aes(x = sentiment)) + 
  geom_bar(fill = "#00ff22") + labs(x = "sentiment", y = "number of words",
           title = "Justin Bieber Word Sentiments")

afinn_tweets <- justinbieber_words %>% 
  inner_join(afinn) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

jbieberplot2 <- ggplot(afinn_tweets, aes(index, sentiment, fill = sentiment),
                       title = "Justin Bieber Tweet Sentiments") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

afinn_word_counts <- justinbieber_words %>%
  inner_join(afinn) %>%
  count(word, score, sort = TRUE)

jbieberplot3 <- ggplot(afinn_word_counts, aes(x = score)) + 
  geom_bar(fill = "#00ff22") + labs(x = "score", y = "number of words",
           title = "Justin Bieber Word Sentiments")


#do the same for taylor swift. comments from rihanna sentiment analysis apply.
kendricklamar_tweets <- parseTweets("kendricklamar_tweets.json")

kendricklamar_tweets <- filter(kendricklamar_tweets, lang == "en")

text_df <- data_frame(line = 1:length(kendricklamar_tweets$text), 
                      text = kendricklamar_tweets$text)

kendricklamar_words <- text_df %>% 
  unnest_tokens(word, text)

kendricklamar_words <- kendricklamar_words %>%
  anti_join(stop_words)

kendricklamar_words <- filter(kendricklamar_words, word != "t.co")
kendricklamar_words <- filter(kendricklamar_words, word != "https")
kendricklamar_words <- filter(kendricklamar_words, word != "feat")
kendricklamar_words <- filter(kendricklamar_words, word != "rt")
kendricklamar_words <- filter(kendricklamar_words, word != "damn")
kendricklamar_words <- filter(kendricklamar_words, word != "kendrick")
kendricklamar_words <- filter(kendricklamar_words, word != "lamar")
kendricklamar_words <- filter(kendricklamar_words, word != "kendricklamar")

kendrickcount <- kendricklamar_words %>%
  count(word, sort = TRUE)

bing_word_counts <- kendricklamar_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

png("klamarcloud.png", width=6, height=4, units="in", res=300)
kendricklamar_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
dev.off()

klamarplot1 <- ggplot(bing_word_counts, aes(x = sentiment)) + 
  geom_bar(fill = "#ff0000") + labs(x = "sentiment", y = "number of words",
           title = "Kendrick Lamar Word Sentiments")

afinn_tweets <- kendricklamar_words %>% 
  inner_join(afinn) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

klamarplot2 <- ggplot(afinn_tweets, aes(index, sentiment, fill = sentiment),
                      title = "Kendrick Lamar Tweet Sentiments") +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

afinn_word_counts <- kendricklamar_words %>%
  inner_join(afinn) %>%
  count(word, score, sort = TRUE)

klamarplot3 <- ggplot(afinn_word_counts, aes(x = score)) +
  geom_bar(fill = "#ff0000") + labs(x = "score", y = "number of words",
           title = "Kendrick Lamar Word Sentiments")


## Now I'll get the data ready for Shiny

# add artist name column to each df
rihanna_words$artist <- "rihanna"

taylorswift_words$artist <- "taylor swift"

justinbieber_words$artist <- "justin bieber"

kendricklamar_words$artist <- "kendrick lamar"

#bind dfs together

total <- rbind(rihanna_words, taylorswift_words, justinbieber_words, kendricklamar_words)
