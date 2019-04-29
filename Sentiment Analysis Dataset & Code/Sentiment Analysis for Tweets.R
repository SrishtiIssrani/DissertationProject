# Data Preprocessing Template
install.packages('tm')
install.packages('SnowballC')
install.packages('tidytext')
install.packages('tidyverse')
install.packages('dplyr')
install.packages('ggplot2')
library('tidytext')
library('tidyverse')
library('dplyr')
library('ggplot2')
library ('tm')
library('SnowballC')
# Importing the dataset
dataset = read.csv('Baahubali2Tweets.csv', stringsAsFactors = FALSE)
#replace capital to lower text
dataset$Tweets_tweet <- tolower(dataset$Tweets_tweet) 
#remove gross
#dataset$Tweets_tweet <- gsub("gross", "", dataset$Tweets_tweet)
# remove at people
dataset$Tweets_tweet <- gsub("@\\w+", "", dataset$Tweets_tweet)
#remove punctuation
dataset$Tweets_tweet <- gsub("[[:punct:]]", "", dataset$Tweets_tweet)
#remove hyperlinks
dataset$Tweets_tweet <- gsub("http\\w+", "", dataset$Tweets_tweet)
#remove hashtag
dataset$Tweets_tweet <- gsub("[ |\t]{2,}", "", dataset$Tweets_tweet)
#remove space in the beginning
dataset$Tweets_tweet <- gsub("^ ", "", dataset$Tweets_tweet)
#remove space at the end
dataset$Tweets_tweet <- gsub(" $", "", dataset$Tweets_tweet)
#remove emojis and special characters
dataset$Tweets_tweet <- gsub('<.*>', '', enc2native(dataset$Tweets_tweet))

Corpus = VCorpus(VectorSource(dataset$Tweets_tweet))
Corpus = tm_map(Corpus, removeNumbers)
Corpus = tm_map(Corpus, removeWords, stopwords())
Corpus = tm_map(Corpus, stemDocument)
Corpus = tm_map(Corpus, stripWhitespace)

dtm = DocumentTermMatrix(Corpus)

dtm = removeSparseTerms(dtm, 0.999)


bing<- get_sentiments("bing")

SA <- tidy(Corpus)
SA %>%
  unnest_tokens(word, text) %>% 
  inner_join(bing, by = "word") %>% 
  #filter(sentiment == "positive") %>%
  #group_by(word, sentiment) %>%
  group_by(sentiment)%>%
  count(sentiment)%>%
  spread(sentiment, n)%>%
 mutate(sentiment= positive / negative) %>%
  View()
  View(dataset)