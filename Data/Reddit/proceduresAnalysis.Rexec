######Preparing Data

#install.packages("rvest")
#install.packages("dplyr")
#install.packages("tm")'
#install.packages("wordcloud")

library(rvest)
library(dplyr)

setwd("D://Users/tss3dn/PycharmProjects/Capstone")

aFile = readLines("outputTest.csv")

aFile

library(tm)
myCorpus = Corpus(VectorSource(aFile))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus = tm_map(myCorpus, removeWords, c("also", "will", "then", "make", "dont", "use", "ive", "now", "people", "get", "just", "really", 
                                           "can", "much", "lot", "didnt", "thats", "that", "might", "since", "back", "using", "getting", "may",
                                           "sure", "way", "youre", "things", "like", "actually", "lol", "isnt"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 4))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
names(v)


freq <- rowSums(m)


library(wordcloud)
set.seed(4363)
par(mar=c(1,1,1,1))

wordcloud(names(v), v, min.freq = 20)


df <- as.data.frame(names(v))
#View(df)



myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 2))

rowTotals <- apply(myDTM , 1, sum) #Find the sum of words in each Document
dtm.new   <- myDTM[rowTotals> 0, ]         #remove all docs without words

myDTM <- dtm.new

######### Topic Modeling

#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("ggplot2")

#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidytext")
library(tidytext)
#install.packages("stringr")
library(stringr)
library(tm)
#install.packages("topicmodels")
library(topicmodels)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

n_topics <- 6

myDTM

myDTM.lda <- LDA(myDTM, k = n_topics, control = list(seed = 1234))

myDTM.lda

myDTM.topics <- tidy(myDTM.lda, matrix = "beta") 

myDTM.top_terms <- myDTM.topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, term, -beta)

myDTM.top_terms2 <- myDTM.top_terms %>%
  group_by(topic) %>%
  summarise(terms = toString(sort(unique(term))))
myDTM.top_terms2

myDTM.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##Adding Stop Words

#myCorpus <- tm_map(myCorpus, removeWords, 'acne')
myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 3))

rowTotals <- apply(myDTM , 1, sum) #Find the sum of words in each Document
dtm.new   <- myDTM[rowTotals> 0, ]           #remove all docs without words

myDTM <- dtm.new

myDTM.lda <- LDA(myDTM, k = n_topics, control = list(seed = 123))

myDTM.top_terms <- myDTM.topics %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, term, -beta)

myDTM.top_terms

write.csv(myDTM.top_terms, "procTopics.csv", row.names=TRUE, na="")

myDTM.top_terms2 <- myDTM.top_terms %>%
  group_by(topic) %>%
  summarise(terms = toString(sort(unique(term))))
myDTM.top_terms2

myDTM.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text = element_text(size = 12, face = "bold"))



