library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(genderizeR)
library(gender)
library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(reshape2)
library(lubridate)

##### LOAD TEXTS AND PRE CLEANING #####

setwd("~/Gitdir/Pippy-data/Data/Facebook") 
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], na.strings = ""))


# Fix all the dates, first AcneAnswers dates are slightly different from the rest!
AcneAnswers_facebook_comments.csv$comment_published <- mdy_hm(AcneAnswers_facebook_comments.csv$comment_published)
AcneAnswers_facebook_statuses.csv$comment_published <- mdy_hm(AcneAnswers_facebook_statuses.csv$comment_published)
facebook_comments_135818293109976.csv$comment_published <- ymd_hms(facebook_comments_135818293109976.csv$comment_published)
facebook_comments_1582733551821102.csv$comment_published <- ymd_hms(facebook_comments_1582733551821102.csv$comment_published)
facebook_statuses_135818293109976.csv$comment_published <- ymd_hms(facebook_statuses_135818293109976.csv$comment_published)
facebook_statuses_1582733551821102.csv$comment_published <- ymd_hms(facebook_statuses_1582733551821102.csv$comment_published)

# Combine all the separate csv files
fb <- rbind(AcneAnswers_facebook_comments.csv[c('comment_author','comment_message','comment_published',
                                                'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                                'num_sads', 'num_angrys','num_special')], 
            AcneAnswers_facebook_statuses.csv[c('comment_author','comment_message','comment_published',
                                    'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                    'num_sads', 'num_angrys','num_special')] , 
            facebook_comments_135818293109976.csv[c('comment_author','comment_message','comment_published',
                                                    'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                                    'num_sads', 'num_angrys','num_special')],
            facebook_comments_1582733551821102.csv[c('comment_author','comment_message','comment_published',
                                                     'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                                     'num_sads', 'num_angrys','num_special')],
            facebook_statuses_135818293109976.csv[c('comment_author','comment_message','comment_published',
                                                    'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                                    'num_sads', 'num_angrys','num_special')],
            facebook_statuses_1582733551821102.csv[c('comment_author','comment_message','comment_published',
                                                     'num_reactions','num_likes','num_loves','num_wows','num_hahas',
                                                     'num_sads', 'num_angrys','num_special')])      
## Find gender ##

fb$comment_author <- as.character(fb$comment_author)
fb$name <- sapply(strsplit(fb$comment_author,' '), function(x) x[1])
name <- gender(as.character(fb$name), years = c(1932,2012))[1:4]

# Get an inner join 
fb <- inner_join(fb, name, by = "name")

# Remove duplicated data
fb <- fb[!duplicated(fb),] # Takes out roughly ~1000 rows

# Take out first names
fb <- fb[, -c(12:14)]

# Change df to tibble
fb <- as.tibble(fb)

# Remove all NAs
fb <- fb[rowSums(is.na(fb)) == 0,]

# Change text to character 
fb$comment_message <- as.character(fb$comment_message)

# Add source
fb$source <- "Facebook"

##### BEGIN CLEANING TEXT #####

# Get cleaned raw text 
some_txtm <- as.character(fb$comment_message)
some_txtm = gsub("[[:punct:]]", "", some_txtm)
some_txtm = gsub("[[:digit:]]", "", some_txtm)
some_txtm = gsub("http\\w+", "", some_txtm)
some_txtm = gsub("[ \t]{2,}", "", some_txtm)
some_txtm = gsub("^\\s+|\\s+$", "", some_txtm)
some_txtm = gsub("[^\x01-\x7F]+","",some_txtm)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

some_txtm = sapply(some_txtm, try.error)
some_txtm = some_txtm[!is.na(some_txtm)]
names(some_txtm) = NULL
fb$comment_message <- some_txtm

# Get corpus
doxm <- fb$comment_message
doxm <- Corpus(VectorSource(doxm)) 
doxm <- tm_map(doxm,content_transformer(tolower)) 
doxm <- tm_map(doxm,stripWhitespace) 
doxm <- tm_map(doxm,removeWords,stopwords('english')) 
doxm <- tm_map(doxm,removePunctuation)
doxm <- tm_map(doxm,stemDocument)
doxm <- tm_map(doxm, removeNumbers)

# Get term document matrix and document term matrix 
dtmm <- DocumentTermMatrix(doxm)
tdmm <- TermDocumentMatrix(doxm)

# Remove all NAs
fb <- fb[rowSums(is.na(fb)) == 0,]

