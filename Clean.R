library(tidyverse)
library(base64enc)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

#### Blogs

setwd('D:/Users/tss3dn/Desktop/PIPPY-data/Data/Blogs')

theBeautyBrains <- read.csv('items_thebeautybrains.com_1.csv')
hudaBeauty <- read.csv('items_hudabeauty.com_1.csv')
hotBeatyHealth <- read.csv('items_www.hotbeautyhealth.com_1.csv')
musings <- read.csv('items_www.musingsofamuse.com_1.csv')
womenStuff <- read.csv('items_www.mywomenstuff.com_1.csv')
vivaWoman <- read.csv('items_www.vivawoman.net_1.csv')
trendHunter <- read.csv('items_www.trendhunter.com_1.csv')
trendHunter2 <- read.csv('items_www.trendhunter.com_2.csv')
trendHunter <- rbind(trendHunter, trendHunter2)
rm(trendHunter2)
temptalia <- read.csv('items_www.temptalia.com_1.csv')

blogs <- rbind(theBeautyBrains[c('Date','Text')], hudaBeauty[c('Date','Text')], hotBeatyHealth[c('Date','Text')], musings[c('Date','Text')],womenStuff[c('Date','Text')],vivaWoman[c('Date','Text')],trendHunter[c('Date','Text')],temptalia[c('Date','Text')])

pMiss <- function(x){sum(is.na(x))/length(x)*100}

blogs$Date <- as.character(blogs$Date)
blogs$Text <- as.character(blogs$Text)
blogs$Date[blogs$Date==""] <- NA
blogs$Date[blogs$Date=='None']<-NA
blogs$Text[blogs$Text==""] <- NA
keepers <- which(apply(blogs,1,pMiss)==0)
gBlogs <- blogs[keepers,]

gBlogs$Date <- as.Date(gBlogs$Date)
keepers <- which(apply(gBlogs,1,pMiss)==0)
gBlogs <- gBlogs[keepers,]

gBlogs$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", gBlogs$Text)
# remove at people
gBlogs$Text = gsub("@\\w+", "", gBlogs$Text)
# remove punctuation
gBlogs$Text = gsub("[[:punct:]]", "", gBlogs$Text)
# remove numbers
gBlogs$Text = gsub("[[:digit:]]", "", gBlogs$Text)
# remove html links
gBlogs$Text = gsub("http\\w+", "", gBlogs$Text)
# remove unnecessary spaces
gBlogs$Text = gsub("[ \t]{2,}", "", gBlogs$Text)
gBlogs$Text = gsub("^\\s+|\\s+$", "", gBlogs$Text)
gBlogs$Text = gsub('???','',gBlogs$Text)
gBlogs$Text = gsub('etc','',gBlogs$Text)
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
# lower case using try.error with sapply 
gBlogs$Text = sapply(gBlogs$Text, try.error)

# remove NAs in gBlogs$Text
gBlogs$Text = gBlogs$Text[!is.na(gBlogs$Text)]
names(gBlogs$Text) = NULL

col=brewer.pal(6,"Dark2")
wordcloud(gBlogs$Text, min.freq=50, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=10, random.order=F,colors=col)

blogWords <- gBlogs %>% tidytext::unnest_tokens(word,Text) %>% anti_join(tidytext::stop_words)

ingr <- read.csv('Ingredients.csv')
colnames(ingr) = c('Ingredient')
ingr$Ingredient <- as.character(ingr$Ingredient)
ingr <- ingr %>% tidytext::unnest_tokens(word,Ingredient) %>% anti_join(tidytext::stop_words)
ingr$word <- sapply(ingr$word, try.error)
ingrWordsB <- inner_join(blogWords,ingr,by='word')
wordcloud(ingrWordsB$word, min.freq=25,
          random.color=T, max.word=200, random.order=F,colors=col)


ingrWordsB <- data.frame(ingrWordsB, source = 'blog')
ingrWordsB$source <- as.character(ingrWordsB$source)
#write.csv(ingrWordsB,'ingrWordsB.csv')


###### Reddit


setwd('D:/Users/tss3dn/Desktop/PIPPY-data/Data/Reddit')

reddit <- read.delim('redditWithTimestamp.csv')
colnames(reddit) = 'post'
reddit <- as.character(reddit$post)

reddit <- paste(reddit, collapse = '\n')
reddit <- gsub(', ',' ', reddit)
reddit <- unlist(strsplit(reddit, '[,]'))

dates <- substr(reddit[2:length(reddit)],0,24)
posts1 <- substr(reddit[1:1],8,nchar(reddit[1]))
posts <- substr(reddit[3:length(reddit)-1],25,1000000)
posts <- append(posts1,posts)
posts <- gsub('\n',' ',posts)
rClean <- data.frame(posts,dates)
rClean$posts <- as.character(rClean$posts)
rClean$dates <- as.character(rClean$dates)

rClean$posts[rClean$posts=="[removed] "] <- NA
rClean$posts[rClean$posts==" [removed]"] <- NA
rClean$posts[rClean$posts=="[removed]"] <- NA
rClean$posts[rClean$posts==""] <- NA
rClean$posts[rClean$posts=="[deleted] "] <- NA
rClean$posts[rClean$posts==" [deleted]"] <- NA
rClean$posts[rClean$posts=="[deleted]"] <- NA

rClean$dates[nchar(rClean$dates)!=24] <- NA
rClean$dates <- ifelse(grepl(':',rClean$dates),rClean$dates,NA)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
keepers <- which(apply(rClean,1,pMiss)==0)

red <- rClean[keepers,]
View(red)

red$dates <- substr(red$dates,5,1000000)
red$year <- substr(red$dates,17,1000000)
red$dates <- substr(red$dates,0,6)
red <- within(red, dates <- paste(dates, year, sep=' '))

red$dates <- as.Date(red$dates, '%b %d %Y')

red$posts = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", red$posts)
# remove at people
red$posts = gsub("@\\w+", "", red$posts)
# remove punctuation
red$posts = gsub("[[:punct:]]", "", red$posts)
# remove numbers
red$posts = gsub("[[:digit:]]", "", red$posts)
# remove html links
red$posts = gsub("http\\w+", "", red$posts)
# remove unnecessary spaces
red$posts = gsub("[ \t]{2,}", "", red$posts)
red$posts = gsub("^\\s+|\\s+$", "", red$posts)
red$posts = gsub('???','',red$posts)
red$posts = gsub('etc','',red$posts)
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
# lower case using try.error with sapply 
red$posts = sapply(red$posts, try.error)

# remove NAs in red$posts
red$posts = red$posts[!is.na(red$posts)]
names(red$posts) = NULL

col=brewer.pal(6,"Dark2")
wordcloud(red$posts, min.freq=50, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)

red <- red[,!(names(red)%in%c('year'))]

redWords <- red %>% tidytext::unnest_tokens(word,posts) %>% anti_join(tidytext::stop_words)
setwd('~/GitHub/PIPPY-data')
ingr <- read.csv('Ingredients.csv')
colnames(ingr) = c('Ingredient')
ingr$Ingredient <- as.character(ingr$Ingredient)
ingr <- ingr %>% tidytext::unnest_tokens(word,Ingredient) %>% anti_join(tidytext::stop_words)
ingr$word <- sapply(ingr$word, try.error)

ingrWordsR <- inner_join(redWords,ingr,by='word')
colnames(ingrWordsR) = c('Date','word')

#write.csv(ingrWordsR,'ingrWordsR.csv')

ingrWordsR <- data.frame(ingrWordsR, source = 'reddit')
ingrWordsR$source <- as.character(ingrWordsR$source)

##########Facebook

library(lubridate)
setwd('~/GitHub/PIPPY-data/Data/Facebook')
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], na.strings = ""))

# Fix all the dates, first AcneAnswers dates are slightly different from the rest!
AcneAnswers_facebook_comments.csv$comment_published <- mdy_hm(AcneAnswers_facebook_comments.csv$comment_published)
AcneAnswers_facebook_statuses.csv$comment_published <- mdy_hm(AcneAnswers_facebook_statuses.csv$comment_published)
facebook_comments_135818293109976.csv$comment_published <- ymd_hms(facebook_comments_135818293109976.csv$comment_published)
facebook_comments_1582733551821102.csv$comment_published <- ymd_hms(facebook_comments_1582733551821102.csv$comment_published)
facebook_statuses_135818293109976.csv$comment_published <- ymd_hms(facebook_statuses_135818293109976.csv$comment_published)
facebook_statuses_1582733551821102.csv$comment_published <- ymd_hms(facebook_statuses_1582733551821102.csv$comment_published)

fb <- rbind(AcneAnswers_facebook_comments.csv[c('comment_message','comment_published')],AcneAnswers_facebook_statuses.csv[c('comment_message','comment_published')],facebook_comments_135818293109976.csv[c('comment_message','comment_published')],
            facebook_comments_1582733551821102.csv[c('comment_message','comment_published')],facebook_statuses_135818293109976.csv[c('comment_message','comment_published')],
            facebook_statuses_1582733551821102.csv[c('comment_message','comment_published')])

pMiss <- function(x){sum(is.na(x))/length(x)*100}

colnames(fb) = c('Text','Date')

fb$Text <- as.character(fb$Text)

fb$Date[fb$Date==""] <- NA
fb$Date[fb$Date=='None']<-NA
fb$Text[fb$Text==""] <- NA
keepers <- which(apply(fb,1,pMiss)==0)
g_fb <- fb[keepers,]


g_fb$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", g_fb$Text)
# remove at people
g_fb$Text = gsub("@\\w+", "", g_fb$Text)
# remove punctuation
g_fb$Text = gsub("[[:punct:]]", "", g_fb$Text)
# remove numbers
g_fb$Text = gsub("[[:digit:]]", "", g_fb$Text)
# remove html links
g_fb$Text = gsub("http\\w+", "", g_fb$Text)
# remove unnecessary spaces
g_fb$Text = gsub("[ \t]{2,}", "", g_fb$Text)
g_fb$Text = gsub("^\\s+|\\s+$", "", g_fb$Text)
g_fb$Text = gsub('???','',g_fb$Text)
g_fb$Text = gsub('etc','',g_fb$Text)
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
# lower case using try.error with sapply 
g_fb$Text = sapply(g_fb$Text, try.error)

# remove NAs in g_fb$Text
g_fb$Text = g_fb$Text[!is.na(g_fb$Text)]
names(g_fb$Text) = NULL

col=brewer.pal(6,"Dark2")
wordcloud(g_fb$Text, min.freq=50, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)

fbWords <- g_fb %>% tidytext::unnest_tokens(word,Text) %>% anti_join(tidytext::stop_words)
setwd("~/GitHub/PIPPY-data")
ingr <- read.csv('Ingredients.csv')
colnames(ingr) = c('Ingredient')
ingr$Ingredient <- as.character(ingr$Ingredient)
ingr <- ingr %>% tidytext::unnest_tokens(word,Ingredient) %>% anti_join(tidytext::stop_words)
ingr$word <- sapply(ingr$word, try.error)
ingrWordsF <- inner_join(fbWords,ingr,by='word')
wordcloud(ingrWordsF$word, min.freq=25,
          random.color=T, max.word=200, random.order=F,colors=col)

ingrWordsF <- data.frame(ingrWordsF, source = 'facebook')
ingrWordsF$source <- as.character(ingrWordsF$source)

###########When combining R and blogs and Facebook
ingrWords <- rbind(ingrWordsR, ingrWordsB,ingrWordsF)
write.csv(ingrWords,'ingrWords.csv')



