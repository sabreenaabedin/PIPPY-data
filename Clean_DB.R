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
library(SnowballC)
library(genderizeR)
library(gender)
library(tidytext)
library(topicmodels)
library(reshape2)
library(lubridate)

###Sentiment Scoring Function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

positives= readLines("~/GitHub/PIPPY-data/positive_words.txt")
negatives= readLines("~/GitHub/PIPPY-data/negative_words.txt")

#####Pulling Ingredients

setwd('~/GitHub/PIPPY-data/Data/Ingredients')
ingr <- read.csv('ingredients.csv')

ingr$Ingredient <- as.character(ingr$Ingredient)

ingr$Ingredient = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", ingr$Ingredient)
# remove at people
ingr$Ingredient = gsub("@\\w+", "", ingr$Ingredient)
# remove punctuation
ingr$Ingredient = gsub("[[:punct:]]", "", ingr$Ingredient)
# remove numbers
ingr$Ingredient = gsub("[[:digit:]]", "", ingr$Ingredient)
# remove html links
ingr$Ingredient = gsub("http\\w+", "", ingr$Ingredient)
# remove unnecessary spaces
ingr$Ingredient = gsub("[ \t]{2,}", "", ingr$Ingredient)
ingr$Ingredient = gsub("^\\s+|\\s+$", "", ingr$Ingredient)
ingr$Ingredient = gsub('???','',ingr$Ingredient)
ingr$Ingredient = gsub('etc','',ingr$Ingredient)

#ingr <- ingr %>% tidytext::unnest_tokens(word,Ingredient) %>% anti_join(tidytext::stop_words)
#ingr$word <- sapply(ingr$word, try.error)

#####Pulling Branding Words
setwd('~/GitHub/PIPPY-data')
brand <- read.csv('Branding.csv')
colnames(brand) = c('BrandWord')
brand$BrandWord <- as.character(brand$BrandWord)
brand <- brand %>% tidytext::unnest_tokens(word, BrandWord) %>% anti_join(tidytext::stop_words)

#####Pulling Application Methods

apps <- read.csv('ApplicationMethods.csv')
colnames(apps) <- c('ApplicationMethod')
apps$ApplicationMethod <- as.character(apps$ApplicationMethod)
apps <- apps %>% tidytext::unnest_tokens(word, ApplicationMethod) %>% anti_join(tidytext::stop_words)

#### Blogs

#setwd('D:/Users/tss3dn/Desktop/PIPPY-data/Data/Blogs')
setwd('~/GitHub/PIPPY-data/Data/Blogs')

blogs <- data.frame()
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) {assign('blog_name', read.csv(temp[i], na.strings = ""))
  if(!is.null(blog_name$Date)){
    blog_name <- blog_name[c('Date','Text')]
  blogs<- rbind(blogs,blog_name)}
}

#theBeautyBrains <- read.csv('items_thebeautybrains.com_1.csv')
#hudaBeauty <- read.csv('items_hudabeauty.com_1.csv')
#hotBeatyHealth <- read.csv('items_www.hotbeautyhealth.com_1.csv')
#musings <- read.csv('items_www.musingsofamuse.com_1.csv')
#womenStuff <- read.csv('items_www.mywomenstuff.com_1.csv')
#vivaWoman <- read.csv('items_www.vivawoman.net_1.csv')
#trendHunter <- read.csv('items_www.trendhunter.com_1.csv')
#trendHunter2 <- read.csv('items_www.trendhunter.com_2.csv')
#trendHunter <- rbind(trendHunter, trendHunter2)
#rm(trendHunter2)
#temptalia <- read.csv('items_www.temptalia.com_1.csv')

#blogs <- rbind(theBeautyBrains[c('Date','Text')], hudaBeauty[c('Date','Text')], hotBeatyHealth[c('Date','Text')], musings[c('Date','Text')],womenStuff[c('Date','Text')],vivaWoman[c('Date','Text')])#,trendHunter[c('Date','Text')],temptalia[c('Date','Text')])

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
#wordcloud(gBlogs$Text, min.freq=50, scale=c(5,2),rot.per = 0.25,
#     random.color=T, max.word=10, random.order=F,colors=col)

Score <- score.sentiment(gBlogs$Text,positives,negatives,.progress='none')

gBlogs <- cbind(gBlogs, Score$score)
gBlogs <- data.frame(gBlogs, post_id = seq.int(nrow(gBlogs)))

gBlogs <- data.frame(gBlogs, source = 'blog')
gBlogs$source <- as.character(gBlogs$source)

blogWords <- gBlogs %>% tidytext::unnest_tokens(word,Text, drop=FALSE) %>% anti_join(tidytext::stop_words)
#blogWords <- blogWords[,!names(blogWords)%in%c('Date','Text','Score.score','source')]

#ingrWordsB <- inner_join(blogWords,ingr,by='word')

brandWordsB <- inner_join(blogWords,brand,by='word')

appsB <- inner_join(blogWords,apps,by='word')

ingrWordsB_test <- data.frame(ingredient="",post_id="")
for (word in ingr$Ingredient){
  #print(word)
  if(length(gBlogs$post_id[grep(word,gBlogs$Text)])>0){
    temp <- data.frame(ingredient=word,post_id = as.character(gBlogs$post_id[grep(word,gBlogs$Text)]))
    ingrWordsB_test <- rbind(ingrWordsB_test,temp)}
}
colnames(ingrWordsB_test)=c('Ingredient','post_id')
ingrWordsB_test$Ingredient <- as.character(ingrWordsB_test$Ingredient)
ingrWordsB_test <- inner_join(ingrWordsB_test,ingr,by='Ingredient')

data.frame(gBlogs$post_id[grep("cyclopentasiloxane",gBlogs$Text)])
#temp<-data.frame(word='oil',post_id=gBlogs$post_id[grep("oil",gBlogs$Text)])
#temp

#if(length(gBlogs$post_id[grep('cinnamon',gBlogs$Text)])>0){
#  temp <- data.frame(ingredient='cinnamon',post_id = as.character(gBlogs$post_id[grep('cinnamon',gBlogs$Text)]))
# ingrWordsB_test <- rbind(ingrWordsB_test,temp)}

###### Reddit


#setwd('D:/Users/tss3dn/Desktop/PIPPY-data/Data/Reddit')
setwd('~/GitHub/PIPPY-data/Data/Reddit')

reddit <- read.delim('redditWithTimestamp30k.csv')
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
#View(red)

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

red <- red[,!(names(red)%in%c('year'))]

Score <- score.sentiment(red$posts,positives,negatives,.progress='none')

red <- cbind(red, Score$score)

red <- data.frame(red, post_id = nrow(gBlogs)+seq.int(nrow(red)))

red <- data.frame(red, source = 'reddit')
red$source<-as.character(red$source)

colnames(red) = c('Text','Date','Score.score','post_id','source')


redWords <- red %>% tidytext::unnest_tokens(word,Text, drop=FALSE) %>% anti_join(tidytext::stop_words)
colnames(redWords) = c('Text','Date','Score.score','post_id','source','word')
#redWords <- redWords[,!names(redWords)%in%c('Date','Text','Score.score','source')]

#ingrWordsR <- inner_join(redWords,ingr,by='word')
#colnames(ingrWordsR) = c('post_id','word','Rating','Description')


brandWordsR <- inner_join(redWords,brand,by='word')
colnames(brandWordsR) = c('post_id','word')

appsR <- inner_join(redWords,apps,by='word')
colnames(appsR) = c('Text','Date','Score.score','post_id','source','word')

ingrWordsR_test <- data.frame(ingredient="",post_id="")
for (word in ingr$Ingredient){
  #print(word)
  if(length(red$post_id[grep(word,red$Text)])>0){
    temp <- data.frame(ingredient=word,post_id = as.character(red$post_id[grep(word,red$Text)]))
    ingrWordsR_test <- rbind(ingrWordsR_test,temp)}
}
colnames(ingrWordsR_test)=c('Ingredient','post_id')
ingrWordsR_test$Ingredient <- as.character(ingrWordsR_test$Ingredient)
ingrWordsR_test <- inner_join(ingrWordsR_test,ingr,by='Ingredient')


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


fb <- rbind(AcneAnswers_facebook_comments.csv[c('comment_message','comment_published')],AcneAnswers_facebook_statuses.csv[c('comment_message','comment_published')],facebook_comments_135818293109976.csv[c('comment_message','comment_published')],
            facebook_comments_1582733551821102.csv[c('comment_message','comment_published')],facebook_statuses_135818293109976.csv[c('comment_message','comment_published')],
            facebook_statuses_1582733551821102.csv[c('comment_message','comment_published')])

# Get term document matrix and document term matrix 
dtmm <- DocumentTermMatrix(doxm)
tdmm <- TermDocumentMatrix(doxm)

# Remove all NAs
fb <- fb[rowSums(is.na(fb)) == 0,]

fb_lim <- fb[c('comment_message','comment_published')]

pMiss <- function(x){sum(is.na(x))/length(x)*100}

colnames(fb_lim) = c('Text','Date')
fb_lim$Text <- as.character(fb_lim$Text)
fb_lim$Date <- as.character(fb_lim$Date)

fb_lim$Date[fb_lim$Date==""] <- NA
fb_lim$Date[fb_lim$Date=='None']<-NA
fb_lim$Text[fb_lim$Text==""] <- NA
keepers <- which(apply(fb_lim,1,pMiss)==0)
g_fb <- fb_lim[keepers,]
g_fb$Date <- as.Date(g_fb$Date)
keepers <- which(apply(g_fb,1,pMiss)==0)
g_fb <- g_fb[keepers,]

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
#wordcloud(g_fb$Text, min.freq=50, scale=c(5,2),rot.per = 0.25,
#random.color=T, max.word=100, random.order=F,colors=col)

Score <- score.sentiment(g_fb$Text,positives,negatives,.progress='none')

g_fb <- cbind(g_fb, Score$score)

g_fb <- data.frame(g_fb, source = 'facebook')
g_fb$source <- as.character(g_fb$source)

g_fb <- data.frame(g_fb, post_id = nrow(gBlogs)+nrow(red)+seq.int(nrow(g_fb)))

fbWords <- g_fb %>% tidytext::unnest_tokens(word,Text,drop=FALSE) %>% anti_join(tidytext::stop_words)
#fbWords <- fbWords[,!names(fbWords)%in%c('Date','Text','Score.score','source')]


#ingrWordsF <- inner_join(fbWords,ingr,by='word')

brandWordsF <- inner_join(fbWords,brand,by='word')

appsF <- inner_join(fbWords,apps,by='word')
colnames(appsF) = c('Text','Date','Score.score','source','post_id','word')

ingrWordsF_test <- data.frame(ingredient="",post_id="")
for (word in ingr$Ingredient){
  #print(word)
  if(length(g_fb$post_id[grep(word,g_fb$Text)])>0){
    temp <- data.frame(ingredient=word,post_id = as.character(g_fb$post_id[grep(word,g_fb$Text)]))
    ingrWordsF_test <- rbind(ingrWordsF_test,temp)}
}
colnames(ingrWordsF_test)=c('Ingredient','post_id')
ingrWordsF_test$Ingredient <- as.character(ingrWordsF_test$Ingredient)
ingrWordsF_test <- inner_join(ingrWordsF_test,ingr,by='Ingredient')


###########When combining R and blogs and Facebook
#ingrWords <- rbind(ingrWordsR, ingrWordsB,ingrWordsF)
#ingrWords <- data.frame(ingrWords, type = 'ingredient')
#ingrWords$type<-as.character(ingrWords$type)
#ingrWords$Description <- as.character(ingrWords$Description)
#ingrWords$Rating <- as.character(ingrWords$Rating)

ingrWords_test <- rbind(ingrWordsR_test, ingrWordsB_test,ingrWordsF_test)
ingrWords_test <- data.frame(ingrWords_test, type = 'ingredient')
ingrWords_test$type<-as.character(ingrWords_test$type)
ingrWords_test$Description <- as.character(ingrWords_test$Description)
ingrWords_test$Rating <- as.character(ingrWords_test$Rating)

brandWordsF <- data.frame(brandWordsF, source = 'facebook')
brandWordsF$source <- as.character(brandWordsF$source)
brandWordsB <- data.frame(brandWordsB, source = 'blog')
brandWordsB$source <- as.character(brandWordsB$source)
brandWordsR <- data.frame(brandWordsR, source = 'reddit')
brandWordsR$source <- as.character(brandWordsR$source)
brandWords <- rbind(brandWordsF, brandWordsB, brandWordsR)
brandWords <- data.frame(brandWords, type = 'branding')
brandWords$type <- as.character(brandWords$type)

appsF <- data.frame(appsF, source = 'facebook')
appsF$source <- as.character(appsF$source)
appsB <- data.frame(appsB, source = 'blog')
appsB$source <- as.character(appsB$source)
appsR <- data.frame(appsR, source = 'reddit')
appsR$source <- as.character(appsR$source)
applicationMethods <- rbind(appsF, appsB, appsR)
applicationMethods<- data.frame(applicationMethods, type = 'application method')
applicationmethods$type <- as.character(applicationMethods$type)

#socialMediaWords <- rbind(ingrWords,brandWords,apps)

posts <- rbind(gBlogs,red,g_fb)
write.csv(posts,'postData.csv')

setwd('~/GitHub/PIPPY-data/')

#write.csv(ingrWords,'ingrWords.csv')
write.csv(brandWords,'brandWords.csv')
write.csv(apps,'formsWords.csv')
#write.csv(rbind(ingrWords,brandWords,apps),'socialMediaWords.csv')

write.csv(ingrWords_test,'ingrWords_test.csv')
