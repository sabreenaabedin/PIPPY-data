library(tidyverse)
library(dplyr)
library(plyr)
library(tidytext)
library(reshape2)
library(lubridate)


#### Load in data for blogs ####

setwd('~/GitHub/PIPPY-data/Data/Blogs')

blogs <- data.frame()
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) {assign('blog_name', read.csv(temp[i], na.strings = ""))
  if(!is.null(blog_name$Date)){
    blog_name <- blog_name[c('Date','Text')]
    blogs<- rbind(blogs,blog_name)}
}

#### Clean the data ####

# Remove all NAs
blogs <- blogs[rowSums(is.na(blogs)) == 0,]

# Change text to character 
blogs$Text <- as.character(blogs$Text)

# Change date to dates 
blogs$Date <- as.character(blogs$Date)
blogs$Date <- gsub("\\['|\\']", "", blogs$Date)
blogs$Date <- ymd_hms(blogs$Date)

# Clean text

gBlogs <- blogs

gBlogs$Text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", gBlogs$Text)
gBlogs$Text = gsub("@\\w+", "", gBlogs$Text)
gBlogs$Text = gsub("[[:punct:]]", "", gBlogs$Text)
gBlogs$Text = gsub("[[:digit:]]", "", gBlogs$Text)
gBlogs$Text = gsub("http\\w+", "", gBlogs$Text)
gBlogs$Text = gsub("[ \t]{2,}", "", gBlogs$Text)
gBlogs$Text = gsub("^\\s+|\\s+$", "", gBlogs$Text)
gBlogs$Text = gsub("[^\x01-\x7F]+","",gBlogs$Text)
gBlogs$Text = gsub('???','',gBlogs$Text)
gBlogs$Text = gsub

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

