#  Twitter data analysis through API and R integration for sentiments of Iphones vs Samsung
# and using data for visualization in Tableau

### Installing packages required
# install.packages('twitteR')
# install.packages('ROAuth')
# install.packages("RCurl")
# install.packages("httr")
# install.packages("stringr")
# install.packages('plyr')
# install.packages("dplyr")
# install.packages("tm")


### Twitter Text Mining - Required Libraries
library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)
library(stringr)
library(plyr)
library(dplyr)
library(tm)


library(ggmap)

#install.packages("wordcloud")
library(wordcloud)

### Establishing A Connection - Direct Method
key="SS*****************f"
secret="p*****************************Zo"
atoken = "9*************************************9"
asecret = "hkR5************************************DE"

setup_twitter_oauth(key, secret, atoken, asecret)

searchTwitter("Samsung")


### Sentiment Score Function - approach after J. Breen
library("stringr")
library("plyr")

## scrapping the tweets with searchTwitter
tweets = searchTwitter("apple+iphone",n=2000, lang = "en", geocode = "34.1,-118.2,150mi")

# User specific search
searchTwitter("from:Udemy")

# time specific searches
searchTwitter("apple+iphone", n=100, since = "2017-10-17") #until resulttype

# Get details of the tweets
tweettext = sapply(tweets, function(x) x$getText())
tweettext

# Get details of the time tweet was posted
tweetdate = lapply(tweets, function(x) x$getCreated())

tweetdate = sapply(tweets, function(x) strftime(x,format = "%Y-%m-%d %H:%M:%S",tz = "UTC"))

isretweet = sapply(tweets, function(x) x$getIsRetweet())

retweetcount = sapply(tweets, function(x) x$getRetweetCount())

# Generic cleaning
# Convert latin1 syntax to ASCII and eliminate empty cells
tweettext = lapply(tweettext, function(x) iconv(x,"latin1","ASCII",sub = ""))

# Specific cleaning
tweettext = lapply(tweettext, function(x) gsub("htt.*","",x))

tweettext = lapply(tweettext, function(x) gsub("#","",x))

tweettext = unlist(tweettext)

#getting the opinion lexicons from working directory
#setwd("C:/Users/Arora's Den/Documents/MS - Business Analytics/Spring-18/Visualization/Twitter Data")
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")


# Function is called sentimentfun
sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  # Parameters
  # tweettext: vector of text to score
  # pos: vector of words of postive sentiment
  # neg: vector of words of negative sentiment
  # .progress: passed to laply() 4 control of progress bar
  
  # create simple array of scores with laply
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   # remove punctuation - using global substitute
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   # remove control characters
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   # remove digits
                   singletweet = gsub("\\d+", "", singletweet)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
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
                   # use tryTolower with sapply
                   singletweet = sapply(singletweet, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )
  
  # data frame with scores for each sentence
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

### Using searchTwitter for our project
# terms = apple+iphone, samsung+galaxy
## geocode:
#lats=c(40.7,51.5,34.1,43.7)
#lons=c(-74,-0.1,-118.2,-79.4)
#cities = New York, London, Los Angeles, Toronto

#scraping the tweets with searchTwitter
tweets = searchTwitter("apple+iphone", n=2000,
                       lang="en",
                       geocode="34.1,-118.2,150mi")
searchTwitter("from:Udemy") # user specific
searchTwitter("apple+iphone", n = 100,
              since = "2017-10-17") # time specific

# extracting the text
tweettext = sapply(tweets, function(x) x$getText())

## first cleaning stage
tweettext=lapply(tweettext, function(x) iconv(x, "latin1","ASCII", sub=""))
tweettext=lapply(tweettext, function(x) gsub("htt.*",' ',x))
tweettext=lapply(tweettext, function(x) gsub("#",'',x))
tweettext=unlist(tweettext)

# getting the opinion lexicons from working directory
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
neg2 = c(neg, "bearish", "fraud"); tail(neg2)

## apply function score.sentiment
scores = sentimentfun(tweettext, pos, neg, .progress='text')

## extracting further elements (besides text) for the export csv
tweetdate=lapply(tweets, function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
isretweet=sapply(tweets, function(x) x$getIsRetweet())
retweetcount=sapply(tweets, function(x) x$getRetweetCount())
favoritecount=sapply(tweets, function(x) x$getFavoriteCount())

## Creating the Data Frame
data=as.data.frame(cbind(ttext=tweettext, date=tweetdate, isretweet=isretweet, retweetcount=retweetcount,
                         favoritecount=favoritecount, score = scores$score, product = "Apple Iphone", city = "Los Angeles", country = "USA"))
## remove duplicates
data2 = duplicated(data[,1])
data$duplicate = data2

## create file for Tableau
write.csv(data, file= "apple_langeles.csv")

