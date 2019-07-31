library(twitteR)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)
library(wordcloud)

# Declare Twitter API Credentials
api_key <- "nOMPqC5PNtElymf02yilCTbLH" 
api_secret <- "QlVUeKwt4vB1Qkw0LS4OaXrMAU000nPUFO7EXbu1wWADv8yNHX" 
token <- "1977401202-j67KajnfdJjVnvewXvakyWDJVD6GulJm1u81Reh"
token_secret <- "vOARZdGRQFVw0pwDakwnzaQup9ysSkd1PqzAVLR82VOL5"

setup_twitter_oauth(api_key, api_secret, token, token_secret)

tweets <- searchTwitter("deadmau5", n=200)

#print (tweets)

tweets.df <- twListToDF(tweets)
tweets = sapply(tweets,function(x) x$getText())

# CLEANING BEGINS

tweets_cl <- gsub('\\p{So}|\\p{Cn}', '', tweets, perl = TRUE)
tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets_cl)
tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
tweets_cl = gsub("@\\w+", "", tweets_cl)
tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
tweets_cl <- gsub('\\d+', '', tweets_cl)

tweets = tolower(tweets_cl)
print("LOWER DONE")

tweetsb = tweets

############## STORING CLEAN TWEETS IN A SEPARATE VARIABLE ###
#tweets_clean <- tweets


#print (tweets)
#map_chr converts to Vector
#tweetaftermap <- map_chr(tweets, function(x) x$text)
#print ("vector thing done!")

#mySentiment <- get_nrc_sentiment(tweetaftermap)

mySentiment <- get_nrc_sentiment(tweets)
print ("sentiment analysis done!")

write.csv(tweets, file = "trumpet3.csv")
print("written to file")


print("***** PRINTING SENTIMENT *****")
#print(mySentiment)

tweets <- cbind(tweets, mySentiment)
print ("Sentiment binding done")

#tweets

print("******** PRINTINT t ******")
t<-tweets[c(2:11)]
#print(t)

print("**** SENTIMENT TOTALS")

sentimentTotals <- data.frame(colSums(tweets[c(2:11)]))
sentimentTotals
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), 
sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
        geom_bar(aes(fill = sentiment), stat = "identity") +
        theme(legend.position = "none") +
        xlab("Sentiment") + ylab("Total Count") + ggtitle("Total 
Sentiment Score for All Tweets")

# sentimentTotals <- data.frame(colSums(tweets[,c(11:18)]))
# print("data frame column done")
# names(sentimentTotals) <- "count"
# sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
# rownames(sentimentTotals) <- NULL
# ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
#       geom_bar(aes(fill = sentiment), stat = "identity") +
#       theme(legend.position = "none") +
#       xlab("Sentiment") + ylab("Total Count") + ggtitle("Total #Sentiment Score for All Tweets")

