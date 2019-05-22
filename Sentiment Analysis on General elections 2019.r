#Load required packages
install.packages("rtweet")
library(rtweet)
library(tm) #To perform text mining
library(RCurl) #To fetch URL's & compose http requests
library(ROAuth) #To authenticate twitter account
library(SnowballC) #To implement word stemming for comparision of vocabulary
library(wordcloud) #To create word cloud comparision
library(dplyr) #To perform data manipulation
library(ggplot2) #To perform visualization
library(stopwords)
library(RColorBrewer) #Different color patterns can be used for visualization
library(syuzhet) #To extract sentiment and sentiment-derived plots
library(httr) #To work with URLS's and HTTP

#To access keys and tokens of twitter account. The details can be taken from developer.twitter.com
Consumer_key <- "xxxxxxxxxxxxxx" #copy from developer.twitter.com

Consumer_Secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxx" #copy from developer.twitter.com

Consumer_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" #copy from developer.twitter.com

Consumer_token_Secret <- "xxxxxxxxxxxxxxxxxxxxxxxxx"#copy from developer.twitter.com

#To access twitter account
#setup_twitter_oauth(Consumer_key, Consumer_Secret, Consumer_token, Consumer_token_Secret)
#origop <- options("httr_oauth_cache")
#options(httr_oauth_cache = TRUE)

#Extracting tweets'
#bjp <- searchTwitter("@BJP4India",n= 5000, since = "2019-04-11" , until = "2019-05-20", lang = "English") 
#cong <- searchTwitter("@INCIndia",n= 5000, since = "2019-04-11" , until = "2019-05-20", lang = "English") 

# whatever name you assigned to your created app
appname <- "Senti analysis ML"


# create token named "twitter_token"
twitter_token <- create_token(app = appname, Consumer_key, 
                              Consumer_Secret, Consumer_token, Consumer_token_Secret)

# Retrieve tweets for a particular hashtag
r_stats <- search_tweets("@BJP4India", n = 5000, token = twitter_token)
bjp <- r_stats 
cong_stats <- search_tweets("@INCIndia", n = 5000, token = twitter_token)
cong<- cong_stats

#Clean the tweet data
bjp$text = gsub("&amp", "", bjp$text)
bjp$text = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", bjp$text)
bjp$text = gsub("@\\w+", "", bjp$text)
bjp$text = gsub("[[:punct:]]", "", bjp$text)
bjp$text = gsub("[[:digit:]]", "", bjp$text)
bjp$text = gsub("http\\w+", "", bjp$text)
bjp$text = gsub("[ \t]{2,}", "", bjp$text)
bjp$text = gsub("^\\s+|\\s+$", "", bjp$text)
bjp$text = iconv(bjp$text, "UTF-8", "ASCII", sub = "")

cong$text = gsub("&amp", "", cong$text)
cong$text = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", cong$text)
cong$text = gsub("@\\w+", "", cong$text)
cong$text = gsub("[[:punct:]]", "", cong$text)
cong$text = gsub("[[:digit:]]", "", cong$text)
cong$text = gsub("http\\w+", "", cong$text)
cong$text = gsub("[ \t]{2,}", "", cong$text)
cong$text = gsub("^\\s+|\\s+$", "", cong$text)
cong$text = iconv(cong$text, "UTF-8", "ASCII", sub = "")

# Transform and clean the text
bjpdocs <- Corpus(VectorSource(bjp$text))
congdocs <- Corpus(VectorSource(cong$text))

# Convert the text to lower case
bjpdocs <- tm_map(bjpdocs, content_transformer(tolower))
congdocs <- tm_map(congdocs, content_transformer(tolower))

# Remove numbers
bjpdocs <- tm_map(bjpdocs, removeNumbers)
congdocs <- tm_map(congdocs, removeNumbers)

# Remove english common stopwords
bjpdocs <- tm_map(bjpdocs, removeWords, stopwords("english"))
congdocs <- tm_map(congdocs, removeWords, stopwords("english"))
# Remove punctuations
bjpdocs <- tm_map(bjpdocs, removePunctuation)
congdocs <- tm_map(congdocs, removePunctuation)

# Eliminate extra white spaces
bjpdocs <- tm_map(bjpdocs, stripWhitespace)
congdocs <- tm_map(congdocs, stripWhitespace)

# Text stemming (reduces words to their root form)
bjpdocs <- tm_map(bjpdocs, stemDocument)
congdocs <- tm_map(congdocs, stemDocument)

# Changing to Term document matrix
bjpdtm <- TermDocumentMatrix(bjpdocs)
congdtm <- TermDocumentMatrix(congdocs)
m1 <- as.matrix(bjpdtm)
m2 <- as.matrix(congdtm)
v1 <- sort(rowSums(m1),decreasing=TRUE)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
d2 <- data.frame(word = names(v2),freq=v2)
head(d1, 10)
head(d2, 10)

# Generate the WordCloud for bjp
par(bg="grey30")
wordcloud(d1$word, d1$freq, col=terrain.colors(length(d1$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "BJP Tweets", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)

# Generate the WordCloud for cong
par(bg="grey30")
wordcloud(d2$word, d2$freq, col=terrain.colors(length(d2$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Cong Tweets", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)

#Emotion scores for bjp tweets
emotions <- get_nrc_sentiment(bjp$text)
emo_bar<- colSums(emotions)
emo_sum <- data.frame(count= emo_bar, emotions = names(emo_bar))
emo_sum$emotions <- factor(emo_sum$emotion, 
                           levels = emo_sum$emotion[order(emo_sum$count,  decreasing = T)])
#Emotion scores for cong tweets
emotions_cong <- get_nrc_sentiment(cong$text)
emo_bar_cong<- colSums(emotions_cong)
emo_sum_cong <- data.frame(count= emo_bar_cong, emotions = names(emo_bar_cong))
emo_sum_cong$emotions <- factor(emo_sum_cong$emotion, 
                        levels = emo_sum_cong$emotion[order(emo_sum_cong$count,  decreasing = T)])

#Visualize emotions

bjpplot <- qplot(emotions, data=emo_sum, weight=count, geom="bar", fill=emotions)+ggtitle("BJP Tweet sentiments")
congplot <- qplot(emotions, data=emo_sum_cong, weight=count, geom="bar", fill=emotions)+ggtitle("Cong Tweet sentiments")
bjpplot
congplot 


# Retrieve tweets for a particular hashtag
modi <- search_tweets("@narendramodi", n = 5000, token = twitter_token)

raga <- search_tweets("@RahulGandhi", n = 5000, token = twitter_token)


#Clean the tweet data
modi$text = gsub("&amp", "", modi$text)
modi$text = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", modi$text)
modi$text = gsub("@\\w+", "", modi$text)
modi$text = gsub("[[:punct:]]", "", modi$text)
modi$text = gsub("[[:digit:]]", "", modi$text)
modi$text = gsub("http\\w+", "", modi$text)
modi$text = gsub("[ \t]{2,}", "", modi$text)
modi$text = gsub("^\\s+|\\s+$", "", modi$text)
modi$text = iconv(modi$text, "UTF-8", "ASCII", sub = "")

raga$text = gsub("&amp", "", raga$text)
raga$text = gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", raga$text)
raga$text = gsub("@\\w+", "", raga$text)
raga$text = gsub("[[:punct:]]", "", raga$text)
raga$text = gsub("[[:digit:]]", "", raga$text)
raga$text = gsub("http\\w+", "", raga$text)
raga$text = gsub("[ \t]{2,}", "", raga$text)
raga$text = gsub("^\\s+|\\s+$", "", raga$text)
raga$text = iconv(raga$text, "UTF-8", "ASCII", sub = "")

# Transform and clean the text
modidocs <- Corpus(VectorSource(modi$text))
ragadocs <- Corpus(VectorSource(raga$text))

# Convert the text to lower case
modidocs <- tm_map(modidocs, content_transformer(tolower))
ragadocs <- tm_map(ragadocs, content_transformer(tolower))

# Remove numbers
modidocs <- tm_map(modidocs, removeNumbers)
ragadocs <- tm_map(ragadocs, removeNumbers)

# Remove english common stopwords
modidocs<- tm_map(modidocs, removeWords, stopwords("english"))
ragadocs <- tm_map(ragadocs, removeWords, stopwords("english"))
# Remove punctuations
modidocs <- tm_map(modidocs, removePunctuation)
ragadocs <- tm_map(ragadocs, removePunctuation)

# Eliminate extra white spaces
modidocs <- tm_map(modidocs, stripWhitespace)
ragadocs <- tm_map(ragadocs, stripWhitespace)

# Text stemming (reduces words to their root form)
modidocs <- tm_map(modidocs, stemDocument)
ragadocs <- tm_map(ragadocs, stemDocument)

# Changing to Term document matrix
modidtm <- TermDocumentMatrix(modidocs)
ragadtm <- TermDocumentMatrix(ragadocs)
m3 <- as.matrix(modidtm)
m4 <- as.matrix(ragadtm)
v3 <- sort(rowSums(m3),decreasing=TRUE)
v4 <- sort(rowSums(m4),decreasing=TRUE)
d3 <- data.frame(word = names(v3),freq=v3)
d4 <- data.frame(word = names(v4),freq=v4)
head(d3, 10)
head(d4, 10)

# Generate the WordCloud for bjp
par(bg="grey30")
wordcloud(d3$word, d3$freq, col=terrain.colors(length(d3$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Modi Tweets", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)

# Generate the WordCloud for cong
par(bg="grey30")
wordcloud(d4$word, d4$freq, col=terrain.colors(length(d4$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Raga Tweets", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)

#Emotion scores for bjp tweets
emotionsmodi <- get_nrc_sentiment(modi$text)
emo_barmodi<- colSums(emotionsmodi)
emo_summodi <- data.frame(count= emo_barmodi, emotions = names(emo_barmodi))
emo_summodi$emotions <- factor(emo_summodi$emotion, 
                           levels = emo_summodi$emotion[order(emo_summodi$count,  decreasing = T)])
#Emotion scores for cong tweets
emotions_raga <- get_nrc_sentiment(cong$text)
emo_bar_raga<- colSums(emotions_raga)
emo_sum_raga <- data.frame(count= emo_bar_raga, emotions = names(emo_bar_raga))
emo_sum_raga$emotions <- factor(emo_sum_raga$emotion, 
                                levels = emo_sum_raga$emotion[order(emo_sum_raga$count,  decreasing = T)])

#Visualize emotions

modiplot <- qplot(emotions, data=emo_summodi, weight=count, geom="bar", fill=emotions)+ggtitle("Modi Tweet sentiments")
ragaplot <- qplot(emotions, data=emo_sum_raga, weight=count, geom="bar", fill=emotions)+ggtitle("Rahul Tweet sentiments")
modiplot
ragaplot 