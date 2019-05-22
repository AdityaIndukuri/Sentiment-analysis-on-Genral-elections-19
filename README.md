# Sentiment-analysis-on-Genral-elections-19
Sentiment analysis on recently concluded general elections and derive the emotions from the tweets. This helps us to predict the winner for Parliament elections in 2019.
Abstract

Sentiment analysis is perhaps one of the most popular applications of NLP. It is the process of computationally identifying and categorizing opinions expressed in a piece of text, especially in order to determine whether the writer’s attitude towards a particular topic. Microblogging websites like twitter has become a good source of information where people post real time messages about their opinions on various topics. In this article we are going to perform sentiment analysis on recently concluded general elections and derive the emotions from the tweets. This helps us to predict the winner for Parliament elections in 2019.

Steps to conduct the sentiment analysis

To perform this analysis I have considered below twitter handles to collect the twitter data

"@BJP4India" - 3487 observations were retrieved 
@INCIndia - 3151 observations were retrieved
@narendramodi - 3132 observations were retrieved
@RahulGandhi - 3661 observations were retrieved
We have used R programming language with below main libraries to conduct this analysis.

rtweet -  To collect twitter data
tm - To perform text mining
RCurl - To fetch URL's & compose http requests
ROAuth - To authenticate twitter account
SnowballC - To implement word stemming for comparision of vocabulary
wordcloud - To create word cloud comparision
ggplot2 - To visualize
RColorBrewer - To use different color patterns for visualization
syuzhet - To extract sentiment and sentiment-derived plots
Prerequisite: Need to have a twitter developer account with keys and tokens created
