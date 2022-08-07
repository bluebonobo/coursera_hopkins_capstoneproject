# Packages
library(knitr)       # used to make kable tables
library(tm)          # text mining package
# library(SnowballC)   # applies Porter's stemmming algorithm (discussed later) 
# library(magrittr)    # allows pipe operator
# library(tidytext)    # tidy text for plots
library(ggplot2)     # used for plots
library(dplyr)       # Manipulate data frames
library(sentimentr)    # Remove Profanity
library(quanteda)      # Text Analysis and Corpus Managment leading library
library(LaF)           # need to install Large Ascii Files library install.packages('LaF') to sample source files
library(wordcloud)
library(NLP)

# Setting up the working directory same as this source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
print(getwd())

# # # Getting the data
# working_english_corpus  <- VCorpus(DirSource("./dataset/final/en_US/sample/"), readerControl = list(language="lat")) 
# 
# # Checking few records
# # First 2 records in the english blog file
# working_english_corpus[[1]][1]$content[1]
# working_english_corpus[[1]][1]$content[2]
# # First 2 records in the english news file
# working_english_corpus[[2]][1]$content[1]
# working_english_corpus[[2]][1]$content[2]
# # first 2 records in the english twitter file
# working_english_corpus[[3]][1]$content[1]
# working_english_corpus[[3]][1]$content[2]
# 
# # how many lines of text in each file en_US file
# print(paste("number lines in twitter file", lapply(working_english_corpus[[3]][1],length), sep = ":"))
# print(paste("number lines in blogs file", lapply(working_english_corpus[[1]][1],length), sep = ":"))
# print(paste("number lines in news file", lapply(working_english_corpus[[2]][1],length), sep = ":"))
# 
# # how many lines of text in each file en_US file
# 
# 
# 
# print ("========= Cleaning the Corpus")
# # we follow the following doc re. preprocessing https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# # eliminate extra whitespaces
# working_english_corpus <- tm_map(working_english_corpus, stripWhitespace)
# working_english_corpus <- tm_map(working_english_corpus, content_transformer(tolower))
# working_english_corpus <- tm_map(working_english_corpus, removeWords, stopwords("english"))
# working_english_corpus <- tm_map(working_english_corpus, removePunctuation)
# working_english_corpus <- tm_map(working_english_corpus, content_transformer(removeNumbers))
# working_english_corpus <- tm_map(working_english_corpus, content_transformer(stemDocument), language = "english")
# 
# print ("========= Remove Profanity")
# 
# print ("========= Tokenize Corpus")
# tokenized_working_english_corpus <- function(x) tokenize_word(working_english_corpus)
# 
# print ("========= Create Term Document Matrix")
# term <- DocumentTermMatrix(working_english_corpus)
# # term <- DocumentTermMatrix(working_english_corpus, control = list(tokenize = tokenized_working_english_corpus))
# 
# 
# freq <- sort(colSums(as.matrix(term)), decreasing=TRUE)
# print ("========= Most used")
# print(head(freq,10))
# print ("========= Least used")
# print(tail(freq,10))
# 
# top20 <- head(freq,10)
# bottom20 <-tail(freq,10)
# 
# barplot(top20, main="top20 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE, las = 2)
# barplot(bottom20, main="bottom20 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE, las = 2)
# 
# 
# # wordcloud
# wordcloud(names(freq), freq, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)


# ============================== Exploratory Analysis BLOGS Data

# # Getting the data
working_english_corpus_blogs  <- VCorpus(DirSource("./dataset/final/en_US/sample/blogs_sample"), readerControl = list(language="lat")) 

print ("========= Cleaning the Corpus")
# we follow the following doc re. preprocessing https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# eliminate extra whitespaces
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, stripWhitespace)
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, content_transformer(tolower))
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, removeWords, stopwords("english"))
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, removePunctuation)
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, content_transformer(removeNumbers))
working_english_corpus_blogs <- tm_map(working_english_corpus_blogs, content_transformer(stemDocument), language = "english")

print ("========= Remove Profanity")

# print ("========= Tokenize Corpus")
# tokenized_working_english_corpus_blogs <- function(x) tokenize_word(working_english_corpus_blogs)

print ("========= Create Term Document Matrix")
dtm_blogs <- DocumentTermMatrix(working_english_corpus_blogs)
freq_blogs <- sort(colSums(as.matrix(dtm_blogs)), decreasing=TRUE)
top10_blogs <- head(freq_blogs,10)
bottom10_blogs <-tail(freq_blogs,10)
barplot(top10_blogs, main="Blogs top10 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE, las = 2)
# wordcloud
wordcloud(names(freq_blogs), freq_blogs, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)

# how many lines of text in each file en_US file
print(paste("Blog File : number lines", lapply(working_english_corpus_blogs[[1]][1],length), sep = ":"))

# how many lines of text in each file en_US file
print(paste("Blog file : number terms", dim(dtm_blogs)[2], sep = ":"))

# # Tokenize Corpus
# Bigramtokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# bigramDocumentMatrix <- TermDocumentMatrix(working_english_corpus_blogs,control = list(tokenize = Bigramtokenizer))
# # Find frequency of tokenized terms (this is so clunky in R!! Had to find this code)
# bigramf <- findFreqTerms(bigramDocumentMatrix, lowfreq=0)
# Bigramfreq <- rowSums(as.matrix(bigramDocumentMatrix[bigramf,]))
# # Create dataframe with word and frequency columns
# Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)
# Bigramfreq_sort<- Bigramfreq[order(-Bigramfreq$frequency),]

# Tokenize Corpus Bigrams
Bigramtokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
bigramDocumentMatrix <- TermDocumentMatrix(working_english_corpus_blogs,control = list(tokenize = Bigramtokenizer))
# Find frequency of tokenized terms (this is so clunky in R!! Had to find this code)
bigramf <- findFreqTerms(bigramDocumentMatrix, lowfreq=0)
Bigramfreq <- rowSums(as.matrix(bigramDocumentMatrix[bigramf,]))
# Create dataframe with word and frequency columns. Most frequent first
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)
Bigramfreq_sort<- Bigramfreq[order(Bigramfreq$frequency),]

# Tokenize Corpus 3-grams
Trigramtokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
trigramDocumentMatrix <- TermDocumentMatrix(working_english_corpus_blogs,control = list(tokenize = Trigramtokenizer))
# Find frequency of tokenized terms (this is so clunky in R!! Had to find this code)
trigramf <- findFreqTerms(trigramDocumentMatrix, lowfreq=0)
Trigramfreq <- rowSums(as.matrix(trigramDocumentMatrix[trigramf,]))
# Create dataframe with word and frequency columns. Most frequent first
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)
Trigramfreq_sort<- Trigramfreq[order(Trigramfreq$frequency),]


#  use this https://r-graph-gallery.com/210-custom-barplot-layout.html
termfrequencygraph <- function(data,title,num){
  df <- data[order(-data$frequency),][1:num,]
  barplot(names.arg = df[1:num,]$word, las = 2, df[1:num,]$freq,
          col ="gray",  cex.names=0.5, cex.axis =0.5,
          main = title,
          xlab = "Word frequencies",
          horiz=TRUE)
}

# Increase margin size
par(mar=c(4,8,4,4))
termfrequencygraph(Bigramfreq_sort,"Blogs Top 10 Bigrams", 10)
termfrequencygraph(Trigramfreq_sort,"Blogs Top 10 Trigrams", 10)
# Restore margin size
par(mar=c(4,4,4,4))



# ============================== Exploratory Analysis NEWS Data

# # Getting the data
working_english_corpus_news  <- VCorpus(DirSource("./dataset/final/en_US/sample/news_sample"), readerControl = list(language="lat")) 

print ("========= Cleaning the Corpus")
# we follow the following doc re. preprocessing https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# eliminate extra whitespaces
working_english_corpus_news <- tm_map(working_english_corpus_news, stripWhitespace)
working_english_corpus_news <- tm_map(working_english_corpus_news, content_transformer(tolower))
working_english_corpus_news <- tm_map(working_english_corpus_news, removeWords, stopwords("english"))
working_english_corpus_news <- tm_map(working_english_corpus_news, removePunctuation)
working_english_corpus_news <- tm_map(working_english_corpus_news, content_transformer(removeNumbers))
working_english_corpus_news <- tm_map(working_english_corpus_news, content_transformer(stemDocument), language = "english")

print ("========= Remove Profanity")

# print ("========= Tokenize Corpus")
# tokenized_working_english_corpus_blogs <- function(x) tokenize_word(working_english_corpus_blogs)

print ("========= Create Term Document Matrix")
dtm_news <- DocumentTermMatrix(working_english_corpus_news)
freq_news <- sort(colSums(as.matrix(dtm_news)), decreasing=TRUE)
top10_news <- head(freq_news,10)
bottom10_news <-tail(freq_news,10)
barplot(top10_news, main="News top10 words frequency", ylab = "Names", xlab="Frequency", horiz = TRUE, las = 2)
# wordcloud
wordcloud(names(freq_news), freq_news, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)

# how many lines of text in each file en_US file
print(paste("News file : number lines", lapply(working_english_corpus_news[[1]][1],length), sep = ":"))

# how many lines of text in each file en_US file
print(paste("News file : number terms", dim(dtm_news)[2], sep = ":"))


# ============================== Exploratory Analysis Twitter Data

# # Getting the data
working_english_corpus_twitter  <- VCorpus(DirSource("./dataset/final/en_US/sample/twitter_sample"), readerControl = list(language="lat")) 

print ("========= Cleaning the Corpus")
# we follow the following doc re. preprocessing https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# eliminate extra whitespaces
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, stripWhitespace)
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, content_transformer(tolower))
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, removeWords, stopwords("english"))
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, removePunctuation)
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, content_transformer(removeNumbers))
working_english_corpus_twitter <- tm_map(working_english_corpus_twitter, content_transformer(stemDocument), language = "english")

print ("========= Remove Profanity")

# print ("========= Tokenize Corpus")
# tokenized_working_english_corpus_blogs <- function(x) tokenize_word(working_english_corpus_blogs)

print ("========= Create Term Document Matrix")
dtm_twitter <- DocumentTermMatrix(working_english_corpus_twitter)
freq_twitter <- sort(colSums(as.matrix(dtm_twitter)), decreasing=TRUE)
top10_twitter <- head(freq_twitter,10)
bottom10_twitter <-tail(freq_twitter,10)
barplot(top10_twitter, main="Twitter top10 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE, las = 2)
# wordcloud
wordcloud(names(freq_twitter), freq_twitter, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)

# how many lines of text in each file en_US file
print(paste("Twitter file : number lines", lapply(working_english_corpus_twitter[[1]][1],length), sep = ":"))

# how many lines of text in each file en_US file
print(paste("Twitter file : number terms", dim(dtm_twitter)[2], sep = ":"))


