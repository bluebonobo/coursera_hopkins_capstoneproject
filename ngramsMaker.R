#based on work done on week3 (week3.R)


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
library(tidyr)
library(RWeka)

# Setting up the working directory same as this source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
print(getwd())


start_time <- Sys.time()

# ============================== Exploratory Analysis BLOGS Data

# # Getting the data
# working_english_corpus_blogs  <- VCorpus(DirSource("./dataset/final/en_US/sample/blogs_sample"), readerControl = list(language="lat")) 
# working_english_corpus_blogs  <- VCorpus(DirSource("./dataset/final/en_US"), readerControl = list(language="lat"))
working_english_corpus_blogs  <- VCorpus(DirSource("./dataset/final/en_US/sample"), readerControl = list(language="lat"))

print(dim(working_english_corpus_blogs))
print(working_english_corpus_blogs)



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

# print ("========= Create Term Document Matrix")
# dtm_blogs <- DocumentTermMatrix(working_english_corpus_blogs)
# freq_blogs <- sort(colSums(as.matrix(dtm_blogs)), decreasing=TRUE)
# top10_blogs <- head(freq_blogs,10)
# bottom10_blogs <-tail(freq_blogs,10)
# barplot(top10_blogs, main="Blogs top10 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE, las = 2)
# # wordcloud
# wordcloud(names(freq_blogs), freq_blogs, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)
# 
# # how many lines of text in each file en_US file
# print(paste("Blog File : number lines", lapply(working_english_corpus_blogs[[1]][1],length), sep = ":"))
# 
# # how many lines of text in each file en_US file
# print(paste("Blog file : number terms", dim(dtm_blogs)[2], sep = ":"))


# print ("========================= BiGrams =========================")
# 
# start_time_bigrams <- Sys.time()
# 
# # Tokenize Corpus Bigrams
# Bigramtokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# bigramDocumentMatrix <- TermDocumentMatrix(working_english_corpus_blogs,control = list(tokenize = Bigramtokenizer))
# # Find frequency of tokenized terms (this is so clunky in R!! Had to find this code)
# bigramFreqTerms<- findFreqTerms(bigramDocumentMatrix, lowfreq=0)
# bigramFreq <- rowSums(as.matrix(bigramDocumentMatrix[bigramFreqTerms,]))
# # Create dataframe with word and frequency columns. Most frequent first
# bigramFreq <- data.frame(word=names(bigramFreq),frequency=bigramFreq)
# bigramFreq_sort<- bigramFreq[order(bigramFreq$frequency),]
# 
# print(dim(bigramFreq_sort))
# print(tail(bigramFreq_sort))
# 
# bigramTable <- separate(bigramFreq_sort, col=word, into=c('word1', 'word2'), sep = " ")
# 
# print(typeof(bigramTable))
# print(dim(bigramTable))
# print(tail(bigramTable))
# 
# end_time_bigrams <- Sys.time()
# print(paste("bigrams execution time >> ", end_time_bigrams - start_time_bigrams))
# 
# saveRDS(bigramTable, "./ngramsTables/bigramTable.rds")
# 

print ("========================= TriGrams =========================")

start_time_trigrams <- Sys.time()

# Trigramtokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

Trigramtokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))


trigramDocumentMatrix <- TermDocumentMatrix(working_english_corpus_blogs,control = list(tokenize = Trigramtokenizer))
trigramFreqTerms<- findFreqTerms(trigramDocumentMatrix, lowfreq=0)
trigramFreq <- rowSums(as.matrix(trigramDocumentMatrix[trigramFreqTerms,]))
trigramFreq <- data.frame(word=names(trigramFreq),frequency=trigramFreq)
trigramFreq_sort<- trigramFreq[order(trigramFreq$frequency),]

trigramTable <- separate(trigramFreq_sort, col=word, into=c('word1', 'word2', 'word3'), sep = " ")

print(dim(trigramTable))
print(tail(trigramTable))

end_time_trigrams <- Sys.time()
print(paste("trigrams execution time >> ", end_time_trigrams - start_time_trigrams))

saveRDS(trigramTable, "./ngramsTables/trigramTable.rds")

