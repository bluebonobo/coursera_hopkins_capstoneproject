# Packages
library(knitr)       # used to make kable tables
library(tm)          # text mining package
library(ggplot2)     # used for plots
library(dplyr)       # Manipulate data frames
library(sentimentr)    # Remove Profanity
library(quanteda)      # Text Analysis and Corpus Managment leading library
library(LaF)           # need to install Large Ascii Files library install.packages('LaF') to sample source files
library(wordcloud)    #Wordcloud graph
# library(SnowballC)   # applies Porter's stemmming algorithm (discussed later) 
# library(magrittr)    # allows pipe operator
# library(tidytext)    # tidy text for plots


# Setting up the working directory same as this source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
print(getwd())

# SAMPLING TO DO >> Sampling Remember your inference class and how a representative sample can be used to infer facts about a population. 
# You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. 
# That way, you can store the sample and not have to recreate it every time. 
# You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.

# SAMPLING : RUN but crashes after generating the files. Output files are located in /sample
# sub sample dataset : create sample files which contain sample_percentge of original files
# sample_percentage = 0.001
# lisfil <- list.files("./dataset/final/en_US") #Replace E:\\Data with your directory name
# for (i in 1:length(list.files("./dataset/final/en_US"))) {
#   print(lisfil[i])
#   total_nblines <- determine_nlines(paste("./dataset/final/en_US/",lisfil[i], sep=""))
#   print(paste(paste("./dataset/final/en_US/",lisfil[i], sep=""), total_nblines, sep = ":"))
#   sample <- sample_lines(paste("./dataset/final/en_US/",lisfil[i], sep=""), n=round(sample_percentage*total_nblines), nlines = total_nblines)
#   # output_file <- file(paste("./dataset/final/en_US/sample/sample_",lisfil[i], sep=""), "wb")
#   output_file <- file("./dataset/final/en_US/sample/sample_allsources.txt", "ab")
#   writeBin( paste(sample, collapse = "\n"), output_file )
#   # close(output_file)
# }
# close(output_file)


# # Getting the data from SAMPLE
# working_english_corpus  <- VCorpus(DirSource("./dataset/final/en_US/sample/all"), readerControl = list(language="lat")) 

# Getting the data from 3 separate files
working_english_corpus  <- VCorpus(DirSource("./dataset/final/en_US/sample"), readerControl = list(language="lat")) 

# Checking few records
# First 2 records in the english blog file
working_english_corpus[[1]][1]$content[1]
working_english_corpus[[1]][1]$content[2]


# how many lines of text in twitter en_US file
print(paste("number lines in twitter file", lapply(working_english_corpus[[1]][1],length), sep = ":"))


# Cleaning the Corpus
print ("========= Cleaning the Corpus")
# we follow the following doc re. preprocessing https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# eliminate extra whitespaces
working_english_corpus <- tm_map(working_english_corpus, stripWhitespace)
# convert to lower case
working_english_corpus <- tm_map(working_english_corpus, content_transformer(tolower))
# ?? remove stop words - Aren't stopwords important for this project?
working_english_corpus <- tm_map(working_english_corpus, removeWords, stopwords("english"))
# remove punctuation
working_english_corpus <- tm_map(working_english_corpus, removePunctuation)
# remove numbers
working_english_corpus <- tm_map(working_english_corpus, content_transformer(removeNumbers))
# stemming
working_english_corpus <- tm_map(working_english_corpus, content_transformer(stemDocument), language = "english")

#Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input
# and returns a tokenized version of it.

#Profanity Filtering - removing profanity and other words you do not want to predict.
# based on https://rdrr.io/cran/sentimentr/man/extract_profanity_terms.html

print ("========= Profanity")

# prof_words <- extract_profanity_terms(working_english_corpus[[3]][1]$content, profanity_list = unique(tolower(lexicon::profanity_alvarez)),)
# print(prof_words)

#corpus <- tm_map(corpus, removeWords, profanity_vector)
# working_english_corpus <-tm_map(working_english_corpus, removeWords, profanity_list = unique(tolower(lexicon::profanity_arr_bad)))
#also use lexicon::profanity_arr_bad, lexicon::profanity_alvarez, lexicon::profanity_banned, 
# lexicon::profanity_racist, lexicon::profanity_zac_anger
# profanity_list <- c(lexicon::profanity_arr_bad, lexicon::profanity_alvarez, lexicon::profanity_banned,
# lexicon::profanity_racist, lexicon::profanity_zac_anger)

# prof_words$sentence
# prof_words$neutral
# prof_words$profanity
# attributes(extract_profanity_terms(working_english_corpus[[3]][1]$content))$counts
# attributes(extract_profanity_terms(working_english_corpus[[3]][1]$content))$elements

# print(working_english_corpus)

print(working_english_corpus[[1]][1])

# tokenized_working_english_corpus <- tokens(working_english_corpus, what="word", 
#        remove_punct = TRUE,
#        remove_symbols = TRUE,
#        remove_numbers = TRUE,
#        remove_url = TRUE,
#        remove_separators = TRUE,
#        split_hyphens = TRUE)

tokenized_working_english_corpus <- tokenize_word(working_english_corpus)

print(tokenized_working_english_corpus[[1]])

print ("========= Term Document Matrix")

# Term document matrix is also a method for representing the text data. In this method, the text data is 
# represented in the form of a matrix. The rows of the matrix represent the sentences from the data which needs to be analyzed 
# and the columns of the matrix represent the word.
term <- DocumentTermMatrix(working_english_corpus)
print(term)
# <<DocumentTermMatrix (documents: 3, terms: 2212816)>>
# Non-/sparse entries: 2824796/3813652
# Sparsity           : 57%
# Maximal term length: 487
# Weighting          : term frequency (tf)

print(dim(term))
# print(term$dimnames, term$dimnames)

# print ("========= High Frequency")
# # 
# print(TDMHighFreq <- findFreqTerms(term))

freq <- sort(colSums(as.matrix(term)), decreasing=TRUE)
print ("========= Most used")
print(head(freq,10))
print ("========= Least used")
print(tail(freq,10))

# findAssocs(term,term="said",0.7)

top20 <- head(freq,20)
bottom20 <-tail(freq,20)

# freq is a named numeric vector type in R : need to read about this !!
# plot(x = head(freq,10), y = names(head(freq,10)))
# plot(x = freq, y = names(freq))
barplot(top20, main="top20 words frequency", xlab = "Names", ylab="Frequency", horiz = TRUE)


# wordcloud
wordcloud(names(freq), freq, max.words=100, random.order=FALSE, colors=brewer.pal(8, "Accent"), scale=c(7,.4), rot.per=0)

# counts <- colSums(as.matrix(term))
# sorted_word_occurence <- sort(counts, decreasing = TRUE)
# print(head(sorted_word_occurence, 10))
# print(typeof(sorted_word_occurence))
# dfsorted <- data.frame(sorted_word_occurence)
# dim(dfsorted)