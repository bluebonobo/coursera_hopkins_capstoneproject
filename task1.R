# Packages
# library(knitr)       # used to make kable tables
# library(harrypotter) # harry potter book series
library(tm)          # text mining package
# library(SnowballC)   # applies Porter's stemmming algorithm (discussed later) 
# library(magrittr)    # allows pipe operator
# library(tidytext)    # tidy text for plots
# library(ggplot2)     # used for plots
# library(dplyr)       # Manipulate data frames 
library(sentimentr)    # Remove Profanity
library(quanteda)      # Text Analysis and Corpus Managment leading library
library(LaF)           # need to install Large Ascii Files library install.packages('LaF') to sample source files

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
working_english_corpus  <- VCorpus(DirSource("./dataset/final/en_US/sample"), readerControl = list(language="lat")) 

# Checking few records
# First 2 records in the english blog file
working_english_corpus[[1]][1]$content[1]
working_english_corpus[[1]][1]$content[2]
# First 2 records in the english news file
working_english_corpus[[2]][1]$content[1]
working_english_corpus[[2]][1]$content[2]
# first 2 records in the english twitter file
working_english_corpus[[3]][1]$content[1]
working_english_corpus[[3]][1]$content[2]

# Quiz 1 =====================================================================================================================================

# how many lines of text in twitter en_US file
print(paste("number lines in twitter file", lapply(working_english_corpus[[3]][1],length), sep = ":"))
# working_english_corpus[[3]][2] is empty metadata
print(paste("number lines in blogs file", lapply(working_english_corpus[[1]][1],length), sep = ":"))
print(paste("number lines in news file", lapply(working_english_corpus[[2]][1],length), sep = ":"))

# Twitter line Max length
max(nchar(working_english_corpus[[3]][1]$content))
# [1] 140
# News line Max length
max(nchar(working_english_corpus[[2]][1]$content))
# [1] 11384
# Blogs line Max length
max(nchar(working_english_corpus[[1]][1]$content))
# [1] 40833

# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs
# by the number of lines the word "hate" (all lowercase) occurs, about what do you get?

sum(grepl("love",working_english_corpus[[3]][1]$content))/sum(grepl("hate",working_english_corpus[[3]][1]$content))
# [1] 4.108592

# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
working_english_corpus[[3]][1]$content[grep("biostats",working_english_corpus[[3]][1]$content)]
# [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

# How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (
# I.e. the line matches those characters exactly.)
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",working_english_corpus[[3]][1]$content))
# [1] 3

# End of Quiz 1 ===================================================================================================================

# Term document matrix is also a method for representing the text data. In this method, the text data is 
# represented in the form of a matrix. The rows of the matrix represent the sentences from the data which needs to be analyzed 
# and the columns of the matrix represent the word.
# term <- DocumentTermMatrix(working_english_corpus)
# term
# <<DocumentTermMatrix (documents: 3, terms: 2212816)>>
# Non-/sparse entries: 2824796/3813652
# Sparsity           : 57%
# Maximal term length: 487
# Weighting          : term frequency (tf)


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

print(working_english_corpus[[3]][1])

# tokenized_working_english_corpus <- tokens(working_english_corpus, what="word", 
#        remove_punct = TRUE,
#        remove_symbols = TRUE,
#        remove_numbers = TRUE,
#        remove_url = TRUE,
#        remove_separators = TRUE,
#        split_hyphens = TRUE)

tokenized_working_english_corpus <- tokenize_word(working_english_corpus)

print(tokenized_working_english_corpus[[3]])


