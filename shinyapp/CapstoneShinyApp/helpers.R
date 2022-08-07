library(tidyverse)
library(stringr)

# readRDS(bigramTable, "./ngramsTables/bigramTable.rds")
bigramTable <- readRDS("./ngramsTables/bigramTable.rds")
trigramTable <- readRDS("./ngramsTables/trigramTable.rds")
fourgramTable <- readRDS("./ngramsTables/fourgramTable.rds")

print(dim(bigramTable))

bigramWorking <- function(input_words){
  num <- length(input_words)

  # out <- as.character(slice_tail(filter(bigramTable, word1==input_words[num]), n=1))
  # out <- as.character(filter(bigramTable, word1==input_words[num])[1,2])

  filter(bigramTable, word1==input_words[num]) %>%
    # top_n(1, n) %>%
    slice_tail() %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

bigram <- function(input_words){
  num <- length(input_words)
  findword <- filter(slice_tail(filter(bigramTable, word1==input_words[num])), row_number() == 1L)
  findword <- select(findword, num_range("word", 2))
  out <- as.character(findword) 
  # if cannot find a match returns 'no match' 
  if (out=="character(0)") return("no match") else return(out)
}

trigram <- function(input_words){
  num <- length(input_words)
  findword <- filter(slice_tail(filter(trigramTable, word1==input_words[num])), row_number() == 1L)
  findword <- select(findword, num_range("word", 2))
  out <- as.character(findword) 
  # if trigram lookup doesn't find a match then calls bigram lookup function  - this is called backoff strategy
  if (out=="character(0)") return(bigram(input_words)) else return(out)
}

fourgram <- function(input_words){
  num <- length(input_words)
  findword <- filter(slice_tail(filter(fourgramTable, word1==input_words[num])), row_number() == 1L)
  findword <- select(findword, num_range("word", 2))
  out <- as.character(findword) 
  # if fourgram lookup doesn't find a match then calls trigram lookup function- this is called backoff strategy
  if (out=="character(0)") return(trigram(input_words)) else return(out)
}


# predictDummy <- function(input) {
# 
#     # predict something only if input numnber of words is 1 or more and return nothing otherwise
#     number_words_input <- length(strsplit(input, " ")[[1]])
#     if (number_words_input == 0) {
#       return("")
#     } else {
#       return(paste("prediction", "", number_words_input))
#     }
# }

predict <- function(input){
  # Create a dataframe
  # input <- data_frame(text = input)
  input <- tibble(text = input)
  # Clean the Inpput
  # replace_reg <- "[^[:alpha:][:space:]]*"
  # input <- input %>%
  #   mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  # input_words <- tolower(input_words)
  # Calls lookup function based on number of words entered. Calls fourgram for 3 words or more
  out <- ifelse(input_count==0,"",
                ifelse(input_count == 1, bigram(input_words), 
                  ifelse (input_count == 2, trigram(input_words), fourgram(input_words))))
    # Output
  return(out)
}
