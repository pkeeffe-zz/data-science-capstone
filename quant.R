library(quanteda)
library(readtext)
library(caret)

df_text <- readtext("d:/dev/Coursera/Capstone/NLP Project/data/en_US/*")
setwd('d:/dev/Coursera/Capstone/NLP Project')
twitter <- readLines('data/en_US/en_US.twitter.txt')
news <- readLines('data/en_US/en_US.news.txt')
blogs <- readLines('data/en_US/en_US.blogs.txt')

corp <- corpus(c(twitter,news,blogs))

corp_sample <- corpus_sample(corp,size=60000)

my_dfm <- dfm(corp)

ngram_2 <- tokens(corp_sample, what='word', remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, ngrams = 2)
ngram_3 <- tokens(corp, what='word', remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, ngrams = 3)
ngram_4 <- tokens(corp, what='word', remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, ngrams = 4)
ngram_5 <- tokens(corp, what='word', remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, ngrams = 5)
ngram_6 <- tokens(corp, what='word', remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, ngrams = 6)


co_loc_ng2 <- textstat_collocations(corp_sample, method = "lambda", size = 2, min_count = 5, 
                                    remove_numbers = TRUE, remove_symbols = TRUE)

co_loc_ng3 <- textstat_collocations(corp_sample, method = "lambda", size = 3, min_count = 5,
                                    remove_numbers = TRUE, remove_symbols = TRUE)

co_loc_ng4 <- textstat_collocations(corp_sample, method = "lambda", size = 4, min_count = 5,
                                    remove_numbers = TRUE, remove_symbols = TRUE)

co_loc_ng5 <- textstat_collocations(corp_sample, method = "lambda", size = 5, min_count = 5,
                                    remove_numbers = TRUE, remove_symbols = TRUE)

co_loc_ng6 <- textstat_collocations(corp_sample, method = "lambda", size = 6, min_count = 5,
                                    remove_numbers = TRUE, remove_symbols = TRUE)

