options(java.parameters = "-Xmx32000m")

library(tm)
library(NLP)
library(RWeka)
library(stringr)

dtm_2gTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm_3gTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm_4gTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm_5gTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
dtm_6gTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6))

setwd('~/pat/data-science-capstone')
file_src <- DirSource (directory = '~/pat/data-science-capstone/data/en_US/')

corp <- VCorpus(file_src,readerControl = list(
  reader = readPlain,
  encoding = "UTF-8",
  language = 'en_US',
  load = 'false',
  dbcontrol = list (
    useDb = TRUE,
    dbName = 'texts.db',
    dbType = 'DB1'
  )
))

# Clean the corpus of punctuation, white space and numbers (not used for text prediction)
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeNumbers)

dtm_2g <- DocumentTermMatrix(corp, control =list(
  tokenize = dtm_2gTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTfIdf(x, normalize = TRUE)
))


saveRDS(dtm_2g, file = 'data/dtm_2g_large.data')
dtm_2g <- removeSparseTerms(dtm_2g,0.95)
saveRDS(dtm_2g, file = 'data/dtm_2g_sparse.data')

dtm_3g <- DocumentTermMatrix(corp, control =list(
  tokenize = dtm_3gTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTf(x)
))
saveRDS(dtm_3g, file = 'data/dtm_3g_large.data')
dtm_3g <- removeSparseTerms(dtm_3g,0.2)
saveRDS(dtm_3g, file = 'data/dtm_3g_sparse.data')

dtm_4g <- DocumentTermMatrix(corp, control =list(
  tokenize = dtm_4gTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTf(x)
))
saveRDS(dtm_4g, file = 'data/dtm_4g_large.data')
dtm_4g <- removeSparseTerms(dtm_4g,0.2)
saveRDS(dtm_4g, file = 'data/dtm_4g_sparse.data')

dtm_5g <- DocumentTermMatrix(corp, control =list(
  tokenize = dtm_5gTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTf(x)
))
saveRDS(dtm_5g, file = 'data/dtm_5g_large.data')
dtm_5g <- removeSparseTerms(dtm_5g,0.2)
saveRDS(dtm_5g, file = 'data/dtm_5g_sparse.data')


dtm_6g <- DocumentTermMatrix(corp, control =list(
  tokenize = dtm_6gTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTf(x)
))
saveRDS(dtm_6g, file = 'data/dtm_6g_large.data')
dtm_6g <- removeSparseTerms(dtm_6g,0.2)
saveRDS(dtm_6g, file = 'data/dtm_6g_sparse.data')

