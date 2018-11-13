options(java.parameters = "-Xmx12288m")

library(tm)
library(RWeka)
library(stringr)
library(caret)


twitter <- readLines('data/en_US/en_US.twitter.txt')
news <- readLines('data/en_US/en_US.news.txt')
blogs <- readLines('data/en_US/en_US.blogs.txt')

doc <- c(twitter, news, blogs)

corp <- VCorpus(VectorSource(doc))

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
weirdChars<-function(x) gsub("œâ€™˜", "", x)
SampCrps<- tm_map(SampCrps, tt)

corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeNumbers)

dtm_2g <- readRDS('data/dtm_2g_large.data')
dtm_2g_sparse <- removeSparseTerms(dtm_2g,0.95)
saveRDS(dtm_2g_sparse, file = 'data/dtm_2g_sparse.data')

dtm_2g_sparse <- readRDS('data/dtm_2g_sparse.data')


am <- as.matrix(dtm_2g_sparse)

i <- 1
vals <- c(sum(am[1:3,1]))
for (i in c(2:length(dtm_2g_sparse$dimnames$Terms))) {
    vals <- c(vals,sum(am[1:3,i]))
}


vals <- c(sum(am[1:3,9000]))
for (i in c(9001:10000)) {
    vals <- c(vals,sum(am[1:3,i]))
}


am1 <- str_split(dtm_2g_sparse$dimnames$Terms[9000:10000], ' ', simplify = TRUE)
am2 <- cbind (am1,vals)
colnames(am2) <- c('prev_words','next_word','tfidf')

saveRDS(vals, file = 'data/vals.data')
saveRDS(dtm_2g_sparse$dimnames$Terms, file = 'data/terms.data')

vals <- readRDS('data/vals.data')
terms <- readRDS('data/terms.data')

terms[1:10]

