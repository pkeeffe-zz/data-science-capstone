library(tm)
library(NLP)
library(RWeka)
library(stringr)
library(gpuR)

setwd('d:/dev/Coursera/Capstone/NLP Project/')
#file_src <- DirSource (directory = 'd:/dev/Coursera/Capstone/NLP Project/data/en_US/')
file_src <- DirSource (directory = 'd:/dev/Coursera/Capstone/NLP Project/data-min/')

# Generate the Corpus and associated document term matrix for everything up to 4-gram
FullTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))
corp <- VCorpus(file_src,readerControl = list(
  reader = readPlain,
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


bi_dtm <- DocumentTermMatrix(corp, control =list(
  tokenize = FullTokenizer,
  tolower = TRUE,
  wordLengths = c(1,Inf),
  weighting = function(x) weightTf(x)
))

# Clean up the term list from the doc term matrix and generate ngram vectors
term_vals <- trimws(bi_dtm$dimnames$Terms[grepl('^[A-Za-z]',bi_dtm$dimnames$Terms)])
ngram_1g <- term_vals[str_count(term_vals, "\\w+") == 1]
ngram_2g <- term_vals[str_count(term_vals, "\\w+") == 2]
ngram_3g <- term_vals[str_count(term_vals, "\\w+") == 3]
ngram_4g <- term_vals[str_count(term_vals, "\\w+") == 4]
prob_matrix_col_count <- length(ngram_1g)

# Generate a 2-Gram Probabililty Matrix
freq_prob_matrix_2g <- gpuMatrix(0,ncol=length(ngram_1g), nrow = length(ngram_1g))
colnames(freq_prob_matrix_2g) <- c(ngram_1g,ngram_1g)


dimnames(freq_prob_matrix_2g) <-
i_len <- length(ngram_1g)
i <- 1;
j <- 1;
for (i in c(1:i_len)) {
    for (j in c(1:prob_matrix_col_count)){
        cur_sentence <- trimws(paste (ngram_1g[i],ngram_1g[j]))
        if (cur_sentence %in% ngram_2g) {
           freq_prob_matrix_2g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(ngram_2g)
        }
    }
}
#Remove all Rows with all NAs (no 2 gram relationship between row and cols)
freq_prob_matrix_2g <- freq_prob_matrix_2g[!apply (freq_prob_matrix_2g,1,function(x) all(is.na(x))),
                                           !apply (freq_prob_matrix_2g,2,function(x) all(is.na(x)))]

# Generate a 3-Gram Probabililty Matrix
freq_prob_matrix_3g <- matrix(NA,ncol=length(ngram_1g), nrow = length(ngram_2g))
dimnames(freq_prob_matrix_3g) <- list(ngram_2g,ngram_1g)
i_len <- length(ngram_2g)
i <- 1;
j <- 1;
for (i in c(1:i_len)) {
    for (j in c(1:prob_matrix_col_count)){
        cur_sentence <- trimws(paste (ngram_2g[i],ngram_1g[j]))
        if (cur_sentence %in% ngram_3g) {
            freq_prob_matrix_3g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(ngram_3g)
        }
    }
}
freq_prob_matrix_3g <- freq_prob_matrix_3g[!apply (freq_prob_matrix_3g,1,function(x) all(is.na(x))),
                                           !apply (freq_prob_matrix_3g,2,function(x) all(is.na(x)))]


# Generate a 4-Gram Probabililty Matrix
freq_prob_matrix_4g <- matrix(NA,ncol=length(ngram_1g), nrow = length(ngram_3g))
dimnames(freq_prob_matrix_3g) <- list(ngram_3g,ngram_1g)
i_len <- length(ngram_3g)
i <- 1;
j <- 1;
for (i in c(1:i_len)) {
    for (j in c(1:prob_matrix_col_count)){
        cur_sentence <- trimws(paste (ngram_3g[i],ngram_1g[j]))
        if (cur_sentence %in% ngram_4g) {
            freq_prob_matrix_3g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(ngram_4g)
        }
    }
}
freq_prob_matrix_4g <- freq_prob_matrix_4g[!apply (freq_prob_matrix_4g,1,function(x) all(is.na(x))),
                                           !apply (freq_prob_matrix_4g,2,function(x) all(is.na(x)))]



#write.csv2(freq_prob_matrix_1g,file='freq_prob_matrix_1g.csv')
freq_prob_matrix_1g[1,!is.na(freq_prob_matrix_1g_1[1,])]

