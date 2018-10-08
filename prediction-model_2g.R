
options(java.parameters = "-Xmx12288m")

library(tm)
library(RWeka)
library(stringr)
library(Matrix)
library(utf8)

setwd('d:/dev/Coursera/Capstone/NLP Project/')
file_src <- DirSource (directory = 'd:/dev/Coursera/Capstone/NLP Project/data/en_US/')
#file_src <- DirSource (directory = 'd:/dev/Coursera/Capstone/NLP Project/data-min/')

# Generate the Corpus and associated document term matrix for everything up to 4-gram
Tokenizer_2g <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
Tokenizer_3g <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
Tokenizer_4g <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
ngtokenizer <- function(x) ngram_asweka(x, min = 2, max = 2)

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

if (file.exists('/data/bi_dtm_sparse.data')) {
    bi_dtm <- readRDS('data/bi_dtm_sparse.data')
} else {
    bi_dtm <- DocumentTermMatrix(corp, control =list(
        tokenize = Tokenizer_2g,
        tolower = TRUE,
        weighting = function(x) weightTf(x)
    ))
    saveRDS(bi_dtm, file = 'data/bi_dtm_large.data')
    bi_dtm <- removeSparseTerms(bi_dtm,0.2)
    saveRDS(bi_dtm, file = 'data/bi_dtm_sparse.data')
}


if (file.exists('/data/tri_dtm_sparse.data')) {
    tri_dtm <- readRDS('data/tri_dtm_sparse.data')
} else {
    tri_dtm <- DocumentTermMatrix(corp, control =list(
        tokenize = Tokenizer_3g,
        tolower = TRUE,
        weighting = function(x) weightTf(x)
    ))
    saveRDS(tri_dtm, file = 'data/tri_dtm_large.data')
    tri_dtm <- removeSparseTerms(tri_dtm,0.2)
    saveRDS(tri_dtm, file = 'data/tri_dtm_sparse.data')
}


if (file.exists('/data/quad_dtm_sparse.data')) {
    quad_dtm <- loadRDS('data/quad_dtm_sparse.data')
} else {
    quad_dtm <- DocumentTermMatrix(corp, control =list(
        tokenize = Tokenizer_4g,
        tolower = TRUE,
        weighting = function(x) weightTf(x)
    ))
    saveRDS(quad_dtm, file = 'data/quad_dtm_large.data')
    quad_dtm <- removeSparseTerms(quad_dtm,0.2)
    saveRDS(quad_dtm, file = 'data/quad_dtm_sparse.data')
}

# Clean up the term list from the doc term matrix and generate ngram vectors
term_vals_2g <- trimws(bi_dtm$dimnames$Terms[grepl('^[A-Za-z]',bi_dtm$dimnames$Terms)])
ngram_1g <- term_vals[str_count(term_vals, "\\w+") == 1]
ngram_2g <- term_vals[str_count(term_vals, "\\w+") == 2]
str_replace_all(ngram_2g, "[œâ€™]", "")
str_replace_all(ngram_2g, "[œâ€™]", "")

term_vals_3g <- trimws(tri_dtm$dimnames$Terms[grepl('^[A-Za-z] [A-Za-z]',tri_dtm$dimnames$Terms)])
ngram_3g <- term_vals[str_count(term_vals, "\\w+") == 3]
str_replace_all(ngram_3g, "[œâ€™]", "")


# Handle prediction
testString <- 'absolutely'

if (str_length(testString) == 1) {
    target_matrix <- ngram_2g
} else if (str_length(testString == 2)) {
    
} else if (str_length(testString == 3)) {
    
} else {
    
}

grepString <- paste('^',testString,' ', sep = '')
options <- colSums(inspect(bi_dtm[,grep(grepString, ngram_2g, value=T)]))
recommendations <- sort(options, decreasing = TRUE)
names(recommendations[1:3])


prob_matrix_col_count <- length(ngram_1g)


# Generate a 2-Gram Probabililty Matrix
freq_prob_matrix_2g <- Matrix(0,ncol=length(ngram_1g), nrow = length(ngram_1g), sparse = TRUE)
#freq_prob_matrix_2g <- matrix(0,ncol=length(ngram_1g), nrow = length(ngram_1g))

dimnames(freq_prob_matrix_2g) <- list(ngram_1g,ngram_1g)
i_len <- length(ngram_1g)
i <- 1;
j <- 1;
for (i in c(1:i_len)) {
    for (j in c(1:prob_matrix_col_count)){
        cur_sentence <- trimws(paste (ngram_1g[i],ngram_1g[j]))
        if (cur_sentence %in% ngram_2g) {
            freq_prob_matrix_2g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(ngram_2g)
            print(cur_sentence) 
        }
    }
}

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

