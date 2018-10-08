library(tm)
library(NLP)
library(RWeka)
library(stringr)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
FullTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))

#file_src <- DirSource (directory = '~/data-science-capstone/data/en_US/')
file_src <- DirSource (directory = 'd:/dev/Coursera/Capstone/NLP Project/data-min/')

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

corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,stripWhitespace)
corp <- tm_map(corp,removeNumbers)

#uni_dtm <- DocumentTermMatrix(corp, control =list(
#  tolower = TRUE
#))

bi_dtm <- DocumentTermMatrix(corp, control =list(
  tokenize = FullTokenizer,
  tolower = TRUE,
  weighting = function(x) weightTf(x)
))

row_vals <- trimws(bi_dtm$dimnames$Terms[grepl('^[A-Za-z]',bi_dtm$dimnames$Terms)])
#col_vals <-  trimws(uni_dtm$dimnames$Terms[grepl('^[A-Za-z]',uni_dtm$dimnames$Terms)])

row_vals_1g <- row_vals[str_count(row_vals, "\\w+") == 1]
row_vals_2g <- row_vals[str_count(row_vals, "\\w+") == 2]
row_vals_3g <- row_vals[str_count(row_vals, "\\w+") == 3]

freq_prob_matrix_3g <- matrix(NA,ncol=length(row_vals_1g), nrow = length(row_vals_2g))
dimnames(freq_prob_matrix_1g) <- list(row_vals_1g,row_vals_2g)

i_len <- length(row_vals_1g)
j_len <- length(row_vals_1g)   
i <- 1;
j <- 1;

for (i in c(1:i_len)) {
    for (j in c(1:j_len)){
        cur_sentence <- trimws(paste (row_vals_1g[i],row_vals_1g[j]))
        if (cur_sentence %in% row_vals_3g) {
            freq_prob_matrix_1g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(row_vals_2g)
        }
    }
}

write.csv2(freq_prob_matrix_1g,file='~/freq_prob_matrix_1g.csv')









freq_prob_matrix_2g <- matrix(NA,ncol=length(row_vals_1g), nrow = length(row_vals_2g))
dimnames(freq_prob_matrix_1g) <- list(row_vals_1g,row_vals_2g)

i_len <- length(row_vals_1g)
j_len <- length(row_vals_1g)   
i <- 1;
j <- 1;

for (i in c(1:i_len)) {
    for (j in c(1:j_len)){
        cur_sentence <- trimws(paste (row_vals_g[i],row_vals_1g[j]))
        if (cur_sentence %in% row_vals_2g) {
            freq_prob_matrix_1g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(row_vals_2g)
        }
    }
}

write.csv2(freq_prob_matrix_1g,file='~/freq_prob_matrix_1g.csv')






















t <- matrix(NA,ncol=length(col_vals), nrow = length(row_vals))

dimnames(t) <- list(row_vals,col_vals)

two_gram_count <- sum(str_count(rownames(t), "\\w+") == 2)
three_gram_count <- sum(str_count(rownames(t), "\\w+") == 3)
four_gram_count <- sum(str_count(rownames(t), "\\w+") == 4)



i_len <- length(rownames(t))
j_len <- length(colnames(t))   

for (i in c(1:i_len)) {
  for (j in c(1:j_len)){
    cur_sentence <- trimws(paste (rownames(t)[i],colnames(t)[j]))
    if (cur_sentence %in% row_vals) {
      if (str_count(cur_sentence, "\\w+") == 2) {
        t[i,j] <- sum(bi_dtm[1:3,cur_sentence])/two_gram_count
      } else if (str_count(cur_sentence, "\\w+") == 3) {
        t[i,j] <- sum(bi_dtm[1:3,cur_sentence])/three_gram_count
      } else {
        t[i,j] <- sum(bi_dtm[1:3,cur_sentence])/four_gram_count
      }
    }
  }
}



#Clean up the term document matrices
bi_dtmm <- as.matrix(bi_dtm)

#Calculate the probability across all 3 documents in the Corpus.
bi_dtmm <- cbind(bi_dtmm,rowSums(bi_dtmm)/nrow(bi_dtmm))
colnames(bi_dtmm)[4] <- 'Prob'

bigram_prob <- bi_dtmm[,'Prob']
ttrigram_prob <- tri_dtmm[,'Prob']

bigram_prob <- bigram_prob[str_count(names(bigram_prob), '\\w+') > 1]
trigram_prob <- trigram_prob[str_count(names(bigram_prob), '\\w+') > 2]
startstates <- unique(str_extract(names(bigram_prob), "([A-Za-z]+)"))


# Build a 3 dimensional matrix to represent probablility of each n-gram.
matrix_elements <- 

test <- "we"

if (str_count(test, '\\w+') == 1) {
  nextstates <- grep('^' + test + ' ',names(bigram_prob), value = T)
  recommended <- names(which(dtmm[nextstates,4] == max(dtmm[nextstates,4])))  
  } else if (str_count(test, '\\w+') == 2) {
  
}

nextstates <- grep('^we ',names(bigram_prob), value = T)
dtmm[nextstates,4]




corp_clean <- tm_map(corp,stripWhitespace)
corp_clean <- tm_map(corp,tolower)
