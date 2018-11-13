options(java.parameters = "-Xmx12288m")

library(tm)
library(RWeka)
library(stringr)

#setwd('~/data-science-capstone/')
setwd('d:/dev/Coursera/Capstone/NLP Project/')

dtm_2g <- readRDS('data/dtm_2g_sparse.data')
dtm_3g <- readRDS('data/dtm_3g_sparse.data')
dtm_4g <- readRDS('data/dtm_4g_sparse.data')
dtm_5g <- readRDS('data/dtm_5g_sparse.data')
dtm_6g <- readRDS('data/dtm_6g_sparse.data')

dtm_2g <- readRDS('data/dtm_2g_large.data')
dtm_3g <- readRDS('data/dtm_3g_large.data')
dtm_4g <- readRDS('data/dtm_4g_large.data')
dtm_5g <- readRDS('data/dtm_5g_large.data')
dtm_6g <- readRDS('data/dtm_6g_large.data')

pred <- function(inputstring) {
  
  
  inputstring_vector <- str_split(inputstring, ' ')[[1]]
  if (str_count(inputstring,'\\w+') > 5) {
    endpos <-  length(inputstring_vector) 
    startpos <- endpos - 4
    inputstring <- paste(inputstring_vector[startpos:endpos],collapse = ' ')
  }
  
  grepString <- paste('^',inputstring,' ', sep = '')
  if (str_count(inputstring,'\\w+') == 1) {
    options <- sort(colSums(inspect(dtm_2g[,grep(grepString, dtm_2g$dimnames$Terms, value=T)])),decreasing=TRUE)
  } else if (str_count(inputstring,'\\w+') == 2) {
    options <- sort(colSums(inspect(dtm_3g[,grep(grepString, dtm_3g$dimnames$Terms, value=T)])),decreasing = TRUE)
  } else if (str_count(inputstring,'\\w+') ==3) {
    options <- sort(colSums(inspect(dtm_4g[,grep(grepString, dtm_4g$dimnames$Terms, value=T)])), decreasing = TRUE)
  } else if (str_count(inputstring,'\\w+') == 4) {
    options <- sort(colSums(inspect(dtm_5g[,grep(grepString, dtm_5g$dimnames$Terms, value=T)])),decreasing = TRUE)
  } else if (str_count(inputstring,'\\w+') == 5) {
    options <- sort(colSums(inspect(dtm_6g[,grep(grepString, dtm_6g$dimnames$Terms, value=T)])),decreasing = TRUE)
  }
  if (is.na(options[1]) && str_count(inputstring,'\\w+') > 1) {
    inputstring_vector <- str_split(inputstring, ' ')[[1]]
    pred(paste(inputstring_vector[2:length(inputstring_vector)], collapse = ' '))
  } else {
    return(names(options[1:3])) 
  }
}








test <- list(
  "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
  "You're the reason why I smile everyday. Can you follow me please it would mean the",
  "Hey sunshine, can you follow me and make me the",
  "Very early observations on the Bills game offense still struggling but the",
  "Go on a romantic date at the",
  "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
  "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
  "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
  "Be grateful for the good times and keep the faith during the",
  "If this isn't the cutest thing you've ever seen, then you must be")

test <- lapply(test, tolower)
lapply(test, pred)

i <- 0
j <- 0

for (i in c(1:i_len)) {
  for (j in c(1:j_len)){
    cur_sentence <- trimws(paste (row_vals_1g[i],row_vals_1g[j]))
    if (cur_sentence %in% row_vals_3g) {
      freq_prob_matrix_1g[i,j] <- sum(bi_dtm[1:3,cur_sentence])/length(row_vals_2g)
    }
  }
}


  