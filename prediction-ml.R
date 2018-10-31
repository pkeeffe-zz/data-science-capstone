
dtm_2g <- readRDS('data/dtm_2g_large.data')
dtm_2g_sparse <- removeSparseTerms(dtm_2g,0.4)
saveRDS(dtm_2g_sparse, file = 'data/dtm_2g_sparse.data')
saveRDS(vals, file = 'data/vals.data')
saveRDS(dtm_2g_sparse$dimnames$Terms, file = 'data/terms.data')

am <- as.matrix(dtm_2g_sparse)

a <- dtm_2g_sparse$dimnames$Terms
c <- colSums(inspect(dtm_2g_sparse[1:3,]))

i <- 1
for (i in c(1:length(dtm_2g_sparse$dimnames$Terms))) {
    vals <- c(vals,sum(am[1:3,i]))
}