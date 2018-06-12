library(corpus)
library(caret)
library(gsubfn)
library(stopwords)

setwd('d:/dev/coursera/capstone/nlp project')

twitter <- readLines('data/en_US/en_US.twitter.txt')
news <- readLines('data/en_US/en_US.news.txt')
blogs <- readLines('data/en_US/en_US.blogs.txt')

blogs_cp <- scan(file='data/en_US/en_US.blogs.txt',sep='\n',what='char',quote='')
blogs_cp <- tolower(blogs_cp)
words.list <- strsplit(blogs_cp,'\\W+')
words.vector <- unlist(words.list)

#Create a stop list of uninteresting words
stop.list <- c('the','a','of','and','is','be','are')
words.vector2 <- words.vector[!words.vector%in%stop.list]
freq.list <- table(words.vector2)
sorted.freq.list <- sort(freq.list,decreasing = T)

#remove hapax legomena (words that occur only once in the corpora)
sorted.freq.list <- sorted.freq.list[sorted.freq.list>1] 

#Concat each token entry with associated frequency (tab seperated).
sorted.table <- paste(names(sorted.freq.list),sorted.freq.list,sep = '\t')

#Dump the table to a file
cat("Word\tFreq",sorted.table,file=choose.files(),sep='\n')

textfile <- scan(file='output/test.txt',sep='\n',what='char',quote='')

table(unlist(strapply(textfile,'\\w+',perl=T)))

head(sorted.freq.list)
max(nchar(blogs))


twitter <- trimws(twitter)

text_corp <- corpus_frame('twitter',text = twitter)

term_stats(text_corp)

text_ntoken(text_corp)

print(twitter_corp)

love_count <- grep('love',twitter,ignore.case = FALSE)
hate_count <- grep('hate',twitter,ignore.case = FALSE)

length(love_count)/length(hate_count)

bio <- grep('biostats',twitter,ignore.case = FALSE)
twitter[bio[1]]

exct <- grep('A computer once beat me at chess, but it was no match for me at kickboxing',twitter,ignore.case = FALSE)
