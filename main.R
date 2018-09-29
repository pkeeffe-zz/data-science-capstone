library(corpus)
library(caret)
library(gsubfn)
library(stopwords)
library(ngram)

setwd('~/data-science-capstone/')

twitter <- readLines('data/en_US/en_US.twitter.txt')
news <- readLines('data/en_US/en_US.news.txt')
blogs <- readLines('data/en_US/en_US.blogs.txt')

blogs.3gram <- term_stats(blogs,ngrams=3,filter = text_filter(drop_punct=TRUE))

blogs.sentences <- text_split(blogs,units = "sentence")

ng <- ngram(blogs,n = 2, sep = " ")

blogs.ts.3gram <- term_stats(blogs.sentences,ngrams=3,filter = text_filter(drop_punct=TRUE))
blogs.ts.2gram <- term_stats(blogs.sentences,ngrams=3,filter = text_filter(drop_punct=TRUE))
blogs.ts.1gram <- term_stats(blogs.sentences,ngrams=3,filter = text_filter(drop_punct=TRUE))

term_matrix(blogs.sentences,ngrams=1:3,filter = text_filter(drop_punct=TRUE))


news.words <- text_split(news,units = 'token')
term_stats(news,ngrams=1,filter = text_filter(drop_punct=TRUE))

blogs.sentences.freq <- text_ntoken(blogs.sentences)
blogs.sentences.freq <- blogs.sentences.freq[!(blogs.sentences.freq>100)]  
blogs.sentences.freq <- blogs.sentences.freq[!(blogs.sentences.freq1<3)]  
gdf <- as.data.frame(blogs.sentences.freq1)
g <- ggplot (gdf,aes(x=gdf$blogs.sentences.freq1)) + 
    geom_histogram(aes(y=..density..), binwidth = 0.5) +
    geom_density(alpha = .2, fill = '#FF6666') +
    labs(x='Sentence Length',y='Dist. Density') +
    ggtitle('Distribution of Blog Sentence Length')
g

plot(d)
max(blogs.sentences.freq)
sum(blogs.sentences.freq1>100)




head(blogs.sentences.freq1)

sum(text_nsentence(blogs.sentences))


text <- c("I saw Mr. Jones today.", 
          "Split across\na line.",
          "What. Are. You. Doing????",
          "She asked 'do you really mean that?' and I said 'yes.'")

# split text into sentences
text_sen <- text_split(text, units = "sentences")
text.sentences.freq <- text_ntoken(text_sen)
  
# get the number of sentences
text_sen <- text_nsentence(text)





blogs_cp <- scan(file='data/en_US/en_US.blogs.txt',sep='\n',what='char',quote='')
blogs_cp <- tolower(blogs_cp)
words.list <- strsplit(blogs_cp,'\\W+')
words.vector <- unlist(words.list)

#Create a stop list of uninteresting words
stop.list <- c('the','a','of','and','is','be','are')
words.vector2 <- words.vector[!words.vector%in%stop.list]
freq.list <- table(words.vector2)
sorted.freq.list <- sort(freq.list,decreasing = T)
word.df <- as.data.frame(sorted.freq.list[1:20])
require(scales)
g <- ggplot (word.df, aes(x=words.vector2,y=Freq)) + 
  geom_bar(stat='identity') + 
  scale_y_continuous(labels = comma)
g


news_cp <- scan(file='data/en_US/en_US.news.txt',sep='.',what='char',quote='')

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
