library(corpus)
library(caret)

twitter <- readLines('data/en_US/en_US.twitter.txt')
news <- readLines('data/en_US/en_US.news.txt')
blogs <- readLines('data/en_US/en_US.blogs.txt')
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
