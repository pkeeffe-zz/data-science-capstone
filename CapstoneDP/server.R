#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(readtext)
library(stringr)

setwd('d:/dev/Coursera/Capstone/NLP Project')

if (file.exists('/data/co_loc_ng2.data')) { # If co-location matrices already exist load them.
    co_loc_ng2 <- readRDS('/data/co_loc_ng2.data')
    co_loc_ng3 <- readRDS('/data/co_loc_ng2.data')
    co_loc_ng4 <- readRDS('/data/co_loc_ng2.data')
    co_loc_ng5 <- readRDS('/data/co_loc_ng2.data')
    co_loc_ng6 <- readRDS('/data/co_loc_ng2.data')
} else { # If it is the first time running the server then the model files have to be generated.
    twitter <- readLines('data/en_US/en_US.twitter.txt')
    news <- readLines('data/en_US/en_US.news.txt')
    blogs <- readLines('data/en_US/en_US.blogs.txt')
    corp <- corpus(c(twitter,news,blogs))
    corp_sample <- corpus_sample(corp,size=1000000)
    co_loc_ng2 <- textstat_collocations(corp_sample, method = "lambda", size = 2, min_count = 5, 
                                        remove_numbers = TRUE, remove_symbols = TRUE)
    co_loc_ng3 <- textstat_collocations(corp_sample, method = "lambda", size = 3, min_count = 5,
                                        remove_numbers = TRUE, remove_symbols = TRUE)
    co_loc_ng4 <- textstat_collocations(corp_sample, method = "lambda", size = 4, min_count = 5,
                                        remove_numbers = TRUE, remove_symbols = TRUE)
    co_loc_ng5 <- textstat_collocations(corp_sample, method = "lambda", size = 5, min_count = 5,
                                        remove_numbers = TRUE, remove_symbols = TRUE)
    co_loc_ng6 <- textstat_collocations(corp_sample, method = "lambda", size = 6, min_count = 5,
                                        remove_numbers = TRUE, remove_symbols = TRUE)
    
    saveRDS(co_loc_ng2, file = 'data/co_loc_ng2.data')
    saveRDS(co_loc_ng3, file = 'data/co_loc_ng3.data')
    saveRDS(co_loc_ng4, file = 'data/co_loc_ng4.data')
    saveRDS(co_loc_ng5, file = 'data/co_loc_ng5.data')
    saveRDS(co_loc_ng6, file = 'data/co_loc_ng6.data')
}

pred <- function(inputstring) {
    
    inputstring_vector <- str_split(inputstring, ' ')[[1]]
    if (str_count(inputstring,'\\w+') > 5) {
        endpos <-  length(inputstring_vector) 
        startpos <- endpos - 4
        inputstring <- paste(inputstring_vector[startpos:endpos],collapse = ' ')
    }
    grepString <- paste('^',inputstring,' ', sep = '')
    if (str_count(inputstring,'\\w+') == 1) {
        options <- subset(co_loc_ng2,collocation %in% grep(grepString,co_loc_ng2$collocation,value=T))
    } else if (str_count(inputstring,'\\w+') == 2) {
        options <- subset(co_loc_ng3,collocation %in% grep(grepString,co_loc_ng3$collocation,value=T))
    } else if (str_count(inputstring,'\\w+') ==3) {
        options <- subset(co_loc_ng4,collocation %in% grep(grepString,co_loc_ng4$collocation,value=T))
    } else if (str_count(inputstring,'\\w+') == 4) {
        options <- subset(co_loc_ng5,collocation %in% grep(grepString,co_loc_ng5$collocation,value=T))
    } else if (str_count(inputstring,'\\w+') == 5) {
        options <- subset(co_loc_ng6,collocation %in% grep(grepString,co_loc_ng6$collocation,value=T))
    }
    if (nrow(options)==0 && str_count(inputstring,'\\w+') > 1) {
        inputstring_vector <- str_split(inputstring, ' ')[[1]]
        pred(paste(inputstring_vector[2:length(inputstring_vector)], collapse = ' '))
    } else {
        return(options[1:100,]) 
    }
}



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$predictedWord <- renderText({
      "Prediction goes here!"
  })
    
})
