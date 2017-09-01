setwd("Documents/Kaggle/Voda/")

library(tm)
library(wordcloud)
library(plyr)
library(ggplot2)
library(RTextTools)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tibble)
library(readr)
library(magrittr)
library(tidyr)



##########################################################
sample_text <- read_lines("training_text")



myCorpus = Corpus(VectorSource(sample_text))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
##########################################################



s_text <- read_lines("Documents/Kaggle/Personalized_Medicine/test_variants")
myCorpus <- Corpus(VectorSource(s_text))
myCorpus <- tm_map(myCorpus,tolower)
myCorpus <- tm_map(myCorpus,removePunctuation)

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
s_matrix <- as.matrix(myDTM)
s_vec <- sort(rowSums(s_matrix),decreasing = T)

head(s_vec)
View(s_vec)
wordcloud(names(s_vec),s_vec,min.freq = 5,max.words = 30)


wordcloud(myCorpus, max.words =20,min.freq=1,scale=c(3,2), 
          random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())

##########################################################


s_test <- read_lines("Documents/Kaggle/Personalized_Medicine/test_text")
s_Corpus <- Corpus(VectorSource(s_test))
s_Corpus <- tm_map(s_Corpus,tolower)
s_Corpus <- tm_map(s_Corpus,removePunctuation)
wordcloud(s_Corpus,max.words = 10, min.freq = 30,scale = c(3,2),random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
#####################################################################################################
