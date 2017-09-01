install.packages("bigmemory")
install.packages("ff")

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
library(ff)
library(wordcloud)


getwd()
setwd("Documents/Kaggle/Voda/")

sample_1 <- read_lines("sample.txt")
sample_1

myCorpus = Corpus(VectorSource(sample_1))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)
v = sort(rowSums(m), decreasing = TRUE)


wordcloud(names(v),v,min.freq = 20,max.words = 10,colors = "Blue")

