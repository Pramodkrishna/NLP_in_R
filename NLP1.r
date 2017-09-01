

#Reference https://rpubs.com/lmullen/nlp-chapter
library(rJava)
install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)
library(tm)
library(wordcloud)
install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
                 repos=NULL, type="source")

#Basic Tokenization
bio <- readLines("Documents/Kaggle/Voda/Simulation_Games/sample.txt")


bio1 <- paste(bio, collapse = " ")
print(bio1)

"""
Sentence and Word Annotations
"""
bio1_string <- as.String(bio1)

"""
#Next we need to create annotators for words and sentences. 
#Annotators are created by functions which load the underlying Java libraries.

#These annotators form a “pipeline” for annotating the text in our bio variable
"""
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

bio_annotations <- annotate(bio1_string, list(sent_ann, word_ann))\
head(bio_annotations)

"""
We see that the annotation object contains a list of sentences (and also words) identified by position. 
That is, the first sentence in the document begins at character 1 and ends at character 111. 
The sentences also contain information about the positions of the words that comprise them.
"""

bio_doc <- AnnotatedPlainTextDocument(bio1_string, bio_annotations)

"""
We can combine the biography and the annotations to create what the NLP package calls an AnnotatedPlainTextDocument.
"""

class(bio_doc)

"""
#bio_doc$content
#bio_doc$meta
#bio_doc$annotations
"""

sents(bio_doc) %>% head(2)

"""
Sents give the sentences and 
Words give words

"""

words(bio_doc) %>% head(15)

#Annotating people and places

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)


bio_annotations2 <- annotate(bio1, pipeline)

bio_doc2 <- AnnotatedPlainTextDocument(bio1, bio_annotations2)


entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc2, kind = "organization")

