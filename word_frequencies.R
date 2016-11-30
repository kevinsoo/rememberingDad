######################################################################
#### Calculate and visualize word-frequencies in Soo Ewe Jin's writing
#### Author: Kevin Soo
######################################################################

# load packages
library(tidyverse)
library(ggthemes)
library(tm)
library(SnowballC)
library(wordcloud)

# load data
load(file="ejStar.Rda")

######### pre-processing text

# create corpus
corpus <- Corpus(VectorSource(ej$text))

# preprocessing
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
# inspect(corpus)[[1]][1]

# remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
minorwords <- c("soo", "ewe", "jin", "executive", "editor", "â") # remove specific words here
corpus <- tm_map(corpus, removeWords, minorwords)

# remove stems and whitespace
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# save as text document
corpus <- tm_map(corpus, PlainTextDocument)

# create document term matrix
dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)

########## explore data

# word frequencies
freq <- colSums(as.matrix(dtm))   
freq[head(order(-freq), 20)] # 20 most frequent words
head(table(freq), 20) # frequencies of frequencies

# remove sparse words
dtms <- removeSparseTerms(dtm, 0.5) # 50% empty space, maximum
rownames(dtms) <- ej$title # document titles
inspect(dtms)

# into data frame
wf <- data.frame(word=names(freq), freq=freq) %>% arrange(-freq)
wf$rank <- 1:nrow(wf)
wf$word <- factor(wf$word, levels=wf[order(wf$rank),"word"])

# plot word frequencies
wf %>% filter(freq>300) %>%
    ggplot(aes(x=word, y=freq)) +
    geom_bar(stat="identity", aes(fill=freq)) +
    scale_color_ptol("cyl") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    xlab("Words") +
    ylab("Frequency") +
    labs(fill='Frequency') +
    ggtitle("Most commonly used words by Soo Ewe Jin")
    
# create wordclouds
wordcloud(names(freq), freq, min.freq=500) # only words that appear > 500 times
wordcloud(names(freq), freq, max.word=100, # top 100 words
          scale=c(5,.1), rot.per = .25,
          colors=brewer.pal(9, "PuBuGn")) # number of colors, palette
