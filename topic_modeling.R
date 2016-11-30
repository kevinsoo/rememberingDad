##################################################
#### Topic modeling of Soo Ewe Jin's writing
#### Author: Kevin Soo
#### Run after preprocessing in word_frequencies.R
##################################################

# load packages
library(topicmodels)

# length should be total number of terms
length(freq)
# create sort order (descending)
ord <- order(freq, decreasing=TRUE)
# list all terms in decreasing order of freq
freq[ord]

# set parameters for gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# number of topics
k <- 3

# run LDA using gibbs sampling
ldaOut <- LDA(dtm, k, method="Gibbs", 
              control=list(nstart=nstart, seed=seed, best=best, burnin=burnin, iter=iter, thin=thin))

# top 10 terms in each topic
terms <- as.matrix(terms(ldaOut, 10))

# get topics of each document
topic <- as.matrix(topics(ldaOut))
topic <- topic[1:length(topic)]
article <- ej$title # document titles
date <- ej$date # dates
num <- 1:length(article) # number

# set up data frame and label topics
topics <- data.frame(num, date, article, topic)
topics$topic <- ifelse(topics$topic==1, "Encouragement",
                       ifelse(topics$topic==2, "Reflections",
                              ifelse(topics$topic==3, "Society", "")))
topics$topic <- as.factor(topics$topic)

# probabilities associated with each topic assignment
topicProb <- as.data.frame(ldaOut@gamma)
colnames(topicProb) <- c("Encouragement", "Reflections", "Society")

# compute relative importance of each topic
FirstToSecond <- rep(NA, nrow(topicProb))
FirstToThird <- rep(NA, nrow(topicProb))
SecondToThird <- rep(NA, nrow(topicProb))
for (i in 1:nrow(topicProb)) {
    tmp <- sort(topicProb[i,])
    FirstToSecond[i] <- as.numeric(tmp[3]/tmp[2])
    FirstToThird[i] <- as.numeric(tmp[3]/tmp[1])
    SecondToThird[i] <- as.numeric(tmp[2]/tmp[1])
}
topicProb <- data.frame(topicProb, FirstToSecond, FirstToThird, SecondToThird)

# save all topic probabilities
topics <- data.frame(topics, topicProb)
save(topics, file="topics.Rda")

# plot topics over time
topicsOverTime <- topics %>% gather(topics, p, Encouragement:Society) %>% arrange(num, topics)
topicsOverTime %>% filter() %>%
    ggplot(aes(x=num, y=p, group=topics)) + 
    geom_bar(stat="identity", aes(fill=topics)) +
    scale_fill_ptol("Topic") +
    theme_minimal() +
    xlab("Article #") +
    ylab("P(topic|article)")

# separate plots
topicsOverTime %>% filter() %>%
    ggplot(aes(x=date, y=p, group=topics)) + 
    facet_grid(~ topics) +
    geom_point(stat="identity", aes(color=topics), alpha=0.4) +
    stat_smooth(color="gray25", method=lm) +
    scale_color_ptol("Topic") +
    theme_minimal() +
    theme(legend.position="none") +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "6 months", date_labels = "%Y") +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
    xlab("Date") +
    ylab("P(topic|article)") +
    ggtitle("Trends in topics over time")