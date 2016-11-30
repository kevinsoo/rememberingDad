####################################################
#### Clustering words found in Soo Ewe Jin's writing
#### Author: Kevin Soo
####################################################

# load packages
library(reshape2)
library(cluster)
library(fpc)

# document term matrix from word_frequencies.R
dtms <- removeSparseTerms(dtm, 0.5) # 50% empty space, maximum
rownames(dtms) <- ej$title # document titles
inspect(dtms)

# create dendrogram
d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
plot(fit, hang=-1, main="Neighboring words", xlab="Words", ylab="", axes=F, sub="")
groups <- cutree(fit, k=3) # k clusters
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around clusters

# k-means clustering
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, 
         color=T, shade=F, labels=3, lines=0, span=F,
         main="Clusters of words in Soo Ewe Jin's writing")

# heat map
heatmap(
    as.matrix(d), Rowv=NA,
    Colv=as.dendrogram(hclust(dist(t(as.matrix(d)))))
)

# heatmap in ggplot
meltedWords <- data.frame(melt(as.matrix(d)))
colnames(meltedWords) <- c("i", "j", "Distance")
meltedWords$i <- factor(meltedWords$i, levels=meltedWords[order(wf$rank),"word"])

# plot word frequencies
meltedWords %>% arrange(-Distance) %>%
    ggplot(aes(x=i, y=j, fill=Distance)) +
    geom_raster() +
    scale_color_ptol("cyl") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    xlab("") +
    ylab("") +
    labs(fill='Distance') +
    ggtitle("Distance between frequently used words")
