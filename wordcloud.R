library(readr)
library(tm)
library(wordcloud)
library(png)

cat("Read data ...\n")
reviews <- read_csv('E:/kiran/amazon-fine-foods/Reviews.csv')

# Many products have multiple productIds and their reviews have multiple copies
uniqReviews <- unique(reviews[, 3:10])

### define a function that takes a vector of texts and returns word frequency matrix
getWordFreq <- function(textVector) {
  # remove "not" from stopwords, as "not" is very important negative word
  stopWords <- stopwords("en")
  stopWords <- stopWords[stopWords != "not"]
  
  corpus = Corpus(VectorSource(textVector))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("amazon", stopWords))  
  
  dtm = DocumentTermMatrix(corpus)
  removeSparse <- removeSparseTerms(dtm, 0.997)
  return(as.data.frame(as.matrix(removeSparse)))
}

### define a function that shows the top 100 frequent words in clound
displayWordcloud <- function(wordFrequency) {
  op <- options("warn")
  on.exit(options(op))
  options(warn=1)
  wordcloud(colnames(wordFrequency), colSums(wordFrequency),
            max.words = 100, random.order = FALSE, scale = c(4, 1),
            rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}

### word frequencies of the column "summary" for score = 1, 2, 3, 4, 5
cat("Calculate word frequency of reviews with score = 1 ...\n")
wordFreq1 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==1])

cat("Calculate word frequency of reviews with score = 2 ...\n")
wordFreq2 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==2])

cat("Calculate word frequency of reviews with score = 3 ...\n")
wordFreq3 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==3])

cat("Calculate word frequency of reviews with score = 4 ...\n")
wordFreq4 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==4])

cat("Calculate word frequency of reviews with score = 5, be patient ...\n")
wordFreq5 <- getWordFreq(uniqReviews$Summary[uniqReviews$Score ==5])


### save plot as png file
cat("make plot ...\n")
png(file = "wordcloud_score.png", width = 1100, height = 270)

layout(matrix(1:5, nrow=1))

displayWordcloud(wordFreq1)
text(x = 0.5, y = 1.1, "Worst Review", cex = 2)
displayWordcloud(wordFreq2)
text(x = 0.5, y = 1.1, "Bad Review", cex = 2)
displayWordcloud(wordFreq3)
text(x = 0.5, y = 1.1, "Mediocre", cex = 2)
displayWordcloud(wordFreq4)
text(x = 0.5, y = 1.1, "Good Review", cex = 2)
displayWordcloud(wordFreq5)
text(x = 0.5, y = 1.1, "Best Review", cex = 2)

dev.off()


### clear memory
remove(list = ls())
gc()