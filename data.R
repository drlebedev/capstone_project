source("global.R")
setup()

load.corpus <- function(sampleSize = 0.1) {
  
  con <- file("./final/en_US/en_US.twitter.txt", "r") 
  tweetsVector <- readLines(con)
  close(con)
  con <- file("./final/en_US/en_US.news.txt", "r") 
  newsVector <- readLines(con)
  close(con)
  con <- file("./final/en_US/en_US.blogs.txt", "r") 
  blogsVector <- readLines(con)
  close(con)
  con <- file("./src/en_US.profanity.txt", "r") 
  profanityVector <- readLines(con)
  close(con)

  #Filter rude words  
  tweetsVector <- tweetsVector[sapply(tokenize(tweetsVector), function(l) {length(intersect(l, profanityVector)) > 0})]
  blogsVector <- blogsVector[sapply(tokenize(blogsVector), function(l) {length(intersect(l, profanityVector)) > 0})]
  newsVector <- newsVector[sapply(tokenize(newsVector), function(l) {length(intersect(l, profanityVector)) > 0})]
  
  tweetsVector <- paste(START, tweetsVector, END)
  blogsVector <- paste(START, blogsVector, END)
  newsVector <- paste(START, newsVector, END)
  
  twtCorpus <- corpus(tweetsVector)
  newsCorpus <- corpus(newsVector)
  blogsCorpus <- corpus(blogsVector)

  sampleTwtCorpus <- subset(twtCorpus, sample(c(TRUE,FALSE), ndoc(twtCorpus), replace = TRUE, prob = c(sampleSize, 1 - sampleSize)))
  sampleNewsCorpus <- subset(newsCorpus, sample(c(TRUE,FALSE), ndoc(newsCorpus), replace = TRUE, prob = c(sampleSize, 1 - sampleSize)))
  sampleBlogsCorpus <- subset(blogsCorpus, sample(c(TRUE,FALSE), ndoc(blogsCorpus), replace = TRUE, prob = c(sampleSize, 1 - sampleSize)))
  
  twtSampling <- sample(c(TRUE,FALSE), ndoc(sampleTwtCorpus), replace = TRUE, prob = c(9, 1))
  newsSampling <- sample(c(TRUE,FALSE), ndoc(sampleNewsCorpus), replace = TRUE, prob = c(9, 1))
  blogsSampling <- sample(c(TRUE,FALSE), ndoc(sampleBlogsCorpus), replace = TRUE, prob = c(9, 1))
  
  list(tweets = list(
    corpus = twtCorpus,
    training = subset(sampleTwtCorpus, twtSampling),
    testing = subset(sampleTwtCorpus, !twtSampling)
  ), news = list(
    corpus = newsCorpus,
    training = subset(sampleNewsCorpus, newsSampling),
    testing = subset(sampleNewsCorpus, !newsSampling)
  ), blogs = list(
    corpus = blogsCorpus,
    training = subset(sampleBlogsCorpus, blogsSampling),
    testing = subset(sampleBlogsCorpus, !blogsSampling)
  ))
    
}
