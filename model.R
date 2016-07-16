source("global.R")
source("data.R")
setup()

build.ngrams <- function(corpus) {
  
  model <- list(
    ngram1 = colSums(sort(dfm(corpus$news$training + corpus$blogs$training + corpus$tweets$training, concatenator = " "))),
    ngram2 = colSums(sort(dfm(corpus$news$training + corpus$blogs$training + corpus$tweets$training, ngrams= 2, concatenator = " "))),
    ngram3 = colSums(sort(dfm(corpus$news$training + corpus$blogs$training + corpus$tweets$training, ngrams= 3, concatenator = " ")))
  )
  model

}


build.model <- function(ngrms) {
  
  model <- list(
    ngram1 = data.table(
      word = names(ngrms$ngram1),
      freq = ngrms$ngram1
    ),
    ngram2 = data.table(
      word = names(ngrms$ngram2),
      freq = ngrms$ngram2
    ),
    ngram3 = data.table(
      word = names(ngrms$ngram3),
      freq = ngrms$ngram3
    )
  )

  model$ngram2 <- model$ngram2 %>% rowwise() %>% mutate(
    words = str_split(word, " ")
  ) 
  
  model$ngram3 <- model$ngram3 %>% rowwise() %>% mutate(
    words = str_split(word, " ")
  ) 
  
  model$ngram1 <- model$ngram1 %>% mutate(
    start = word,
    end = word
  ) 

  model$ngram2 <- model$ngram2 %>% mutate(
    start = unlist(words)[1],
    end = unlist(words)[2]
  ) 
  
  model$ngram3 <- model$ngram3 %>% mutate(
    start = unlist(words)[1],
    mid = unlist(words)[2],
    pre = paste(unlist(words)[1], unlist(words)[2]), 
    post = paste(unlist(words)[2], unlist(words)[3]), 
    end = unlist(words)[3]
  ) 
  
  model$ngram1 <- data.table(model$ngram1)
  model$ngram2 <- data.table(model$ngram2)
  model$ngram3 <- data.table(model$ngram3)
  
  model
  
}


save.model <- function(model) {
  unlink("model", T, T)
  dir.create("model", showWarnings = FALSE)
  save(model, file = "model/model.RData")
}

build.smooth <- function(mdl) {
  
  addKNUnigram <- function(model, D1 = 0.75){
    print("Updating unigrams")
    
    bigrams <- nrow(model$ngram2)
    
    setkey(model$ngram2, end)
    CWordDot <- function(word) nrow(model$ngram2[word]) # how many bigram begins with word
    NWordDot <- vapply(model$ngram1$word, CWordDot, numeric(1))

    setkey(model$ngram2, start)
    CDotWord <- function(word) nrow(model$ngram2[word]) # how many bigram ends with word
    NDotWord <- vapply(model$ngram1$word, CDotWord, numeric(1))
    
    model$ngram1$Pcont <- NDotWord/ bigrams

    setkey(model$ngram3, mid)
    CDotWordDot <- function(word) nrow(model$ngram3[word])
    NDotWordDot <- vapply(model$ngram3$word, CDotWordDot, numeric(1))
    
    model$ngram1$lambda <- (D1 / NWordDot) * NDotWordDot

    model$ngram1$NDotWordDot <- NDotWordDot
    model$ngram1[is.nan(model$ngram1$lambda),"lambda"] <- 0
    
    setkey(model$ngram2, word)
    setkey(model$ngram3, word)
    
    model
  }
  
  addKNBigram <- function(model, D2 = 0.75){
    print("Updating bigrams")

    setkey(model$ngram3, post)
    CDotW1W2 <- function(w1w2) nrow(model$ngram3[w1w2])
    NDotW1W2 <- vapply(model$ngram2$word, CDotW1W2, numeric(1))
    
    setkey(model$ngram3, pre)
    CW1W2Dot <- function(w1w2) nrow(model$ngram3[w1w2])
    NW1W2Dot <- vapply(model$ngram2$word, CW1W2Dot, numeric(1))
    
    w2 <- model$ngram2$start

    setkey(model$ngram1, word)
    CWord <- function(word) (model$ngram1[word])
    PWord <- sapply(w2, CWord, simplify = "array")

    NDotWordDot <- unlist(PWord[,"NDotWordDot",])
    NlambdaW2   <- unlist(PWord[,"lambda",])
    NPcontW3    <- unlist(PWord[,"Pcont",])
    
    model$ngram2$lambda2 <- (D2 / model$ngram2$freq) * NW1W2Dot
    
    model$ngram2$Pcont2 <- pmax(NDotW1W2 - D2, 0) / NDotWordDot + NlambdaW2 * NPcontW3
    model$ngram2[is.nan(model$ngram2$Pcont2),"lambda2"] <- 0

    setkey(model$ngram3, word)

    model    
  }
  
  addKNTrigram <- function(model, D3 = 0.75){
    print("Updating trigrams")
    
    FcW1W2 <- function(w1w2) model$ngram2[w1w2]$freq
    NcW1W2 <- vapply(model$ngram3$pre, FcW1W2, numeric(1))
    
    FlambdaW1W2 <- function(w1w2) model$ngram2[w1w2]$lambda2
    NlambdaW1W2 <- vapply(model$ngram3$pre, FlambdaW1W2, numeric(1))
    
    FprobW2W3 <- function(w2w3) model$ngram2[w2w3]$Pcont2
    NprobW2W3 <- vapply(model$ngram3$post, FprobW2W3, numeric(1))
    
    MaxLikelTerm <- pmax(model$ngram3$freq - D3, 0) / NcW1W2
    
    model$ngram3$PKN <- MaxLikelTerm + NlambdaW1W2 * NprobW2W3
    
    model
  }
  
  mdl <- addKNUnigram(mdl)
  mdl <- addKNBigram(mdl)
  mdl <- addKNTrigram(mdl)

  save.model(mdl)
  
  mdl  
}

load.model <- function() {
  if (exists("model")) {
    model 
  } else {
    print("Loading model")
    if (!file.exists("model/model.RData")) {
      stop("Model not found")
    }
    load("model/model.RData")
    model <<- model
    model
  }
}

predict <- function(sentence, model = load.model()) {
  reverse <- rev(unlist(strsplit(paste(START, tolower(sentence)), split=" ")))
  if(length(reverse)==0){
    rc <- model$ngram1[order(-model$ngram1$Pcont),][1:5,]$word
    return(rc)
  }
  bigram <- paste(reverse[2], reverse[1])
  if(sum(model$ngram3$pre == bigram) != 0) {
    a <- model$ngram3[model$ngram3$pre == bigram, ]
    rc <- a[order(-a$PKN),][1:5,]$end
  } else if(sum(model$ngram2$start == reverse[1]) != 0) {
    a <- model$ngram2[model$ngram2$start == reverse[1],]
    rc <- a[order(-a$Pcont2),][1:5,]$end
  } else if(!is.na(reverse[2]) & sum(model$ngram3$start == reverse[2])) {
    a <- model$ngram3[model$ngram3$start == reverse[2],]
    rc  <- a[order(-a$freq),][1:5,]$end
  } else {
    rc <- model$ngram1[order(-model$ngram1$Pcont),][1:5,]$word
  }

  # default answer
  if(is.na(rc[1])) {
    rc <- model$ngram1[order(-model$ngram1$Pcont),][1:5,]$word
  } else if(is.na(rc[2])) {
    rc[2:5] <-  model$ngram1[order(-model$ngram1$Pcont),][1:4,]$word
  } else if(is.na(rc[3])) {
    rc[3:5] <-  model$ngram1[order(-model$ngram1$Pcont),][1:3,]$word
  } else if(is.na(rc[4])) {
    rc[4:5] <-  model$ngram1[order(-model$ngram1$Pcont),][1:2,]$word
  } else if(is.na(rc[5])) {
    rc[5] <-  model$ngram1[order(-model$ngram1$Pcont),][1,]$word
  }
  
  rc <- rc[rc != START & rc != END]
  return(rc)
}

