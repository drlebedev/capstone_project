START <- "_$START$_"
END <- "_$END$_"

setup <- function() {
  
  if (!require(quanteda)) {
    install.packages("quanteda")
    library(quanteda)
  }
  if (!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(data.table)) {
    install.packages("data.table")
    library(data.table)
  }
  
  set.seed(1234)
  
}