# Data Science Specialization - SwiftKey Capstone

# Overview
  # The goal of this capstone is to mimic the experience of being a 
  # data scientist, exploring an area called Natural Language Processing - NLP.
  # The idea is to apply the acquired knowledge and build a solution that, 
  # given a word initially typed, can return some terms and words that can be used / completed, in order to try to predict which word will be typed.

# Pre requirements
  # In order to be able to work with data and analysis, two extremely useful packages for NLP will be used:
  # - TM - For NLP
  # - RWeka - For Tokenization
  
  library(tm)
  library(RWeka)
  library(tidyverse)
  library(stylo)
  library(tokenizers)
  library(stringr)
  
  set.seed(1906)

  #print("How many word to cover 50% of data - one-part-words:")
  #wordCoverage(grams1df, 0.5)
  #print("How many word to cover 90% of data - two-word-words:")
  #wordCoverage(grams1df, 0.9)
  
  #save(grams1df, file="./Scripts/ProjectCapstone/AppCapstone/grams1df.RData")
  #save(grams2df, file="./Scripts/ProjectCapstone/AppCapstone/grams2df.RData")
  #save(grams3df, file="./Scripts/ProjectCapstone/AppCapstone/grams3df.RData")
  #save(grams4df, file="./Scripts/ProjectCapstone/AppCapstone/grams4df.RData")
  load(file="./grams1df.RData")
  load(file="./grams2df.RData")
  load(file="./grams3df.RData")
  load(file="./grams4df.RData")
  
# Building predicting model
  
  #The prediction model will use the preprocessed 2, 3 and 4 gram models to search for the typed words and return the most frequent ones for the suggested completion.
  #They will be created in function format for easier use in the future application.  
  
  grams2df <- separate(grams2df, 1, c("word1", "word2"), sep=" ")
  grams3df <- separate(grams3df, 1, c("word1", "word2", "word3"), sep=" ")
  grams4df <- separate(grams4df, 1, c("word1", "word2", "word3", "word4"), sep=" ")
  
  
  #Predict next word Function takes in the input variable from user and predicts the next word
  predictNextWord <- function(input) {
    #Cleaning the input
    wordInput <- extractWordCleaned(input)

    #Getting the number of words in the input
    wordCount <- length(wordInput)
    
    #Initializing response
    prediction <- c()
    
    #Trimming input to the last three words
    if(wordCount > 3) {
      wordInput <- wordInput[(wordCount-2):wordCount]
      prediction <- matchinFourGranm(wordInput[1],wordInput[2],wordInput[3])
    }
    
    #Four Gram Match
    if(wordCount == 3) {
      prediction <- matchinFourGranm(wordInput[1],wordInput[2],wordInput[3])
    }
    
    #Three Gram Match
    if(wordCount == 2) {
      prediction <- matchThreeGram(wordInput[1],wordInput[2])
    }

    #Two gram match
    if(wordCount == 1) {
      prediction <- matchTwoGram(wordInput[1])
    }
    
    #No word entered
    if(wordCount == 0) {
      prediction <- "No available word entered."
      return(prediction)
    }
    
    #Unknown words
    if(length(prediction)==0) {
      prediction <- "Next word not know from out database."
      return(prediction)
    }
    
    prediction[1:3]

  }

  
  #Extract words from sentence inputed and clean for punctuation and numbers
  extractWordCleaned <- function(text){
    textInput <- tolower(text)
    textInput <- removePunctuation(textInput)
    textInput <- removeNumbers(textInput)
    textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
    textInput <- stripWhitespace(textInput)
    textInput <- txt.to.words.ext(textInput, corpus.lang="English.all", preserve.case = TRUE)
    return(textInput)
  }
  
  
  #Match string in Four Gram and get probable word
  matchinFourGranm <- function (inputWord1,inputWord2,inputWord3) {
    predictWord <- filter(grams4df,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
    if(length(predictWord) == 0){
      predictWord <- filter(grams4df,(word2 == inputWord2 & word3 == inputWord3))$word4
      if(length(predictWord) == 0){
        predictWord <- filter(grams4df,(word1 == inputWord2 & word2 == inputWord3))$word3
        if(length(predictWord) ==0){
          predictWord <- matchThreeGram(inputWord2,inputWord3)
        }
      }
    }
    predictWord
  }
  
  
  
  #Match string in Three Gram and get probable word
  matchThreeGram <- function(inputWord1,inputWord2){
    predictWord <- filter(grams3df,(word1 == inputWord1 & word2 == inputWord2))$word3
    if(length(predictWord)==0){
      predictWord <- filter(grams3df,(word2 == inputWord2))$word3 
      if(length(predictWord)== 0){
        predictWord <- filter(grams3df,(word1 == inputWord2))$word2 
        if(length(predictWord) ==0 ){
          predictWord <- matchTwoGram(inputWord2)
        }
      }
    }
    predictWord
  }
  
  #Match string in Two Gram and get probable word
  matchTwoGram <- function(inputWord1) {
    predictWord <- filter(grams2df,(word1 == inputWord1))$word2
    predictWord
    
  }
  
  #Find presence of word in database
  wordPresence <- function(input) {
    
    wordInput <- extractWordCleaned(input)
    inputCount <- length(wordInput)
    presence <- ""
    
    if (inputCount == 0){
      return (presence)
    } else if (inputCount == 1){
      freq <- sum(filter(grams1df,(Var1 == wordInput[1]))$Freq)
      presence$freq <- freq
      presence$percent <- freq/sum(grams1df$Freq)*100
    } else if (inputCount == 2){
      freq <- sum(filter(grams2df,(word1 == wordInput[1] | word2 == wordInput[2]))$Freq)
      presence$freq <- freq
      presence$percent <- freq/sum(grams2df$Freq)*100
    } else if (inputCount == 3){
      freq <- sum(filter(grams3df,(word1 == wordInput[1] | word2 == wordInput[2] | word3 == wordInput[3]))$Freq)
      presence$freq <- freq
      presence$percent <- freq/sum(grams3df$Freq)*100
    } else {
      freq <- sum(filter(grams3df,(word1 == wordInput[inputCount-2] | word2 == wordInput[inputCount-1] | word3 == wordInput[inputCount-1]))$Freq)
      presence$freq <- freq
      presence$percent <- freq/sum(grams3df$Freq)*100
    }
    
    return (presence)
  }
  
  

  
  
  