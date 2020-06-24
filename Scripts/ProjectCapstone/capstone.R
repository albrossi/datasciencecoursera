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

# Data loading
  # The reference database used to learn about words and their uses was provided by SwiftKey, a company specialized in the subject.
  
  # Data download
  # Database URL: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
  
  # The zip file contains 4 folders, separared by language.
  # The instructions are to work only with English database. 
  dirsource <- "./Scripts/ProjectCapstone/final/en_US/"
  fileslist <- list.files(path=dirsource)
  l <- lapply(paste(dirsource, fileslist, sep="/"), function(f) {
    fsize <- file.info(f)[1]/1024/1024
    con <- file(f, open="r")
    lines <- readLines(con)
    nchars <- lapply(lines, nchar)
    maxchars <- which.max(nchars)
    nwords <- sum(sapply(strsplit(lines, "\\s+"), length))
    close(con)
    return(c(f, format(round(fsize, 2), nsmall=2), length(lines), maxchars, nwords))
  })

  filesummary <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
  colnames(filesummary) <- c("File", "Size(MB)", "Number lines", "Longest line", "Number Words")
  filesummary


# Pre-processing and Sampling
  
  # Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
  # Writing a function that takes a file as input and returns a tokenized version of it.
  
  tokenForFile <- function(filepath) {
    conn <- file(filepath, "rb")
    document <- readLines(conn, encoding = "UTF-8")
    strsplit(x = document, split = "\\s+")
    close(conn)
  }
  
  # You might want to create a separate sub-sample dataset by reading 
  # in a random subset of the original data and writing it out to a 
  # separate file. That way, you can store the sample and not have to 
  # recreate it every time. You can use the rbinom function 
  # to "flip a biased coin" to determine whether you sample a line of text or not.
  
  # Lets sample 2% of each file, and group than in a single object.
  
  # First file
  file_data1 <- file(paste(dirsource, fileslist, sep="/")[1], open="r")
  file_lines1 <- readLines(file_data1)
  close(file_data1)
  
  # Second file
  file_data2 <- file(paste(dirsource, fileslist, sep="/")[2], open="r")
  file_lines2 <- readLines(file_data2)
  close(file_data2)
  
  # Third file
  file_data3 <- file(paste(dirsource, fileslist, sep="/")[3], open="r")
  file_lines3 <- readLines(file_data3)
  close(file_data3)

  file_sample1 = sample(file_lines1, length(file_lines1)/50)
  file_sample2 = sample(file_lines2, length(file_lines2)/50)
  file_sample3 = sample(file_lines3, length(file_lines3)/50)
  
  sample <- c(file_sample1, file_sample2, file_sample3)
  
  rm(file_lines1, file_sample1,
     file_lines2, file_sample2,
     file_lines3, file_sample3)
  
  
# Data cleaning
  
  # Prepare Corpus
  sample <- VCorpus(VectorSource(sample))
  
  # Remove punctuation
  sample = tm_map(sample,removePunctuation)
  
  # Remove numbers from the data
  sample = tm_map(sample,removeNumbers)
  
  # Remove whitespace
  sample = tm_map(sample,stripWhitespace)

  # Remove black listed word (Profanity cleaning)
  conn <- file("./Scripts/ProjectCapstone/blacklist.txt", "rb")
  profanity <- readLines(conn, encoding = "UTF-8")
  close(conn)
  sample <- tm_map(sample, removeWords, profanity) 
  rm(profanity)
  
  
  sampledf <- data.frame(rawtext = sapply(sample, as.character), stringsAsFactors=FALSE)
  sampledf$textLines <- iconv(sampledf$rawtext, 'UTF-8', 'ASCII')
  sampledf$textLines <- tolower(sampledf$textLines)
  sampledf <- sampledf[!is.na(sampledf$textLines),]
  
  
  
# Data exploration
  # Exploratory analysis - perform a thorough exploratory analysis of the 
  # data, understanding the distribution of words and relationship between 
  # the words in the corpora.
 
  # Understand frequencies of words and word pairs - build figures and 
  # tables to understand variation in the frequencies of words and word 
  # pairs in the data.
  
  # Tokenization process: breaking the cleaned sample into works 
  # to better work with than.
  
  # Frequency - 1-part-word
  # Save part-word object in disk for cache -> better performance
 
  grams1 <- tokenize_ngrams(sampledf$textLines,n=1,n_min = 1)
  #save(grams1, file="./Scripts/ProjectCapstone/grams1.RData")
  #load(file = "./Scripts/ProjectCapstone/grams1.RData")
  
  grams1df <- data.frame(table(unlist(grams1)))
  grams1df <- grams1df[order(grams1df$Freq,decreasing = TRUE),]
  save(grams1df, file="./Scripts/ProjectCapstone/AppCapstone/grams1df.RData")
  #rm(grams1df)
  #load(file="./Scripts/ProjectCapstone/AppCapstone/grams1df.RData")
  grams1plot <- grams1df[1:20,]

  names(grams1plot) <- c("Grams","Freq")
  ggplot(grams1plot, aes(x=reorder(Grams, Freq), y=Freq)) +
    geom_bar(stat = "identity") +  coord_flip() +
    xlab("1-part-words") + ylab("Frequency") +
    labs(title = "Most frequent one-gram-word")
  
  # Frequency - 2-part-word
  # Save part-word object in disk for cache -> better performance
  grams2 <- tokenize_ngrams(sampledf$textLines,n=2,n_min = 2)
  
  #save(grams2, file="./Scripts/ProjectCapstone/grams2.RData")
  #load(file = "./Scripts/ProjectCapstone/grams2.RData")
  
  grams2df <- data.frame(table(unlist(grams2)))
  grams2df <- grams2df[order(grams2df$Freq,decreasing = TRUE),]
  save(grams2df, file="./Scripts/ProjectCapstone/AppCapstone/grams2df.RData")
  
  grams2dfplot <- grams2df[1:20,]
  
  names(grams2dfplot) <- c("Grams","Freq")
  ggplot(grams2dfplot, aes(x=reorder(Grams, Freq), y=Freq)) +
    geom_bar(stat = "identity") +  coord_flip() +
    xlab("2-part-words") + ylab("Frequency") +
    labs(title = "Most frequent two-gram-word")

  # Frequency - 3-part-word
  # Save part-word object in disk for cache -> better performance
  
  grams3 <- tokenize_ngrams(sampledf$textLines,n=3,n_min = 3)
  #save(grams3, file="./Scripts/ProjectCapstone/grams3.RData")
  #load(file = "./Scripts/ProjectCapstone/grams3.RData")
  
  grams3df <- data.frame(table(unlist(grams3)))
  grams3df <- grams3df[order(grams3df$Freq,decreasing = TRUE),]
  save(grams3df, file="./Scripts/ProjectCapstone/AppCapstone/grams3df.RData")
  
  
  grams3dfplot <- grams3df[1:20,]
  names(grams3dfplot) <- c("Grams","Freq")
  ggplot(grams3dfplot, aes(x=reorder(Grams, Freq), y=Freq)) +
    geom_bar(stat = "identity") +  coord_flip() +
    xlab("3-part-words") + ylab("Frequency") +
    labs(title = "Most frequent three-gram-word")
  
  
  # Frequency - 4-part-word
  # Save part-word object in disk for cache -> better performance
  
  grams4 <- tokenize_ngrams(sampledf$textLines,n=4,n_min = 4)
  #save(grams4, file="./Scripts/ProjectCapstone/grams4.RData")
  #load(file = "./Scripts/ProjectCapstone/grams4.RData")
  
  grams4df <- data.frame(table(unlist(grams4)))
  grams4df <- grams4df[order(grams4df$Freq,decreasing = TRUE),]
  save(grams4df, file="./Scripts/ProjectCapstone/AppCapstone/grams4df.RData")
  
  # How many unique words do you need in a frequency sorted 
  # dictionary to cover 50% of all word instances in the 
  # language? 90%?
  
  # Data = NGram Tokenizer
  # Percentage Cover: words ratio to cover in data 
  wordCoverage <- function(data, percentageCover){
    pcover <- percentageCover*sum(data$Freq)
    nwords <- 0
    for (i in 1:nrow(data)) {
      if (nwords >= pcover) {
        return (i)
      }
      nwords <- nwords+data$Freq[i]
    }
    
  }
  print("How many word to cover 50% of data - one-part-words:")
  wordCoverage(grams1df, 0.5)
  print("How many word to cover 90% of data - two-word-words:")
  wordCoverage(grams1df, 0.9)
  
  
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
  
  

  
  
  