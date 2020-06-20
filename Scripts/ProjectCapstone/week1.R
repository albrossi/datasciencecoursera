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
  
  # Convert unwanted symbols in proper format
  samplecleaned = iconv(sample, "UTF-8","ASCii","byte")
  
  # Generate corpus of data
  samplecorpus = Corpus(VectorSource(samplecleaned))
  
  # Remove punctuation
  corpusdatacleaned = tm_map(samplecorpus,removePunctuation)
  
  # Convert data into lower case
  corpusdatacleaned = tm_map(corpusdatacleaned,content_transformer(tolower))
  
  # Remove numbers from the data
  corpusdatacleaned = tm_map(corpusdatacleaned,removeNumbers)
  
  # Remove whitespace
  sample = tm_map(corpusdatacleaned,stripWhitespace)
  rm(samplecorpus, corpusdatacleaned, samplecleaned)
  
  # Remove black listed word (Profanity cleaning)
  conn <- file("./Scripts/ProjectCapstone/blacklist.txt", "rb")
  profanity <- readLines(conn, encoding = "UTF-8")
  close(conn)
  sample <- tm_map(sample, removeWords, profanity) 
  rm(profanity)
  
  
# Data exploration
  # Exploratory analysis - perform a thorough exploratory analysis of the 
  # data, understanding the distribution of words and relationship between 
  # the words in the corpora.
  
  # Tokenization process: breaking the cleaned sample into works 
  # to better work with than.
  
  # 1,2,3 gram tokenizaion: break sample into 1-part-word, 2-parts-word, 3-parts-word
  UniGramTokenizer = function(data) NGramTokenizer(data, Weka_control(min = 1, max = 1))
  BiGramTokenizer = function(data) NGramTokenizer(data, Weka_control(min = 2, max = 2))
  TriGramTokenizer = function(data) NGramTokenizer(data, Weka_control(min = 3, max = 3))
  
  #Generate term document matrix for all tokenizers
  Unigram.tdm = TermDocumentMatrix(sample, control = list(tokenize = UniGramTokenizer))
  bigram.tdm = TermDocumentMatrix(sample, control = list(tokenize = BiGramTokenizer))
  trigram.tdm = TermDocumentMatrix(sample, control = list(tokenize = TriGramTokenizer))
  
  # Understand frequencies of words and word pairs - build figures and 
  # tables to understand variation in the frequencies of words and word 
  # pairs in the data.
  
  FreqTerms = findFreqTerms(Unigram.tdm, lowfreq = 100)
  UniFreq.words = rowSums(as.matrix(Unigram.tdm[FreqTerms,]))
  UniFreq.words.df = data.frame(UniGram=FreqTerms, Frequency=UniFreq.words)
  ggplot(UniFreq.words.df, aes(x=reorder(UniGram, Frequency), y=Frequency)) +
    geom_bar(stat = "identity") +  coord_flip() +
    xlab("UniGram words") + ylab("Frequency") +
    labs(title = "Most frequent Unigram words")





