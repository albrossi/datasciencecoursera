    # Part 1: Simulation Exercise Instructions
    
    ## First things first: Load data
    #Load data and parameters from exercise
    
    #Number os samples
    n <- 40
    
    #Lambda = 0.2
    lambda <- 0.2
    
    #Number of simulations
    b <- 1000
    
    #CI = 95%
    z <- 1.96
    
    #As data is from aleatory distribution generation, set seed for reproducibility
    set.seed(96)
    
    ## 1. Show the sample mean and compare it to the theoretical mean of the distribution.
    
    
    #Create a table to hold data
    data <- matrix(rexp(n * b, rate = lambda), b)
    
    #Simulated mean per sample row 
    sample <- rowMeans(data)
    
    #Mean of means
    simMeanMean <- mean(sample)
    
    #Theoretical exponential mean
    theMean <- 1/lambda
    
    #Histogram of the sample means and theoretical mean
    hist(sample, xlab="Sample Mean", ylab = "Frequence")
    abline(v=theMean, col="blue", lwd=1)
    abline(v=simMeanMean, col="red", lwd=1)
    print(paste("Simulated exponential mean: ", round(simMeanMean, 2)))
    print(paste("Theoretical mean: ", round(theMean, 2)))

    print(paste("simulated exponential variance: ", round(var(simMean), 2)))
    
    par(mfrow = c(1, 1))
    simNorm <- rnorm(1000, mean = simMeanMean, sd = sd(sample))
    hist(sample, breaks = 20, main = "Exponential Distribution - Lambda equals to 0.2", xlab = "Means", col = "red")
    hist(simNorm, breaks = 20, main = "Normal Distribution - Mean and SD from sample", xlab = "Normal", col = "blue")
    
    
    library(ggplot2)
    library(stats)
    data(ToothGrowth)
    
  
    
    summary(ToothGrowth)
    str(ToothGrowth)
    qplot(x=supp,y=len,data=ToothGrowth, facets=~dose, main="Tooth growth grouped by Supplement type and dosage",xlab="Supplement type", ylab="Tooth length") + geom_boxplot(aes(fill = supp))
    
    t.test(x = ToothGrowth$len, data = ToothGrowth, paired = FALSE, conf.level = 0.95)$conf
    mean(ToothGrowth[ToothGrowth$supp == "OJ", ]$len)
    mean(ToothGrowth[ToothGrowth$supp == "VC", ]$len)    