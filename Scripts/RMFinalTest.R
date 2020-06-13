# Summary
#
# This is a statistical study on exploring the relationship between a set of variables and miles per gallon (MPG) (outcome).
# 1. Is an automatic or manual transmission better for MPG
# 2. Quantify the MPG difference between automatic and manual transmissions

# Exploratory data analysis

  # The data provided are those of Mtcars
  # No download is needed

  str(mtcars)
  mtcars
  
  # The questions refer to basically two variables:
  # mpg: Miles per gallon
  # am: Transmission (0 = automatic, 1 = manual)
  
  mpg<- mtcars$mpg
  am <- mtcars$am
  unique(mpg)
  unique(am)
  
  # No missing values, no gaps
  
# 1. Is an automatic or manual transmission better for MPG?
   
  # The understanding of this question will be assumed as follows:
  # - Which type of transmission presented indicates the lowest mpg index;
  # - That is, which transmission has the best cost-benefit ratio in terms of fuel consumption (mpg).
  
  # To better visualize some pattern on mpg and transmission, we will plot a graph for better reading
  boxplot(mpg~am,
          ylab = "MPG",
          xlab = "Gear | 0-Auto  1-Manual",
          main="MPG vs Gears")
  
  # Let's try to find some correlation via Linear Regression. Since the variables are numeric, we can use them directly.
  linearR <- lm(mpg~am)
  plot(mpg~am,
       ylab = "MPG",
       xlab = "Gear | 0-Auto  1-Manual",
       main="MPG vs Gears")
  abline(linearR, lwd = 1)

  # Check coeficients  
  summary(linearR)
  
  # Coefficient of 7.245, with p-value of 0.000285 (***) - quite significant.
  # There is some relationship between MPG and transmission
  
  
# 2. Quantify the MPG difference between automatic and manual transmissions
  
  # Let's now analyze the influence of the transmission on the MPG:
  # Multiple R-squared is 0.3598, meaning that just a part of MPG can be attributed to transmission. 
  # To discover more information about the Variance:
  
  linearRV <- aov(mpg ~ ., data = mtcars)
  summary(linearRV)
  
  # Again, lets focus on itens with significance: 
  # - *** cyl = Number of cylinders
  # - * disp = Displacement
  # - wt = Weight (1000 lbs)
  
  linearRVF <- lm(mpg ~ cyl + disp + wt + am, data = mtcars)
  summary(linearRVF)
  
  # This analysis show an Multiple R-squared:  0.8327, suggesting that +83% of variance can be explained by cyl, disp, wt and transmission. 
  # P-values for cyl and weight are below 0.5, suggesting that these are confounding variables in the relation between car Transmission Type and Miles per Gallon.
  
  # Now lets check in more detail residuals:
  
  par(mfrow = c(2, 2))
  plot(linearRVF)

  # The “Residuals vs Fitted” plot here shows us that the residuals are homoscedastic. 
  # We can also see that they are normally distributed, with the exception of a few outliers.
  
  
  