# script for Bifactor CFA
  # author: Molly Grant 
  # date: 14 August 2024 
  # R version 4.4.1 (2024-06-14 ucrt)

########################
# overview ###
########################
# This R script provides simple code for Bifactor analysis ###
# The code uses the lavaan package  ###
# The simple code is then followed with an example using synthetic data###
########################

########################
# set-up ###
########################
#### load packages ####
packages <- c("dplyr", "psych", "factoextra", "lavaan","GPArotation", "ggplot2", "corrplot", "semPlot")

# Load all packages in the list
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

########################
# simple code for Bifactor analysis ###
########################
#explore data
head(data)
View(data)

#descriptive statistics
descriptive_stats <- psych::describe(data)
print(descriptive_stats)

# CFA bifactor model
model <- ' 
G  =~ x1 + x2 + x3 + x4 + x5 + x6 #this is the general factor
factor1 =~ x1 + x2 + x3 
factor2 =~ x4 + x5 + x6 
'#specify the model

modelfit <- lavaan::cfa(model, data = data) #fit the model to the data
summary(modelfit, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) #fit.measures = TRUE command provides fit indices: CFI, TLI, AIC, BIC, sample-size adjusted BIC, RMSEA # standardized = TRUE command provides a summary with standardized loadings as well as non-standardized loadings
semPaths(modelfit, 
         "std",                # Standardized estimates
         layout = "tree",      # Tree layout
         edge.color = "black", # Line color for edges
         edge.label.color = "black", # Label color for edge labels
         node.color = "white", # Node color (optional)
         node.label.color = "black", # Node label color
         residuals = FALSE,    # Optionally hide residuals
         sizeMan = 7,          # Size of measurement nodes (optional)
         sizeLat = 7,          # Size of latent nodes (optional)
         labels = TRUE,        # Show labels
         fade = FALSE,         # Do not fade edges (fully opaque)
         highlight = NULL,     # Optionally, no highlights
         edge.width = 0.3)    

########################
# Bifactor analysis example ###
########################
set.seed(123)  
#### create dataset ####
# creating a dataset for g + 3 factors, all factors are orthogonal
n  <- 5000
g  <- rnorm(n)
f1 <- rnorm(n)
f2 <- rnorm(n)
f3 <- rnorm(n)

x1 <- .9*f1 + .7*g + rnorm(n,0,sqrt(1 - (.9*.7)^2))
x2 <- .8*f1 + .7*g + rnorm(n,0,sqrt(1 - (.8*.7)^2))
x3 <- .7*f1 + .7*g + rnorm(n,0,sqrt(1 - (.7*.7)^2))
x4 <- .6*f1 + .7*g + rnorm(n,0,sqrt(1 - (.6*.7)^2))
x5 <- .6*f2 + .7*g + rnorm(n,0,sqrt(1 - (.6*.7)^2))
x6 <- .7*f2 + .7*g + rnorm(n,0,sqrt(1 - (.7*.7)^2))
x7 <- .8*f2 + .7*g + rnorm(n,0,sqrt(1 - (.8*.7)^2))
x8 <- .9*f2 + .7*g + rnorm(n,0,sqrt(1 - (.9*.7)^2))
x9 <- .6*f3 + .7*g + rnorm(n,0,sqrt(1 - (.6*.7)^2))
x10 <- .7*f3 + .7*g + rnorm(n,0,sqrt(1 - (.7*.7)^2))
x11 <- .8*f3 + .7*g + rnorm(n,0,sqrt(1 - (.8*.7)^2))
x12 <- .9*f3 + .7*g + rnorm(n,0,sqrt(1 - (.9*.7)^2))

bifac <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
rm(n,f1,f2,f3,g,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
summary(bifac)
nrow(bifac)

#explore data
head(bifac)
View(bifac)

#descriptive statistics
descriptive_stats <- psych::describe(bifac)
print(descriptive_stats)

# Correlation matrix
cor_matrix <- cor(bifac, use = "complete.obs")
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "number", type = "upper", tl.cex = 0.8, addCoef.col = "black")

# Histograms of each variable x1 to x12
par(mfrow = c(3, 4))
for (i in 1:12) {
  hist(bifac[[paste0("x", i)]], main = paste0("Histogram of x", i), xlab = paste0("x", i), col = "lightblue", border = "white")
}
par(mfrow = c(1, 1))

# Boxplots to detect outliers for each variable x1 to x12
par(mfrow = c(3, 4))
for (i in 1:12) {
  boxplot(bifac[[paste0("x", i)]], main = paste0("Boxplot of x", i), col = "lightgreen")
}
par(mfrow = c(1, 1))

# CFA bifactor model
model <- ' 
G  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 #this is the general factor
factor1 =~ x1 + x2 + x3 + x4
factor2 =~ x5 + x6 + x7 + x8
factor3 =~ x9 + x10 + x11 + x12

#restrictions
G ~~ 0*factor1 #This restricts the covariance between these factors to be zero = assumes no correlation between these two specific factors.
G ~~ 0*factor2
G ~~ 0*factor3
factor1 ~~ 0*factor2
factor1 ~~ 0*factor3
factor2 ~~ 0*factor3
'#specify the model

modelfit <- lavaan::cfa(model, data = bifac) #fit the model to the data
summary(modelfit, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) #fit.measures = TRUE command provides fit indices: CFI, TLI, AIC, BIC, sample-size adjusted BIC, RMSEA # standardized = TRUE command provides a summary with standardized loadings as well as non-standardized loadings
semPaths(modelfit, 
         "std",                # Standardized estimates
         layout = "tree",      # Tree layout
         edge.color = "black", # Line color for edges
         edge.label.color = "black", # Label color for edge labels
         node.color = "white", # Node color (optional)
         node.label.color = "black", # Node label color
         residuals = FALSE,    # Optionally hide residuals
         sizeMan = 7,          # Size of measurement nodes (optional)
         sizeLat = 7,          # Size of latent nodes (optional)
         labels = TRUE,        # Show labels
         fade = FALSE,         # Do not fade edges (fully opaque)
         highlight = NULL,     # Optionally, no highlights
         edge.width = 0.3)    
