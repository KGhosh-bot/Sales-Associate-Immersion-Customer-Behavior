setwd("C:/Users/Kankana Ghosh/Dropbox/PC/Desktop/Immersion/No1")
# Load necessary packages (install if not already installed)
install.packages("lavaan")
library(lavaan)
install.packages("semptools")

# Load the semTools package
library(semTools)
library(semPlot)

# Load your dataset into a data frame
data <- read.csv("xerxes_syn.csv")
View(data)
# Specify the mediation model
model <- '
  # Direct paths
  time ~ a*peak 
  purchase ~ b*time + c*peak

  # Indirect effect
  indirect_effect := a*b
  
  #Total effect
  total_effect := c+indirect_effect
'

# Fit the model
fit <- sem(model, data = data)
varTable(fit)
# Print the model summary
summary(fit, standardized = TRUE)

# Create the path diagram
p <- semPaths(fit, whatLabels= "est",residuals = FALSE, standardized = TRUE, layout = "tree", edge.label.cex = 1.2,edge.color= "black",nDigits = 3, nCharNodes = 8, sizeMan = 12, normalize= TRUE)
library(semptools)
p_2 <- mark_sig(p, fit)
#p_3 <- mark_se(p_2, fit, sep = "\n")
plot(p_2)

