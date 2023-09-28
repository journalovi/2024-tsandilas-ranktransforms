# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay
# Annalysis for the illustrative example

rm(list=ls())
library(lmerTest)

library(effectsize) # Effect size functions

# The ARTool
library(ARTool)


# https://yingji15.github.io/2019-08-17-inverse-normal-transformation/
# https://www.biostars.org/p/80597/
# use rankit version where c = 0.5, based on https://www.researchgate.net/publication/286889581_Impact_of_Rank-Based_Normalizing_Transformations_on_the_Accuracy_of_Test_Scores
inverseNormalTransform = function(x){
	qnorm((rank(x,na.last="keep") - 0.5)/sum(!is.na(x)))
}



printAnalysis <- function(num, method, model) {
	cat(num, ". Analysis using ", method, "\n\n", sep="")

	print(anova(model))
	print(eta_squared(model))
	print(cohens_f(model))

	cat("\n==========================================================================\n", sep="")
}


# Read the dataset  
df <- read.csv("example_data.csv", sep=",", header=TRUE, strip.white=TRUE)
df$Technique = factor(df$Technique)
df$Difficulty = factor(df$Difficulty)


# For alternative analysis methods
mlog <- lmer(log(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - Log-transformation
mart <- art(Time ~ Difficulty*Technique + (1|Participant), data=df) # ARTool
mrnk <- lmer(rank(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Rank transformation 
mint <- lmer(inverseNormalTransform(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Inverse Transform transformation

printAnalysis(1, "Logarithmic Transformation", mlog)
printAnalysis(2, "ART", mart)
printAnalysis(3, "Rank Transformation", mrnk)
printAnalysis(4, "Inverse Normal Transformation", mint)
