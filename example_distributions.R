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
inverseNormalTransform <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
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
df$logTime <- log(df$Time) 

dfmean <- aggregate(logTime ~ Difficulty, data = df, mean)

dfsd <- aggregate(logTime ~ Difficulty + Participant, data = df, mean)
dfsd <- aggregate(logTime ~ Difficulty, data = dfsd, sd)

curve(dlnorm(x, dfmean[1,2], dfsd[1,2]), xlim = c(0, 5))
curve(dlnorm(x, dfmean[2,2], dfsd[2,2]), add = TRUE, xlim = c(0, 5))
curve(dlnorm(x, dfmean[3,2], dfsd[3,2]), add = TRUE, xlim = c(0, 5))
curve(dlnorm(x, dfmean[4,2], dfsd[4,2]), add = TRUE, xlim = c(0, 5))

