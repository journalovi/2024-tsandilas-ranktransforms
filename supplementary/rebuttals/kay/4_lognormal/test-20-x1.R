# author: Theophanis Tsandilas
# This code reads a template of a 4x3 repeated-measures design and makes the response variable take values from lognormal ditributions
# It then evaluates the Type I error rate of ART for the effect on x2 and the interaction effect when there is a strong effect on x1 

rm(list=ls())

library(dplyr)
library(tidyr)

library(lmerTest)
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)


# This method takes the template and assigns lognormal values to the response variable y 
# For each of the four levels of x1 we draw samples from a differet lognormal distribution
# meanslog: vector containing the meanlog parameters of these distributions
# sdlog: the common variance parameters of the distributions
# Here, we fix the meanslog parameters to specific values, but you can try different combinations
createRandomSample <- function(df, meanslog = c(-0.5, -0.25, 0.25, 0.5), sdlog = 1){

	# We first create some random subject-level intercepts (drawn from normal distribution)
	# You can vary the sd expressing individual variability
	df <- df %>% group_by(s) %>% mutate(intercept = rnorm(1, mean = 0, sd = 0.3))

	#  I know that x1 has four levels: A1, A2, A3, A4
	ncell <- nrow(df) / 4 # This is the number of observations for each level of x1

	# Make sure that the response is double precision	
	df <- df %>% mutate(y = as.double(y))

	# Let's now draw ncell1 observations for each level but from a different lognormal distribution.  
	# This sampling process does not make any distinction among the different levels of x2!
	df[df$x1 == "A1",]$y <- rlnorm(ncell, meanlog = meanslog[1] + df[df$x1 == "A1",]$intercept, sdlog)
	df[df$x1 == "A2",]$y <- rlnorm(ncell, meanlog = meanslog[2] + df[df$x1 == "A2",]$intercept, sdlog)
	df[df$x1 == "A3",]$y <- rlnorm(ncell, meanlog = meanslog[3] + df[df$x1 == "A3",]$intercept, sdlog)
	df[df$x1 == "A4",]$y <- rlnorm(ncell, meanlog = meanslog[4] + df[df$x1 == "A4",]$intercept, sdlog)

	df
}

# INT implementation
INT <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
}


# Analysis with two methods: PAR and ART
analyze <- function(df) {
	m.par <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	m.art <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	m.rnk <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	m.int <- suppressMessages(lmer(INT(y) ~ x1*x2 + (1|s), data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")
	c(# Return the p-values for the three effects 
		suppressMessages(anova(m.par)[vars, 6]), 
		suppressMessages(anova(m.art)[vars, 5]),
		suppressMessages(anova(m.rnk)[vars, 6]), 
		suppressMessages(anova(m.int)[vars, 6])
	) 
}

# Performs repetitive tests and assess Type I error rates 
test <- function(template, repetitions = 3000) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				analyze(createRandomSample(template)) # Analyze a random sample
			}, 
			error = function(cond) { }, 
			finally = { }
		)
	}

	# From p-values to Type I error rates (false positives)
	res.05 <- round(colMeans(results<.05, na.rm = TRUE), digits = 4)

	# Split the results into separate rows 
	return(tribble(~method, ~rates,
			"PAR", res.05[1:3], 
			"ART", res.05[4:6],
			"RNK", res.05[7:9],
			"INT", res.05[10:12]
		)
	)
}


#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

# Read the data template and fix the type of the factors, as required by the ARTool
template <- read.csv("template-20.csv", sep=",", header=TRUE, strip.white=TRUE)
template$s = factor(template$s)
template$x1 = factor(template$x1)
template$x2 = factor(template$x2)

# Call the simulation
results <- test(template, repetitions = 3000)

# Format the results
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[1:4]=c("method", "rateX1","rateX2","rateX1X2")

# Store the results
csvfile <- paste("log/results-20-x1", format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

