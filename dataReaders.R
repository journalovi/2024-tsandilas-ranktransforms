# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# the effectType parameter determines if I'm interested in results where there are effects on both factors (effectType = 0) 
# or only on the first factor (effectType = 1) or in all the results (effectType = 2)
readData <- function(prefix, n = 20, alpha = .05, effectType = 0, 
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		methods = c("PAR", "RNK", "INT", "ART")) {
	df <- read.csv(paste("data/", prefix, "_", n, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	df$distr <- factor(df$distr, levels=distributions)
	df$method <- factor(df$method, levels=methods)

	if(effectType == 0) {
		df <- df[df$effectX2 > 0 | df$effectX1 == 0,]
	} else if(effectType == 1) {
		df <- df[df$effectX2 == 0,]
	}
	return(df[df$alpha == alpha,])
}

prefix <- "1_test_4x3_Ratio"
alpha=.05

df <- rbind(
  readData(prefix, 10, alpha, 0), 
  readData(prefix, 20, alpha, 0),
  readData(prefix, 30, alpha, 0)
)

df <- reshape(df, idvar=c("design", "distr","method","alpha","effectX1","effectX2","effectX1X2"), timevar = "n", direction = "wide")
