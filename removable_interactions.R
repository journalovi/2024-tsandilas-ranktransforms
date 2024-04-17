rm(list=ls())
library(lmerTest)
library(ARTool)
library(effectsize) 

inverseNormalTransform = function(x){
   qnorm((rank(x) - 0.5)/length(x))
}


df <- read.csv("removable_interactions.csv", sep=",", header=TRUE, strip.white=TRUE)
df$SID <- factor(df$SID)
df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)

# Time
mt_par <- lmer(Time ~ Difficulty*Technique + (1|SID), data = df)
mt_rnk <- lmer(rank(Time) ~ Difficulty*Technique + (1|SID), data = df)
mt_int <- lmer(inverseNormalTransform(Time) ~ Difficulty*Technique + (1|SID), data = df)
mt_art <- art(Time ~ Difficulty*Technique + (1|SID), data = df)
mt_log <- lmer(log(Time) ~ Difficulty*Technique + (1|SID), data = df)


# Perceived Performance
mp_par <- lmer(PerceivedPerformance ~ Difficulty*Technique + (1|SID), data = df)
mp_rnk <- lmer(rank(PerceivedPerformance) ~ Difficulty*Technique + (1|SID), data = df)
mp_int <- lmer(inverseNormalTransform(PerceivedPerformance) ~ Difficulty*Technique + (1|SID), data = df)
mp_art <- art(PerceivedPerformance ~ Difficulty*Technique + (1|SID), data = df)

# Also try the ANOVA-type statistic (ATS)
# https://www.quantargo.com/help/r/latest/packages/nparLD/2.1/nparLD
library(nparLD)
mp_ats <- nparLD(PerceivedPerformance ~ Difficulty*Technique, data=df, subject="SID", description=FALSE)


# And finally, we comnduct an analysis with a probit model -- This is the recommended one (Bayesian approach)
library(brms)
 mp_brm <- brm(
   formula = PerceivedPerformance ~ mo(Difficulty)*Technique + (1|SID),
   data = df,
   family = cumulative("probit")
 )


# And frequentist approach
df$PerceivedPerformance <- factor(df$PerceivedPerformance, ordered = TRUE)
# Frequentist probit/logit model 
mprob <- clmm(PerceivedPerformance ~ Difficulty*Technique + (1|SID), link ="probit", threshold = "flexible", data=df)

