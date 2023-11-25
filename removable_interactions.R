rm(list=ls())
library(lmerTest)
library(ARTool)

library(brms)

inverseNormalTransform = function(x){
   qnorm((rank(x) - 0.5)/length(x))
}


df <- read.csv("removable_interactions.csv", sep=",", header=TRUE, strip.white=TRUE)
df$SID <- factor(df$SID)
df$Difficulty <- factor(df$Difficulty, order = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)

m_par <- lmer(PerceivedPerformance ~ Difficulty*Technique + (1|SID), data = df)
m_rnk <- lmer(rank(PerceivedPerformance) ~ Difficulty*Technique + (1|SID), data = df)
m_int <- lmer(inverseNormalTransform(PerceivedPerformance) ~ Difficulty*Technique + (1|SID), data = df)
m_art <- art(PerceivedPerformance ~ Difficulty*Technique + (1|SID), data = df)

m_brm <- brm(
  formula = PerceivedPerformance ~ mo(Difficulty)*Technique + (1|SID),
  data = df,
  family = cumulative("probit")
)


