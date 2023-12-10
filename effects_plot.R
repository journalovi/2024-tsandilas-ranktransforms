# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# A great library for simulating data for a large range of desings: https://debruine.github.io/faux
library(faux)

library(dplyr)
library(tidyr)
library(egg)
library(latex2exp)

############################################################################################
# Data generation for various experimental designs

# Encodes the 1:nlevels to numerical values
# It ensures a fixed distance between the two extreme levels (equal to 1) and random distances in between
# It also ensures that the sum is zero
categ2ratio <- function(nlevels) { 
	levels <- sort(c(0,1, runif(nlevels-2)))

	return(levels - mean(levels))
}

# Transforms categorical to numverical
toNumber <- function(factorvalues) {
	as.numeric(as.character(factorvalues))
}


simulate <- function(n = 30, nlevels = 2, effectSize = 1,  sd_0 = 1, sd_error = 1, convertFun = categ2ratio){
		data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels, sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept + effectSize * x1.t + sigma)

	data$s = factor(data$s)
	return(data)
}


plotDensities <- function(n = 10000, nlevels = 2, effectSize = 4) {
  data <- simulate(n, nlevels, effectSize) 

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = dv, fill = x1, color = x1), alpha = 0.5) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = c(-10, 10))

  plot
}


plotEffects <- function(){
	set.seed(200)

	plot1 <- plotDensities(effectSize=0.5,nlevels=2)
	plot2 <- plotDensities(effectSize=1,nlevels=2)
	plot3 <- plotDensities(effectSize=2,nlevels=2)
	plot4 <- plotDensities(effectSize=4,nlevels=2)
	plot5 <- plotDensities(effectSize=8,nlevels=2)

	plot6 <- plotDensities(effectSize=0.5,nlevels=3)
	plot7 <- plotDensities(effectSize=1,nlevels=3)
	plot8 <- plotDensities(effectSize=2,nlevels=3)
	plot9 <- plotDensities(effectSize=4,nlevels=3)
	plot10 <- plotDensities(effectSize=8,nlevels=3)

	plot11 <- plotDensities(effectSize=0.5,nlevels=4)
	plot12 <- plotDensities(effectSize=1,nlevels=4)
	plot13 <- plotDensities(effectSize=2,nlevels=4)
	plot14 <- plotDensities(effectSize=4,nlevels=4)
	plot15 <- plotDensities(effectSize=8,nlevels=4)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 0.5$")) + centered,
	  plot2 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 1$")) + centered,
	  plot3 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 2$")) + centered,
	  plot4 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 4$")) + centered,
	  plot5 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 8$")) + centered,
		 plot6, plot7, plot8, plot9, plot10, 
		 plot11, plot12, plot13, plot14, plot15, 
		 ncol= 5) 
} 




