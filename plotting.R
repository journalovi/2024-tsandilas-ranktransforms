# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

library(ggplot2)
library(gridExtra)
library(grid)
library(egg)

plotError <- function(df, n, max = 100, var = "rateX1X2", xvar = "effectX1", xlab, legend = TRUE) {
	legendPos = ifelse(legend, "top", "none") 

	cbPalette <- c("#888888", "#E69F00", "#009E73", "#FF5E00")
	shapes <- c(1, 2, 3, 8)

	distr_levels <- levels(df$distr)
	labels <- data.frame(
  		distr = distr_levels,
  		label = c(paste("n=", n, sep=""), rep(NA, length(distr_levels) - 1)) 
	)

	plot <- ggplot(df, aes(x=factor(eval(as.symbol(effectX1))), y=100*eval(as.symbol(var))))+ 
		facet_wrap(~factor(distr), nrow=1) +
	    geom_line(aes(group = method, color=method)) + 
	    geom_point(aes(color=method, shape=method))+
	    scale_y_continuous(limits = c(0, max + 5), breaks = seq(0, 100, 20), expand = c(0, 0)) +
	    ylab("Type I errors (%)") +
	    xlab(xlab) + 
	    theme_bw() + 
	    theme(axis.text.x = element_text(angle=-60, hjust=0, vjust=0.5)) +
	    theme(strip.background =element_rect(fill="#f0f0f0"))+
	    theme(legend.position=legendPos, legend.title=element_blank(),  legend.justification = c("right", "bottom"), legend.margin = margin(0, 0, 0, 0)) +
	    scale_shape_manual(values = shapes) +
	    scale_color_manual(values = cbPalette) + 
	    geom_label(data=labels, aes(x=0.7, y=max/2, label=label, vjust="center", hjust="left"), size=4)

	plot
}

plotErrorGrid <- function(df, max = 100, var = "rateX1X2", xvar = "effectX1", xlab, legend = TRUE) {
	legendPos = ifelse(legend, "top", "none") 

	cbPalette <- c("#888888", "#E69F00", "#009E73", "#FF5E00")
	shapes <- c(1, 2, 3, 8)

	n_levels <- unique(df$n)
	distr_levels <- levels(df$distr)

	labels <- data.frame(
  		distr = rep(distr_levels, length(n_levels)),
  		n = unlist(lapply(1:length(n_levels), function(index){ rep(n_levels[index], length(distr_levels)) })),
  		label = unlist(lapply(1:length(n_levels), function(index){ c(paste("n=", n_levels[index], sep=""), rep(NA, length(distr_levels) - 1)) }))
	)

	plot <- ggplot(df, aes(x=factor(eval(as.symbol(xvar))), y=100*eval(as.symbol(var))))+ 
		facet_grid(n ~ factor(distr)) +
	    geom_line(aes(group=method, color=method)) + 
	    geom_point(aes(color=method, shape=method))+
	    scale_y_continuous(limits = c(0, max + 5), breaks = seq(0, 100, 20), expand = c(0, 0)) +
	    ylab("Type I errors (%)") +
	    xlab(xlab) + 
	    theme_bw() + 
	    theme(axis.text.x = element_text(angle=-60, hjust=0, vjust=0.5)) +
	    theme(strip.background =element_rect(fill="#f0f0f0"))+
	    theme(legend.position=legendPos, legend.title=element_blank(),  legend.justification = c("right", "bottom"), legend.margin = margin(0, 0, 0, 0)) +
	    scale_shape_manual(values = shapes) +
	    scale_color_manual(values = cbPalette) +
	    theme(strip.text.y = element_blank()) + 
	    geom_label(data=labels, aes(x=0.7, y=max/2, label=label, vjust="center", hjust="left"), size=4)

	plot
}
