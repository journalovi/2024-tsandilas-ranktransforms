library(faux)
library(ggplot2)
library(ggExtra)
library(patchwork)

norm2lnorm <- function(x, meanlog = 0, sdlog = 1, mu = mean(x), sd = stats::sd(x), ...) {
   p <- stats::pnorm(x, mu, sd)
   stats::qlnorm(p, meanlog, sdlog, ...)
}

toOrdinal = function(values, thresholds) {
  discretizeValue = function(x){
    for(i in 1:length(thresholds)) {
      if(x <= thresholds[i]) return(i)
    }
    return(length(thresholds) + 1)
  }

  sapply(values, discretizeValue)
}

set.seed(100)

plotConversion <- function(xs, ys, xlab = "Normal scale", ylab, ymin = 0, ymax = max(ys), as.density = T) {
  n <- length(xs)
  grp <- c(rep(TRUE, n/2), rep(FALSE, n/2))

  palette <- c("#888888", "#FF5E00")

  scatter <- ggplot() + geom_point(aes(xs, ys, color = grp, fill =grp), show.legend = FALSE) +
    labs(x = xlab, y = ylab) + theme(plot.margin = margin(-2,-2,-2,-2)) + ylim(ymin, ymax) +
    scale_color_manual(values = alpha(palette, 0.1)) + theme_bw()

  marginal_top <- ggplot() + geom_density(aes(x = xs, fill = grp, color = grp), alpha = 0.5) + scale_color_manual(values = c("#FFFFFF00","#FFFFFF00")) + scale_fill_manual(values = palette) + theme_void() + 
    theme(legend.position = "none")

  marginal_right <- ggplot() 

  if(as.density) {
    marginal_right <- marginal_right + geom_density(aes(x = ys, fill = grp, color = grp), alpha = 0.5) + scale_color_manual(values = c("#FFFFFF00","#FFFFFF00")) +  scale_fill_manual(values = palette) + theme_void() + 
    theme(legend.position = "none") +  xlim(ymin, ymax) + coord_flip()
  } else {
    marginal_right <- marginal_right + geom_bar(aes(x = ys, fill = grp, color = grp), alpha = 0.5, position="stack", width = 0.5) + scale_color_manual(values = c("#FFFFFF00","#FFFFFF00")) +  scale_fill_manual(values = palette) + theme_void() + 
    theme(legend.position = "none") +  xlim(ymin, ymax) + coord_flip()
  }

  plot <- marginal_top + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + scatter + plot_spacer() + marginal_right  + 
    plot_layout(ncol = 3, nrow = 3, widths = c(4, -0.6, 1.2), heights = c(1.2, -0.6, 4))

  plot
}




