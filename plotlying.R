
library(plotly)

# If byDesign is TRUE, then the menu will choose among three experimental designs 
plotlyError <- function(df, xlab = "magnitude of main effects", var = "rateX1X2", xvar = "effectX1", max = 100, byDesign = FALSE){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	cbPalette <- c("#888888", "#E69F00", "#009E73", "#FF5E00")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})


	if(byDesign){
		menuItems <- c("2x3 between", "2x4 mixed", "3x3x3 within")
		yvarnames <- c(paste(var,"2x3",sep="."), paste(var,"2x4",sep="."), paste(var,"3x3x3",sep="."))
	} else {
		menuItems <- c("n = 10", "n = 20", "n = 30")
		yvarnames <- c(paste(var,"10", sep="."), paste(var,"20", sep="."), paste(var,"30", sep="."))
	}

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[1])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[2])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[3])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = 'Type I errors (%)', font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F,  nticks=6, ticks="inside", tickfont = list(size = 11), range=c(0, max))
	  ) 

	  p
	}

	fig <- df %>% 
	      do(
	        p = createPlot(., dnames, symbols) 
	      ) %>% 
	      subplot(nrows = 1, shareX = TRUE, shareY = T, margin = 0.004) %>% layout(annotations = annots) %>% 
	        layout(annotations = list(x = 0.5, y = 0, yshift = -36, xref = "paper", yref = "paper", xanchor = "center", yanchor = "top", showarrow = F, text = xlab), margin = margins) %>%
	      layout(
	        updatemenus = list(list(
	          y = 1.25, x = -0.06, yanchor = 'bottom', xanchor = 'left', direction = 'right', active = 1,
	          buttons = list(
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale"))

	fig
}
