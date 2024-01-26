

library(tidyverse)

readDataset <- function(file) {
  data <- read.csv(file, sep=",", header = TRUE)
  df <- gather(data, modversion, y, no_surface:yes_gist, factor_key=TRUE)
  df$modification <- ifelse(df$modversion %in% c('yes_surface', 'yes_gist'), 'yes', 'no')
  df$version <- ifelse(df$modversion %in% c('yes_surface', 'no_surface'), 'surface', 'gist')
  df <- df[c(1,4,5,3)]

  df$participant_id <- factor(df$participant_id)
  df$version <- factor(df$version)
  df$modification <- factor(df$modification)

  df
}

file_auc = "data/Siestrup23-AUC.csv"
file_far = "data/Siestrup23-FAR.csv"
file_ratings = "data/Siestrup23-ratings.csv"

#df <- readDataset(file_ratings)
df <- readDataset(file_auc)


library(plotly)

#library(dplyr)

fig <- df %>% plot_ly(x = ~version, y = ~y, type = 'violin', color = ~modification, points = "all",  pointpos = 0,  spanmode = 'hard', scalemode = "width", box = list(visible = F), meanline = list(visible = T, width = 4, color="black")) %>%
  layout(violinmode = "group") 
#plot_ly(x = ~version, y = ~y, type = 'scatter', mode = 'lines+markers', linetype = ~modification, color = ~modification) %>% layout(xaxis=list(scattergap = 0.7))