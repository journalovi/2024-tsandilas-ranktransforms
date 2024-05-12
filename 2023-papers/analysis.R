# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# https://cran.r-project.org/web/packages/bib2df/vignettes/bib2df.html

library(dplyr)
library(ggplot2)
library(tidyr)
library(bib2df)
library(stringr)

df <- bib2df("citations-ART-2024-no-theses.bib")

#  %>% slice(1:3)

df$venue <- tools::toTitleCase(ifelse(!is.na(df$JOURNAL), df$JOURNAL, df$BOOKTITLE))

journals <- df %>%
  filter(!is.na(JOURNAL)) %>%
  group_by(JOURNAL) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) 

publishers <- df %>%
  filter(!is.na(PUBLISHER)) %>%
  group_by(PUBLISHER) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

confs <- df %>%
  filter(!is.na(BOOKTITLE)) %>%
  group_by(BOOKTITLE) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

 venues <- df %>%
  filter(!is.na(venue)) %>%
  group_by(venue) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

medic <- df[str_detect(df$venue, regex("medic", ignore_case = TRUE)), ]$venue
bio <- df[str_detect(df$venue, regex("bio|medic", ignore_case = TRUE)), ]$venue
neur <- df[str_detect(df$venue, regex("neur|brain", ignore_case = TRUE)), ]$venue
nature <- df[str_detect(df$venue, regex("nature", ignore_case = TRUE)), ]$venue

venues <- venues %>% slice(1:17)
venues$venue <- factor(venues$venue, levels = venues$venue[order(venues$n)])

plot1 <- ggplot(venues, aes(x = venue, y = n)) +
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = n), 
    ## make labels left-aligned
    hjust = 1, nudge_y = -.4, 
  ) + coord_flip()  +
  theme_void() + theme(axis.title = element_blank()) + 
  theme(axis.text.y = element_text(size = 10, hjust = 1, family = "Arial"),
        plot.margin = margin(rep(15, 4)))

# plot <- ggbarplot(venues, x = "venue", y = "n", 
#            fill = "orange",
#            sort.val = "asc",                      
#            rotate = TRUE,                              
#            dot.size = 6,                                
#            label = TRUE, lab.col = "black", lab.pos = "out",    
#            position = position_stack(),         
#            ggtheme = theme_pubr()                       
#         ) + theme(axis.line.y=element_blank(), axis.ticks.y=element_blank())

