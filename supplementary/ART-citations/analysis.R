
rm(list=ls())

library(ggplot2)
library(bib2df)
library(stringr)
library(dplyr)

df <- suppressWarnings(bib2df("citations-ART-2024-no-theses.bib"))

df$venue <- tools::toTitleCase(ifelse(!is.na(df$JOURNAL), df$JOURNAL, df$BOOKTITLE))
venues <- df %>%
  filter(!is.na(venue)) %>%
  group_by(venue) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# Identify different types of venues
medic <- df[str_detect(df$venue, regex("medic", ignore_case = TRUE)), ]$venue
bio <- df[str_detect(df$venue, regex("bio|medic", ignore_case = TRUE)), ]$venue
neur <- df[str_detect(df$venue, regex("neur|brain", ignore_case = TRUE)), ]$venue
nature <- df[str_detect(df$venue, regex("nature", ignore_case = TRUE)), ]$venue

venues <- venues %>% dplyr::slice(1:17)
venues$venue <- factor(venues$venue, levels = venues$venue[order(venues$n)])

# Figure 4 in the paper
plot <- ggplot(venues, aes(x = venue, y = n)) +
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = n), 
    ## make labels left-aligned
    hjust = 1, nudge_y = -.4, 
  ) + coord_flip()  +
  theme_void() + theme(axis.title = element_blank()) + 
  theme(axis.text.y = element_text(size = 10, hjust = 1, family = "Arial"),
        plot.margin = margin(rep(15, 4)))


