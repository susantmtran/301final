library(tidyverse)
library(naniar)
library(skimr)
library(lubridate)
library(dplyr)

load("Data/movies.RData")

'''
variables of interest: `critics_rating`, `audience_rating`, `top200_box` , 
`best_actor_win`, `best_actress_win` , 
and `best_dir_win` ........ to predict `best_pic_nom`.
'''

#not gonna need imbd_url or rt_url

#variable focused
glimpse(movies)

#skim data
skim_without_charts(movies)

#missingness
miss_var_summary(movies)


movies %>% 
  ggplot(aes(best_pic_nom, fill = best_pic_nom)) +
  geom_bar(position = "dodge") +
  facet_wrap(~mpaa_rating) +
  scale_fill_brewer(name = NULL,
                  palette = "Set2",
                  labels = c("Not Nominated", "Nominated")) +
  labs(x = NULL, y = NULL,
       title = "Best Picture Nominations by Rating") +
  theme_classic() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  face = "bold"))

#come back to this
movies %>% 
  group_by(best_pic_nom) %>% 
  filter(mpaa_rating == "R", best_pic_nom == "yes")

movies_1 <- movies %>% 
  mutate(thtr_rel_month = month(thtr_rel_month, label = TRUE))

movies_1 %>% 
  ggplot(aes(
    x = thtr_rel_month,
    fill = best_pic_nom
  )) +
  geom_bar(position = "stack") +
  scale_fill_brewer(name = NULL,
                    palette = "Set2",
                    labels = c("Not Nominated", "Nominated")) +
  labs(x = NULL, y = NULL,
       title = "Best Picture Nominations by Month Released in Theaters") +
  theme_classic() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  vjust = 0,
                                  face = "bold"))

