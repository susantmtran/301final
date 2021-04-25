library(tidyverse)
library(naniar)
library(skimr)
library(lubridate)

load("Data/movies.RData")

'''
variables of interest: `critics_rating`, `audience_rating`, `top200_box` , 
`best_actor_win`, `best_actress_win` , 
and `best_dir_win` ........ to predict `best_pic_nom`.
'''

#not gonna need imbd_url or rt_url

view(movies)

#variable focused
glimpse(movies)

#skim data
skim_without_charts(movies)

#missingness
miss_var_summary(movies)


movies %>% 
  ggplot(aes(x = mpaa_rating, fill = best_pic_nom)) +
  geom_bar(position = "dodge")


movies %>% 
  ggplot(aes(x = genre, fill = best_pic_nom)) +
  geom_bar()+
  coord_flip()

movies_1 <- movies %>% 
  mutate(thtr_rel_month = month(thtr_rel_month, label = TRUE))

movies_1 %>% 
  ggplot(aes(
    x = thtr_rel_month
             )) +
  geom_bar()


