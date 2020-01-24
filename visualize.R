# incluir imagenes en ggplots:
# https://drmowinckels.io/blog/adding-external-images-to-plots/

# ggimage

theme_set(theme_minimal())

library(tidyverse)
library(ggplot2)

metallica <- readRDS("metallica.rds")

# does not work!
# mutate(track_name = fct_inorder(factor(track_name)))

metallica$track_name <- fct_inorder(factor(metallica$track_name))
metallica$album_name <- fct_inorder(factor(metallica$album_name))


# Dancebility
metallica %>% 
  ggplot(aes(track_name, danceability)) + 
  geom_col() + 
  facet_wrap(album_name~., scales="free", ncol = 3) +
  coord_flip() + 
  labs(title = "Danceability")

# Los temas con riffs repetitivos tienen alto nivel de danceability:
# Devil's Dance, The thing that should not be, Sad but True, etc.

# Dancebility
metallica %>% 
  ggplot(aes(track_name, danceability, fill=album_name)) + 
  geom_col() + 
  facet_wrap(album_name~., scales="free", ncol = 3) +
  coord_flip() + 
  labs(title = "Danceability") + 
  guides(fill = FALSE)

# En acousticness destacan las baladas: Mama Said, Nothing Else Matters,
# etc.
metallica %>% 
  ggplot(aes(track_name, acousticness, fill=album_name)) + 
  geom_col() + 
  facet_wrap(album_name~., scales="free", ncol = 3) +
  coord_flip() + 
  labs(title = "Acousticness") + 
  guides(fill=FALSE)

# Los temas instrumentales también se ven en esta medida
metallica %>% 
  ggplot(aes(track_name, instrumentalness, fill=album_name)) + 
  geom_col() + 
  facet_wrap(album_name~., scales="free", ncol = 3) +
  coord_flip() + 
  labs(title = "Instrumentalness") + 
  guides(fill=FALSE)

metallica %>% 
  ggplot(aes(track_name, tempo, color = album_name, group = 1)) + 
  scale_x_discrete(breaks = NULL) +
  geom_point() +
  geom_smooth(se=FALSE) + 
  geom_smooth(aes(group=album_name), se = FALSE, method = "lm", formula = y~1) + 
  guides(color=FALSE) + 
  labs(title = "Song tempo over time")

