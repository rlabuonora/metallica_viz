# incluir imagenes en ggplots:
# https://drmowinckels.io/blog/adding-external-images-to-plots/

# ggimage
library(tidyverse)
library(ggplot2)
library(jpeg)
library(grid)
library(ggdark)
library(showtext)

theme_set(dark_theme_grey())

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



# Evolution of tempo
tempo <- metallica %>% 
  ggplot(aes(track_name, tempo, color = album_name, group = 1)) + 
  scale_x_discrete(breaks = NULL) +
  geom_point() +
  geom_smooth(se=FALSE) + 
  geom_smooth(aes(group=album_name), se = FALSE, method = "lm", formula = y~1) + 
  guides(color=FALSE) + 
  labs(title = "Song tempo over time", x="Albums", y="Tempo (BPM)")

# Evolution of energy
energy <- metallica %>% 
  ggplot(aes(track_name, energy, color = album_name, group = 1)) + 
  scale_x_discrete(breaks = NULL) +
  geom_point() +
  geom_smooth(se=FALSE) + 
  geom_smooth(aes(group=album_name), se = FALSE, method = "lm", formula = y~1) + 
  guides(color=FALSE) + 
  labs(title = "Song energy over time", x="Albums", y="Energy (BPM)")

fast_songs <- c("Holier Than Thou", 
                "My Apocalypse", 
                "The Four Horsemen")

highligts <- metallica %>% 
  filter(track_name %in%  fast_songs)

# Gráfico de base.
tempo <- metallica %>% 
  ggplot(aes(album_name, tempo, group = 1)) + 
  scale_x_discrete(breaks = NULL, expand=c(0.1, 0)) +
  scale_y_continuous(limits = c(60, 220)) +
  geom_point() +
  geom_text_repel(data = highligts, 
                  aes(label=track_name), 
                  nudge_y = 10,
                  nudge_x = .2,
                  color="white", size = 3) +
  geom_smooth(se=FALSE) + 
  guides(color=FALSE) + 
  labs(title="Metallica", 
       subtitle = "U-shaped evolution of song tempo", 
       caption  = "data: Spotify",
       x="Album", y="Tempo (bpm)")
  

# Agregar las tapas de los discos como imágenes.
covers <- list.files('covers', full.names = TRUE) %>% 
  purrr::map(readJPEG) %>% 
  purrr::map(rasterGrob, interpolate=TRUE)

y_coord_cover <-  60
y_offset <- 10.5

font_add_google("Metal Mania", "metal")
showtext_auto()

output <- tempo + 
  annotation_custom(grob=covers[[1]], xmin=.5, xmax=1.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[2]], xmin=1.5, xmax=2.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[3]], xmin=2.5, xmax=3.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[4]], xmin=3.5, xmax=4.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[5]], xmin=4.5, xmax=5.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[6]], xmin=5.5, xmax=6.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[7]], xmin=6.5, xmax=7.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[8]], xmin=7.5, xmax=8.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[9]], xmin=8.5, xmax=9.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[10]], xmin=9.5, xmax=10.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset) +
  annotation_custom(grob=covers[[11]], xmin=10.5, xmax=11.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset)  +
  theme(
    text = element_text(family="metal"),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust=.5)
  ) +
  ggsave("tempo_2.png")


# https://www.kaylinpavlik.com/song-distance/

  