# Evolution of song tempo

library(tidyverse)
library(ggplot2)
library(jpeg)
library(grid)
library(ggdark)
library(showtext)

theme_set(dark_theme_grey())

# Agregar fuente
font_add_google("Metal Mania", "metal")
showtext_auto()

metallica <- readRDS("metallica.rds")


# metallica$track_name <- fct_inorder(factor(metallica$track_name))
metallica$album_name <- fct_inorder(factor(metallica$album_name))

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
       subtitle = "New albums are almost as fast as the early ones.", 
       caption  = "Data: Spotify",
       x="Album", y="Tempo (bpm)")

# Agregar las tapas de los discos como imágenes.
covers <- list.files('covers', full.names = TRUE) %>% 
  purrr::map(readJPEG) %>% 
  purrr::map(rasterGrob, interpolate=TRUE)

y_coord_cover <-  60
y_offset <- 10.5


for (i in seq_along(covers)) {
  tempo <- tempo +  annotation_custom(grob=covers[[i]], xmin=i-1+.5, xmax=i+.5, ymin=y_coord_cover, ymax=y_coord_cover+y_offset)
}

tempo + 
  theme(
    text = element_text(family="metal"),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust=.5)
  ) +
  ggsave("tempo_2.png")


# https://www.kaylinpavlik.com/song-distance/