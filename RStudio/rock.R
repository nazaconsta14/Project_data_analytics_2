# Empezamos instalando librerias / we start by installing libraries

library(tidyverse)
library(tidyr)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(repr)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tm)
library(viridis)


# Empezamos a dar vistazos a la base de datos con la que vamos a trabajar / We take a look at the database with which we are going to work

database=history_of_rock_spotify_Copy

database

str(database)

dim(database)  

sum(duplicated(database))

glimpse(database) 

attach(database)

names(database)


# Luego empezamos a ver si la base de datos contiene valores null o na / We begin to see if the database contains empty values or, as they say, NA.

any(is.na(database))


which(is.na(database))


sum(is.na(database))


apply(database,MARGIN = 2,function(x) sum(is.na(x)))


###########################################################################################################################################


# Empezamos con la parte de el : Analisis / we start with the part : ANALYZE

#  / What consists of creating dataframes to answer questions and capture it in rstudio and then create a graph to be able to see the answer to that question in another way


# 1. artist - loudness - energy

top_rock_songs <- as.data.frame(table(database$artist))
colnames(top_rock_songs) <- c("artist", "Freq")
top_rock_songs <- merge(top_rock_songs, database[, c("artist", "loudness", "energy")], by = "artist")
top_rock_songs <- arrange(top_rock_songs, desc(loudness))
top_rock_songs <- top_rock_songs %>% distinct(artist, .keep_all = TRUE)
colnames(top_rock_songs) <- c("artist", "Freq", "loudness", "energy")
print(top_rock_songs)


# Mostrar los 10 primeros valores de la lista / Show top 10 values

top_teen_rock_songs <- top_rock_songs %>% slice_head(n = 10)

top_teen_rock_songs



##############


# 2. artist -  popularity


top_artist_bands <- as.data.frame(table(database$artist))
colnames(top_artist_bands) <- c("artist", "Freq")
top_artist_bands <- merge(top_artist_bands, database[, c("artist", "popularity")], by = "artist")
top_artist_bands <- arrange(top_artist_bands, desc(popularity))
top_artist_bands <- top_artist_bands %>% distinct(artist, .keep_all = TRUE)
colnames(top_artist_bands) <- c("artist", "Freq", "popularity")
print(top_artist_bands)


# Mostrar los 10 primeros valores de la lista / Show top 10 values

top_teen_artist_bands <- top_artist_bands %>% slice_head(n = 10)

top_teen_artist_bands



##############




#3 . name -  length


top_songs_bands <- as.data.frame(table(database$name))
colnames(top_songs_bands) <- c("name", "Freq")
top_songs_bands <- merge(top_songs_bands, database[, c("name", "length")], by = "name")
top_songs_bands <- arrange(top_songs_bands, desc(length))
top_songs_bands <- top_songs_bands %>% distinct(name, .keep_all = TRUE)
colnames(top_songs_bands) <- c("name", "Freq", "length")
print(top_songs_bands)


# Mostrar los 10 primeros valores de la lista / Show top 10 values

top_teen_songs_bands <- top_songs_bands %>% slice_head(n = 10)

top_teen_songs_bands


##############


#4. artist - danceability . energy


top_danceability_bands <- as.data.frame(table(database$artist))
colnames(top_danceability_bands) <- c("artist", "Freq")
top_danceability_bands <- merge(top_danceability_bands, database[, c("artist", "danceability...9","energy")], by = "artist")
top_danceability_bands <- arrange(top_danceability_bands, desc(danceability...9))
top_danceability_bands <- top_danceability_bands %>% distinct(artist, .keep_all = TRUE)
colnames(top_danceability_bands) <- c("artist", "Freq", "danceability...9" , "energy")
print(top_danceability_bands)


# Mostrar los 10 primeros valores de la lista / Show top 10 values

top_teen_danceability_bands<- top_danceability_bands %>% slice_head(n = 10)

top_teen_danceability_bands



##############





#5. artist - instrumentalness - acousticness


top_instrumentalness_bands <- as.data.frame(table(database$artist))
colnames(top_instrumentalness_bands) <- c("artist", "Freq")
top_instrumentalness_bands <- merge(top_instrumentalness_bands, database[, c("artist", "instrumentalness","acousticness")], by = "artist")
top_instrumentalness_bands <- arrange(top_instrumentalness_bands, desc(instrumentalness))
top_instrumentalness_bands <- top_instrumentalness_bands %>% distinct(artist, .keep_all = TRUE)
colnames(top_instrumentalness_bands) <- c("artist", "Freq", "instrumentalness" , "acousticness")
print(top_instrumentalness_bands)


# Mostrar los 10 primeros valores de la lista / Show top 10 values

top_teen_instrumentalness_bands<- top_instrumentalness_bands %>% slice_head(n = 10)

top_teen_instrumentalness_bands


#################################################################################################################################


# Graficos / Graphics


# 1. Grafico sobre artistas,instrumentos y acustica / Graphic artist - instrumentalness - acousticness

ggplot(top_teen_instrumentalness_bands, aes(x = instrumentalness, y = acousticness, size = Freq, color = acousticness)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = artist), vjust = -1, hjust = 0.5, size = 3, color = "black", check_overlap = TRUE) +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Relación entre Instrumentalidad y Acústica / Relationship between Instrumentality and Acoustics ",
       subtitle = "Top 10 bands by instrumentalness and acousticness",
       x = "instrumentalness",
       y = "acousticness",
       size = "Freq",
       color = "acousticness") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "gray"))




# 2. Grafico de sonido y energia / Graph about Sound and Energy about the artists


ggplot(top_teen_rock_songs, aes(x = loudness, y = energy, size = Freq, color = Freq)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = artist), vjust = -1, hjust = 0.5, size = 3, color = "black") +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Relación entre volumen y energía / Relationship between volume and energy",
       subtitle = "Las mejores canciones de rock analizadas por volumen y energía / Top rock songs analyzed by loudness and energy ",
       x = "Loudness",
       y = "Energy",
       size = "Freq",
       color = "Freq") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "gray"))




# 3. Grafico sobre la popularidad y las bandas / Graph about popularity and bands


lm_model <- lm(popularity ~ seq_along(popularity), data = top_teen_artist_bands)

ggplot(top_teen_artist_bands, aes(x = reorder(artist, -popularity), y = popularity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "green", linetype = "dashed") +
  geom_text(aes(label = popularity), vjust = -0.5, hjust = 1, size = 3, color = "black") +
  scale_y_continuous(breaks = seq(0, max(top_teen_artist_bands$popularity), by = 10)) +
  labs(title = "Popularidad de artistas-bandas / Popularity of Artists-Bands",
       subtitle = "Los 10 mejores artistas-bandas con mayor popularidad / Top 10 artists-bands with highest popularity",
       x = "Artist/Band",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "gray"))







# 4. Gráfico de Puntos sobre canciones mas largas de los artistas en la base de datos / Graph of Points on longest songs by artists in the database

ggplot(top_teen_songs_bands, aes(x = Freq, y = length, color = name)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = " Duración de la canción versus número de canciones por banda / Song Length vs. Number of Songs by Band",
       subtitle = "Top 10 bandas por número de canciones / Top 10 bands by number of songs",
       x = "Number of Songs",
       y = "Length of Songs (seconds)",
       color = "Song Name") +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = "gray"))








# 5. Grafico sobre las canciones mas bailables de los artistas en la base de datos / Graph about the most danceable songs of the artists in the database

ggplot(top_danceability_bands %>% filter(artist %in% top_teen_danceability_bands$artist), 
       aes(x = reorder(artist, -danceability...9), y = danceability...9, fill = artist)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Distribución de la bailabilidad por los mejores artistas / Distribution of Danceability by Top Artists",
       subtitle = "Los 10 mejores artistas con la mayor bailabilidad / Top 10 artists with the highest danceability",
       x = "Artist",
       y = "Danceability",
       fill = "Artist") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none")

