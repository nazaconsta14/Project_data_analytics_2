# Empezamos instalando las librerias con las que vamos a trabajar / we start by installing libraries

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

# Damos un vistazo a la base de datos con la que vamos a trabajar /We take a look at the database with which we are going to work

manga

str(manga)

dim(manga)  

sum(duplicated(manga))

glimpse(manga) 

attach(manga)

names(manga)


# Vemos errores en la base de datos y vamos a proceder a solucionarlos / We see errors in values in the demographics column and we proceed to correct them


unique(manga2$demographics)





manga2$demographics <- gsub(pattern = "\\s*Shoujo\\s*", replacement = " Shōjo ", manga2$demographics, ignore.case = TRUE)
manga2$demographics <- gsub(pattern = "\\s*Shounen\\s*", replacement = " Shōnen ", manga2$demographics, ignore.case = TRUE)




# Luego empezamos a ver si la base de datos contiene valores nulos o vacios / We begin to see if the database contains empty values or, as they say, NA.

any(is.na(manga))


which(is.na(manga))


sum(is.na(manga))


apply(manga,MARGIN = 2,function(x) sum(is.na(x)))


# Luego de haber visto y analizado si existen valores de NA y donde se ubican, procedemos a trabajar para eliminarlos colocando un valor / After having seen and analyzed if there are NA values and where they are located, we proceed to work to eliminate them by placing a value

# Esta es la primera forma con la que vamos a solucionar los valores nulos o vacios en la base de datos / This is the first way we are going to work with NA values

manga[is.na(manga$score),"score"] <- mean(manga$score,na.rm = TRUE)
manga[is.na(manga$start_date),"start_date"] <- mean(manga$start_date,na.rm = TRUE)
manga[is.na(manga$end_date),"end_date"] <- mean(manga$end_date,na.rm = TRUE)
manga[is.na(manga$updated_at),"updated_at"] <- mean(manga$updated_at,na.rm = TRUE)



# Esta es la segunda forma con la que vamos a solucionar los valores nulos o vacios en la base de datos / This is the second way we are going to work with NA values

manga2<-manga %>%
  select(manga_id,title,type,score,scored_by,status,volumes,chapters,start_date,end_date,members,favorites,sfw,approved,created_at_before,updated_at,real_start_date,real_end_date,genres,themes,demographics,authors,serializations,synopsis) %>%
  mutate(volumes=replace_na(volumes,0))%>%
  mutate(chapters=replace_na(chapters,0))%>%
  mutate(synopsis=replace_na(synopsis,"Unknow"))%>%
  mutate(real_start_date=replace_na(real_start_date,"Unknow"))%>%
  mutate(real_end_date =replace_na(real_end_date ,"Unknow"))



head(manga2)



# Empezamos con la parte de : Analisis / we start with the part : ANALYZE

# Que consiste en crear dataframes para responder preguntas y capturarlo en rstudio para luego crear un gráfico para poder ver la respuesta a esa pregunta de otra manera / What consists of creating dataframes to answer questions and capture it in rstudio and then create a graph to be able to see the answer to that question in another way



# creamos marco de datos / creating the dataframe

loc1 <- as.data.frame(table(manga2$title))
colnames(loc1) <- c("title", "Freq")
loc1 <- merge(loc1, manga2[, c("title", "score")], by = "title")
loc1 <- arrange(loc1, desc(score))
loc1 <- loc1 %>% distinct(title, .keep_all = TRUE)
colnames(loc1) <- c("title", "Freq", "score")

print(loc1)


# Mostramos los primeros 10 valores / Show top 10 values

top_teen <- loc1 %>% slice_head(n = 10)

top_teen


# Creamos el grafico / Creating the graph

options(repr.plot.width = 5, repr.plot.height = 3)

tilt_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 2))

p1 <- ggplot(data = head(loc1, 10), aes(title, Freq, fill = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), color = "red", size = 8) +
  ggtitle("Top 10 Manga Titles by Score") +
  coord_flip() +
  tilt_theme 

# Mostramos el grafico / Show the graph

print(p1)





##########################################################################

# creamos marco de datos / creating the dataframe

loc2 <- as.data.frame(table(manga2$demographics))

colnames(loc2) <- c("demographics", "Freq")

loc2 <- merge(loc2, manga2[, c("demographics", "score")], by = "demographics")

loc2 <- arrange(loc2, desc(score))

loc2 <- loc2 %>% distinct(demographics, .keep_all = TRUE)

colnames(loc2) <- c("demographics", "Freq", "score")

print(loc2)



# Mostramos los tres mejores valores / show top 3 values

top_three <- loc2 %>% slice_head(n = 3)

top_three




# Creamos el grafico / creating the graph
p2 <- ggplot(data = loc2, aes(x = reorder(demographics, score), y = score, fill = score)) +
  geom_bar(stat = "identity") +
  geom_text(data = top_three, aes(label = round(score, 2)), color = "black", size = 3, vjust = -0.5) +
  ggtitle("Top 3 Demographic Genres by Score") +
  xlab("Demographic Genre") +
  ylab("Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Mostramos el grafico / Show the graph
print(p2)


##########################################################################



# creamos marco de datos / creating the dataframe

loc3<-as.data.frame(table(manga2$status))
colnames(loc3)<-c("status","Freq")

loc3<-merge(loc3, manga2[, c("status", "manga_id")], by = "status")
loc3<-arrange(loc3, desc(manga_id))
loc3<-loc3 %>% distinct(status, .keep_all = TRUE)
colnames(loc3) <- c("status", "Freq", "manga_id")



head(loc3)


 # Creamos el grafico / creating the graph 
p3 <- ggplot(data = loc3, aes(x = status, y = Freq, fill = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), color = "black", size = 4, vjust = -0.5) +
  ggtitle("quantity of manga by its condition") +
  xlab("Condition") +
  ylab("Quantity Of Manga") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Mostramos el grafico / Show the graph
print(p3)




##########################################################################


# creamos marco de datos / creating the dataframe


loc4 <- as.data.frame(table(manga2$serializations))
colnames(loc4) <- c("serializations", "Freq")
loc4 <- merge(loc4, manga2[, c("serializations", "score")], by = "serializations")
loc4 <- arrange(loc4, desc(score))
loc4 <- loc4 %>% distinct(serializations, .keep_all = TRUE)
colnames(loc4) <- c("serializations", "Freq", "score")

print(loc4)


# Mostramos los primeros 10 valores / Show top 10 values

top_teen <- loc4 %>% slice_head(n = 10)

top_teen




# Creamos el grafico / creating the graph 
tilt_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))


p4 <- ggplot(top_teen, aes(x = reorder(serializations, score), y = Freq, fill = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(score, 2)), color = "black", size = 3, vjust = -0.5) +
  ggtitle("Top 14 Manga Serializations by Score") +
  xlab("Manga Serializations") +
  ylab("Frequency") +
  coord_flip() +
  tilt_theme

# Mostramos el grafico / Show the graph
print(p4)


####################################################################################################3


# creamos marco de datos / creating the dataframe


loc5 <- as.data.frame(table(manga2$type))
colnames(loc5) <- c("type", "Freq")
loc5 <- merge(loc5, manga2[, c("type", "score")], by = "type")
loc5 <- arrange(loc5, desc(score))
loc5 <- loc5 %>% distinct(type, .keep_all = TRUE)
colnames(loc5) <- c("type", "Freq", "score")

print(loc5)






# Creamos el grafico / creating the graph 
tilt_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1))



p5 <- ggplot(loc5, aes(x = reorder(type, score), y = Freq, fill = score)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(score, 2)), color = "black", size = 3, vjust = -0.5) +
  ggtitle("Top 7 type of novels  by Score") +
  xlab("Novels") +
  ylab("Frequency") +
  coord_flip() +
  tilt_theme

# Mostramos el grafico / Show the graph

print(p5)





