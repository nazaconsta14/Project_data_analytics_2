# Empezamos instalando las librerias  / we start by installing libraries

library(tidyverse)
library(tidyr)
library(dplyr)
library(here) 
library(skimr) 
library(readr) 
library(janitor) 
library(lubridate) 
library(ggplot2) 
library(ggrepel)
library(stringr)

# Damos un vistazo a las base de datos con la que vamos a trabajar / We take a look at the database with which we are going to work

data=vgchartz_2024

str(data)

dim(data)  

sum(duplicated(data))

glimpse(data)

attach(data)

names(data)

# Empezamos viendo si la base de datos contiene valores nulos o vacias / We begin to see if the database contains empty values or, as they say, NA.


any(is.na(data))


which(is.na(data))


sum(is.na(data))


apply(data,MARGIN = 2,function(x) sum(is.na(x)))


# Luego de haber visto y analizado si existen valores de NA y donde se ubican, procedemos a trabajar para eliminarlos colocando un valor/ After having seen and analyzed if there are NA values and where they are located, we proceed to work to eliminate them by placing a value

# Esta es la primera forma con la que vamos a solucionar los valores nulos o vacios / This is the first way we are going to work with NA values

data[is.na(data$critic_score),"critic_score"] <- mean(data$critic_score,na.rm = TRUE)
data[is.na(data$total_sales),"total_sales"] <- mean(data$total_sales,na.rm = TRUE)
data[is.na(data$na_sales),"na_sales"] <- mean(data$na_sales,na.rm = TRUE)
data[is.na(data$jp_sales),"jp_sales"] <- mean(data$jp_sales,na.rm = TRUE)
data[is.na(data$pal_sales),"pal_sales"] <- mean(data$pal_sales,na.rm = TRUE)
data[is.na(data$other_sales),"other_sales"] <- mean(data$other_sales,na.rm = TRUE)
data[is.na(data$release_date),"release_date"] <- mean(data$release_date,na.rm = TRUE)


# Esta es la segunda forma con la que vamos a solucionar los valores nulos o vacios /  This is the second way we are going to work with NA values


data2<-data %>%
  select(title,console,genre,publisher,developer,critic_score,total_sales,na_sales,jp_sales,pal_sales,other_sales,release_date) %>%
  mutate(developer=replace_na(developer,"Unknow"))%>%
  view()



head(data2)



######################


# Empezamos con la parte : Analisis / we start with the part : ANALYZE

# Que consiste en crear dataframes para responder preguntas y capturarlo en rstudio para luego crear un gráfico para poder ver la respuesta a esa pregunta de otra manera / What consists of creating dataframes to answer questions and capture it in rstudio and then create a graph to be able to see the answer to that question in another way






# 1

# Lo que vamos a hacer es crear un dataframe poniendo los nombres y todas sus tiendas críticas. / What we are going to do is create a dataframe putting the names and all their critic_stores

top_10_games <- as.data.frame(table(data2$title))
colnames(top_10_games) <- c("title", "Freq")
top_10_games <- merge(top_10_games, data2[, c("title", "critic_score")], by = "title")
top_10_games <- arrange(top_10_games, desc(critic_score))
top_10_games <- top_10_games %>% distinct(title, .keep_all = TRUE)
colnames(top_10_games) <- c("title", "Freq", "critic_score")

print(top_10_games)



# Mostramos los primeros 10 valores / show top 10 values

top_teen_games <- top_10_games %>% slice_head(n = 10)

top_teen_games








# 2

# Lo que vamos a hacer es crear un dataframe poniendo los nombres de los juegos, desarrolladores, género y todas sus ventas / What we are going to do is create a dataframe putting the names of the games, developers, genre and all their sales

top_games_sales<- as.data.frame(table(data2$title))
colnames(top_games_sales) <- c("title", "Freq")
top_games_sales <- merge(top_games_sales, data2[, c("title", "total_sales","developer","genre")], by = "title")
top_games_sales <- arrange(top_games_sales, desc(total_sales))
top_games_sales <- top_games_sales %>% distinct(title, .keep_all = TRUE)
colnames(top_games_sales) <- c("title", "Freq", "total_sales","developer","genre")


top_games_sales






# Mostramos los primeros 10 valores / show top 10 values

top_teen_games_sales <- top_games_sales %>% slice_head(n = 10)

top_teen_games_sales




# 3

# Lo que vamos a hacer es acumular los nombres de los juegos, uniéndolos con una columna llamada "tienda de críticos" y mostrarla / What we are going to do is accumulate the names of the games, joining it with a column called "critic_store" and display it

top_10_games_critic_score <- data2 %>%
  group_by(title) %>%
  summarize(Games_Critic_Score = sum(critic_score)) %>%
  arrange(desc(Games_Critic_Score)) %>%
  head(10)

top10_Games_Score <- top_10_games_critic_score


View(top10_Games_Score)

top10_Games_Score


# 4

# Lo que vamos a hacer es acumular los nombres de los juegos, uniéndolos con una columna llamada "otras ventas" y mostrarlo / What we are going to do is accumulate the names of the games, joining it with a column called "other_sales" and display it

top_games_other_sales <- data2 %>%
  group_by(title) %>%
  summarize(Other_Sales = sum(other_sales)) %>%
  arrange(desc(Other_Sales)) %>%
  head(10)

filtered_games_other_sales <- data2 %>%
  filter(title %in% top_10_songs_other_sales$title)

top_10_games_other_sales <- top_games_other_sales


view(top_10_games_other_sales)


top_10_games_other_sales




#5

# Lo que vamos a hacer es acumular los nombres de los desarrolladores del juego, agregar los juegos que pertenecen a cada desarrollador y mostrarlo / What we are going to do is accumulate the names of the game developers, add the games that belong to each developer and display it

top_developer_sales <- data2 %>%
  group_by(developer) %>%
  summarize(developer_total_sales = sum(total_sales)) %>%
  arrange(desc(developer_total_sales)) %>%
  head(10)

filtered_developer_sales <- data2 %>%
  filter(developer %in% top_developer_sales$developer)

top_10_developer_sales <- top_developer_sales


view(top_10_developer_sales)


top_10_developer_sales




# 6. Cambios de configuracion en columnas de la base de datos / Configuration changes in database columns


# Convertir release_date al tipo de datos Fecha / Convert release_date to Date data type
data2$release_date <- as.Date(data2$release_date)


# Extraemos el día, mes y año / Extract the day, month and year
data2$posting_day <- format(data2$release_date, "%d")
data2$posting_month <- format(data2$release_date, "%m")
data2$posting_year <- format(data2$release_date, "%Y")



################################################################################






# Graficos / Graphics

################################################################################


#2

top_10_games_sales_share<- top_teen_games_sales%>% 
  ggplot( mapping = aes(x=genre, y=title, fill=total_sales))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  ggtitle("Top 10 Games and his genre with best sales")


top_10_games_sales_share


################################################################################

#3

top10_Games_Score_share<-top10_Games_Score %>% 
  ggplot(mapping = aes(x=title, y=Games_Critic_Score, fill=Games_Critic_Score))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  guides(fill=FALSE) +
  ggtitle("Top 10 Games with best critic_score")


top10_Games_Score_share


################################################################################


#5

top_10_developer_sales_share<-top_10_developer_sales %>% 
  ggplot(mapping = aes(x=developer, y=developer_total_sales, fill=developer_total_sales))+ 
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_gradient(low = "grey20", high = "springgreen3")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  guides(fill=FALSE) +
  ggtitle("Top 10 Developers of Games")

top_10_developer_sales_share

################################################################################

#6

#AÑO/YEARN

options(repr.plot.width=5, repr.plot.height=3)
year <- as.data.frame(table(data2$posting_year))
colnames(year) <- c("Year", "Freq")
the_years_where_more_games_were_made <- ggplot(data = year, aes(Year, Freq, fill = Year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Freq), color="black", size=3) +
  ggtitle("Jobs postings by Year") + 
  guides(fill=FALSE)
the_years_where_more_games_were_made


################################################################################


# MES/MONTH


options(repr.plot.width=5, repr.plot.height=3)
library(ggplot2)

month <- as.data.frame(table(data2$posting_month))
colnames(month) <- c("month",  "Freq")  
the_months_where_more_games_were_made <- ggplot(data = month, aes(month, Freq, fill = month)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=Freq), color="black", size=3) +
  ggtitle("Jobs postings by Month") + 
  guides(fill=FALSE) +
  theme_minimal() 

the_months_where_more_games_were_made
