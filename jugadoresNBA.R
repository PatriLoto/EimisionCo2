
#--------------------------------------------------------------------------#
#scrapping

install.packages("rvest")
install.packages("xml2")
library(dplyr)
library(janitor)
library(xml2)
library(rvest)

#scrapping del sitio de la nba
url <- "https://www.basketball-reference.com/leagues/NBA_2019_totals.html"
jugadores <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]
# verifico tipo de datos
str(jugadores)
View(jugadores)
#elimino columnas nulas-cabeceras a minúsculas
jugadores <- jugadores %>% 
  remove_empty_cols() %>%  #if any exist
  clean_names() 
#equipo <-factor
jugadores$tm <-as.factor(jugadores$tm)


#
jugadoresN <-jugadores%>% mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
  mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0)))
 jugadoresNN<- jugadoresN%>% filter((jugadoresN$age!=0))
#
View(jugadoresNN)

P1<-jugadoresN%>% filter(jugadoresN$tm =="SAS")%>%summarise(sum=sum(jugadoresN$age),promEdad=(mean(jugadoresN$age)))

PA<-jugadoresNN%>% filter(jugadoresNN$tm =="CLE")%>%summarise(Emin2=min (jugadoresNN$age),Emax2= max(jugadoresNN$age),sum2=sum(jugadoresNN$age),promEdad2=(mean(jugadoresNN$age)))
P4<-jugadoresNN%>% filter(jugadoresNN$tm =="ATL")%>%summarise(Emin=min (jugadoresNN$age),Emax= max(jugadoresNN$age),sum=sum(jugadoresNN$age),promEdad=(mean(jugadoresNN$age)))


PromedioEdad <-jugadoresNN%>%group_by(jugadoresNN$tm)%>%arrange(jugadoresNN$tm)
PromedioEdad2 <-jugadoresNN%>%group_by(jugadoresNN$tm)%>%summarise(suma3=sum(jugadoresN$age), Emin3=min (jugadoresNN$age),Emax3= max(jugadoresNN$age))%>%arrange(jugadoresNN$tm)

#%>%summarise(suma=sum(jugadoresN$age),promEdad=(mean(jugadoresN$age)))%>%arrange(jugadoresN$tm)%>%View()
View(PromedioEdad)

#%>%summary(promEdad= mean(JugadoresStats$Age))


https://www.kaggle.com/tonyzhang1997/nba-player-analysis/log

http://blog.schochastics.net/post/analyzing-nba-player-data-i-getting-data/
