
#Librerias
library(easypackages)
libraries("tidyverse","here","gganimate","gifski","LaCroixColoR", "extrafont","ggthemes","ggthemr", "ggsci")  #  ó todo el resto
library(tidyverse)
library(dygraphs)
library(RColorBrewer)
library(viridisLite)
library(ggsci)
library(DT)

install_packages("readr")
# Lectura Dataset Co2
co2 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2.csv")
View(co2)
head(co2)
tail(co2)
# Pre-procesamiento
co2filtro <- co2 %>% filter(pais_region!="Not classified")
co2filtro$pais_region
# elimino los valores nulos y ordeno por país_región
co2SinNA <- na.omit(co2filtro)%>%arrange(pais_region)
View(co2SinNA) 

#filtro por países pertenecientes a Latinoamérica
latam <- filter(co2SinNA, pais_region %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(latam)
tail(latam)
#muestro valores de emision de Latinoamérica
paraDT <- latam %>% select(-codigo_iso)%>% rename(Pais= pais_region, Año=anio, EmisionCo2=emision_co2)
datatable(paraDT, rownames = FALSE,
          options = list(pageLength = 10))
#----------------------------------------------------------------------------
 #selecciono datos de Argentina y luego selecciono unicamente la columna emisión de co2
#stAnual<-latam %>% filter(pais_region=="Argentina")%>%select(emision_co2)
stAnual<-co2 %>% filter(pais_region=="Argentina")%>%select(emision_co2)%>%na.omit(co2$emision_co2)
View(stAnual)
#selecciono datos de Uruguay
stAnualUruguay<-co2 %>% filter(pais_region=="Uruguay")%>%select(emision_co2)
View(stAnualUruguay)

#genero la serie de datos con los datos de Argentina
stAnual.modelo<- ts(data = stAnual, start = c(1960), end = c(2014), frequency = 1)
stAnual.modelo
#genero la serie de datos con los datos de Uruguay
stAnualUruguay.modelo <- ts(data = stAnualUruguay, start = c(1960), end = c(2014), frequency = 1)
stAnualUruguay.modelo

#Gráficos
# 1. Serie de tiempo para twitter con datos de Argentina
library(easypackages)
libraries("tidyverse","dygraph","RColorBrewer")
#lectura de datos
co2 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2.csv")
#filtro los registros de Argentina y elimino los registros que tengan valores nulos
stAnual2<-co2 %>% filter(pais_region=="Argentina")%>%select(emision_co2)%>%na.omit(co2$emision_co2)
#genero la serie temporal
stAnual.modelo<- ts(data = stAnual, start = c(1960), end = c(2014), frequency = 1)
#grafico la serie temporal
dygraph(stAnual.modelo, main = "Evolución de la emisión de Co2 en Argentina") %>%
  dyAxis("x", label="") %>%
  dyAxis("y", label = "Toneladas métricas per cápita")%>%
  dyOptions(colors = RColorBrewer::brewer.pal(n = 5, name = "Spectral")) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3), highlightCircleSize = 5) %>%
  dyEvent("1963-1-1","Año 1963: Menor emisión de Co2", labelLoc = "bottom") %>%
  dyEvent("2014-1-1","Año 2014: Mayor emisión de Co2", labelLoc = "bottom")%>%
  dyOptions(drawPoints = TRUE, pointSize = 1, colors =RColorBrewer::brewer.pal(n = 3, name = "RdBu"))


# 2. Serie de tiempo para twitter con datos de Uruguay
dygraph(stAnualUruguay.modelo, main = "Emisiones de Co2 en Uruguay") %>%
  dyAxis("x", label="") %>%
  dyAxis("y", label = "Toneladas métricas per cápita")%>%
  dyOptions(colors = RColorBrewer::brewer.pal(n = 2, name = "Dark2")) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2), highlightCircleSize = 5) %>%
  dyAnnotation("1986-1-1", text = "A", tooltip = "Menor emisión de Co2") %>%
  dyAnnotation("2012-1-1", text = "B", tooltip = "Mayor emisión de Co2")%>%
  dyOptions(drawPoints = TRUE, pointSize = 1, colors =RColorBrewer::brewer.pal(n = 3, name = "RdBu"))
#--------------------------------------------------------------
# Otros ejemplos de Series Temporales
#--------------------------------------------------------------

#Pob. Colombia:49.07, Pob. Argentina:44.27, Pob. Perú: 32.17
stAnualPeru<-co2 %>% filter(pais_region=="Perú")%>%select(emision_co2)%>%na.omit(co2$emision_co2)
stAnualPeru
stAnualCo<-co2 %>% filter(pais_region=="Colombia")%>%select(emision_co2)%>%na.omit(co2$emision_co2)
stAnualCo
stAnual.modeloP<- ts(data = stAnualPeru, start = c(1960), end = c(2014), frequency = 1)
stAnual.modeloP
stAnual.modeloC<- ts(data = stAnualCo, start = c(1960), end = c(2014), frequency = 1)
stAnual.modeloC

ComparacionEmision <- cbind(stAnual.modeloP, stAnual.modelo, stAnual.modeloC)
ComparacionEmision

dygraph(ComparacionEmision, main = "Comparativa de la emisión de CO2 entre Perú, Argentina y Colombia") %>%
  dySeries("stAnual.modeloP", label = "Perú") %>%
  dySeries("stAnual.modelo", label = "Argentina") %>%
  dySeries("stAnual.modeloC", label = "Colombia") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("x", label="") %>%
  dyAxis("y", label = "Toneladas métricas per cápita")%>%  
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)%>%
  dyLegend(show = "follow", width = 98)%>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 5, 
  highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)) %>%
  dyOptions(colors = brewer.pal(n = 6, name = "RdBu"))Dark2
#RdBu Dark2 PRGn Spectral


dygraph(stAnual.modelo, main = "Evolución de la emisión de Co2 en Argentina") %>%
  dyAxis("x", label="") %>%
  dyAxis("y", label = "Toneladas métricas per cápita")%>%
  dyOptions(colors = RColorBrewer::brewer.pal(n = 2, name = "Dark2")) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2), highlightCircleSize = 5) %>%
  dyAnnotation("1963-1-1", text = "A", tooltip = "Menor emisión de Co2") %>%
  dyAnnotation("2014-1-1", text = "B", tooltip = "Mayor emisión de Co2")%>%
  dyOptions(drawPoints = TRUE, pointSize = 1, colors =RColorBrewer::brewer.pal(n = 3, name = "RdBu"))







color <-col = brewer.pal(n = 3, name = "RdBu")
boxplot(stAnual.modelo ~ cycle(stAnual.modelo), col = "maroon", main = "Distribucion de las emisiones de Co2",
        xlab = "Años",xlim(1960, 2014),
        ylab = "ppm", ylim(1.5,5.5)) 

# Dataset co2_ingreso
co2_ingreso <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2_ingreso.csv")
View(co2_ingreso)
head(co2_ingreso)
tail(co2_ingreso)

#unifico los grupos en co2_ingreso
co2_ingreso[co2_ingreso$grupo == "Ingreso medio y bajo",1]<-"Ingreso medio-bajo"
co2_ingreso[co2_ingreso$grupo == "Ingreso medio-altos",1]<-"Ingreso medio-alto"
co2_ingreso$grupo <-as.factor(co2_ingreso$grupo)
str(co2_ingreso)
co2_ingresoSinNA <- na.omit(co2_ingreso)%>%group_by(grupo)%>%arrange(grupo)
View(co2_ingresoSinNA)

install.packages("ggplotly")
library(ggplotly)
py <- plotly()
data_bar <- list(x =co2_ingresoSinNA$anio,
                 y = co2_ingresoSinNA$emision_co2,
                 type = "bar",
                 marker = list(color = brewer.pal(6, "Paired")))

layout_bar <- list(title = "Price of Meals", xaxis = list(title = "Meal"), yaxis = list(title = "Price ($)"))
response <- py$plotly(data = data_bar, kwargs = list(layout = layout_bar, filename = "Your_Filename"),fileopt = "overwrite")
  response                    
 py$ggplotly(kwargs = list(filename = "Your_Filename",
                            fileopt = "overwrite"))
  
  ggplot(data = co2_ingresoSinNA,
         aes(x = anio,
             y = emision_co2,
             group = grupo,
             colour = grupo)) +
    scale_colour_brewer(palette = "Dark2")
#--------------------------------------------------------------------------

data(iris)

ggplot(co2_ingresoSinNA, aes(anio, emision_co2)) +
  geom_point(aes(color = grupo)) +
  geom_smooth(aes(color = grupo, fill = grupo)) + 
  scale_color_tron()+
  scale_fill_tron()+
  theme_dark() +
  labs (title= "Emisión de Co2 por grupo socioeconómico", subtitle="Período: 1960- 2014", x = "", y = "Toneladas métricas per cápita", 
        caption = "Fuente: Banco Mundial - DatosdeMiercoles por Patricia Loto")+
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D"),
    legend.title = element_blank(),
    legend.text = element_text(colour ="Black" , size = 8),
    axis.text.x = element_text(angle = 50, vjust = 1.5, hjust=1.4),
    plot.title = element_text(family="Courier",
                              size=rel(1), 
                              hjust = 0.5,
                              vjust=2.5,            #Para separarlo del gráfico
                              position_identity(center),   
                              face="bold",       
                              color="darkred",     #Color del título: maroon, lightblue, lightblue,darkblue, darkorange, black.
                              lineheight=2),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "black", face = "bold",  hjust = 1.2, vjust=1)) +
      #scale_size_discrete(range = c(1960,1970,1980,1990,1991,1992,1993,1994, 2014)))

ggsave("emisionXgrupo.png",width = 10, height = 5, dpi = "retina")


#------------------------------------------------------
