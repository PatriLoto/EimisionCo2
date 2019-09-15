
#Librerias
library(easypackages)
libraries("tidyverse","here","gganimate","gifski","LaCroixColoR", "extrafont","ggthemes","ggthemr", "ggsci")  #  ó todo el resto
library(tidyverse)
library(dygraphs)
library(RColorBrewer)
library(viridisLite)
library(ggsci)
library(DT)
library(extrafont)

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

#distribucion de la emisión de co2
color <-col = brewer.pal(n = 3, name = "RdBu")
boxplot(stAnual.modelo ~ cycle(stAnual.modelo), col = "maroon", main = "Distribucion de las emisiones de Co2",
        xlab = "Años",xlim(1960, 2014),
        ylab = "ppm", ylim(1.5,5.5)) 

# Dataset co2_ingreso
co2_ingreso <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2_ingreso.csv")
View(co2_ingreso)
head(co2_ingreso)
tail(co2_ingreso)
co2_ingresoSinNA <- na.omit(co2_ingreso)%>%filter(grupo!="Ingreso medio y bajo")%>% group_by(grupo)%>%arrange(desc(grupo))
View(co2_ingresoSinNA)
co2_ingresoSinNA[co2_ingresoSinNA$grupo == "Ingreso medio-altos",1]<-"Ingreso medio-alto"

#reasigno el orden de los factores
#1: Ingreso alto rojo
#2: Ingreso medio-alto  verde
#3: Ingreso medio amarillo
#4: Ingreso medio-bajo gris
#5: Ingreso Bajo celeste
co2_ingresoSinNA$grupo <-factor(co2_ingresoSinNA$grupo, levels = c("Ingreso alto","Ingreso medio-alto","Ingreso medio","Ingreso medio-bajo","Ingreso bajo"))
levels( co2_ingresoSinNA$grupo) 

co2_ingresoFinal <-co2_ingresoSinNA%>%group_by(grupo)%>%arrange(desc(grupo))
View(co2_ingresoFinal)
#-------------------------------------------------------------------------
# ejemplo de cómo se define un factor
#y2 <- factor(x2, levels = niveles_meses)
#y2
#--------------------------------------------------------------------------
ggplot(co2_ingresoSinNA, aes(anio, emision_co2)) +
  geom_point(aes(color = grupo)) +
  geom_smooth(aes(color = grupo, fill=grupo))+ 
  scale_color_brewer(palette = "RdBu")+
  scale_fill_brewer()+
 # scale_color_tron()+
 # scale_fill_tron()+
  theme_dark() +
  labs (title= "Emisión de Co2 por nivel de ingresos para el período: 1960- 2014", subtitle="A mayor nivel de ingresos mayor emisión de Co2 per cápita", 
        x = "", y = "Toneladas métricas per cápita", 
caption = "Fuente: Banco Mundial - Para #DatosdeMiercoles (21/08/2019) por Patricia Loto") +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.title = element_text("nivel"),
    legend.spacing = unit(0.4, "cm"),
    
    legend.text = element_text(colour ="Black" , size = 8, hjust = 0.3),
    axis.text.x = element_text(angle = 50, vjust = 1.5, hjust=1.4),
    plot.title = element_text(family="Garamond",
                              size=rel(2), 
                              hjust = 0.5,
                              vjust=2.5,            #Para separarlo del gráfico
                              position_identity(center),   
                              face="bold",       
                              color="darkred",     #Color del título: maroon, lightblue, lightblue,darkblue, darkorange, black.
                              lineheight=1.3),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "black", face = "bold",  hjust = 0.9, vjust=1, size = 8))+
  scale_x_continuous(breaks =seq(1960, 2014, by = 4))
    #ggrepel::geom_label_repel(aes(label = emision_co2), data = co2_ingresoSinNA, size = 4,
                            #  label.size = 0)

ggsave("emisionXgrupo5.png",width = 10, height = 5, dpi = "retina")

#------------------------------------------------------
