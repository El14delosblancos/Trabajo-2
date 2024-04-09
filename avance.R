
pacman::p_load(sjlabelled,
               
               dplyr, #Manipulacion de datos
               
               stargazer, #Tablas
               
               sjmisc, # Tablas
               
               summarytools, # Tablas
               
               kableExtra, #Tablas
               
               sjPlot, #Tablas y gráficos
               
               corrplot, # Correlaciones
               
               sessioninfo, # Información de la sesión de trabajo
               
               ggplot2) # Para la mayoría de los gráficos     

load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData")) #Cargar base de datos     


names(proc_data) # Muestra los nombres de las variables de la base de datos

dim(proc_data) # Dimensiones


stargazer(proc_data,type = "text")

sjmisc::descr(proc_data)

sjmisc::descr(proc_data,
              
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  
  kable(.,"markdown")

summarytools::dfSummary(proc_data, plain.ascii = FALSE)

view(dfSummary(proc_data, headings=FALSE))

proc_data_original <-proc_data

dim(proc_data)


sum(is.na(proc_data))

proc_data <-na.omit(proc_data)

dim(proc_data)

proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)

ggplot()

ggplot(proc_data, aes(x = conf_inst))

proc_data %>% ggplot(aes(x = conf_inst)) + 
  
  geom_bar()

proc_data %>% ggplot(aes(x = conf_inst)) + 
  
  geom_bar(fill = "coral")

proc_data %>% ggplot(aes(x = conf_inst)) + 
  
  geom_bar(fill = "coral")+
  
  labs(title = "Confianza en instituciones",
       
       x = "Confianza en instituciones",
       
       y = "Frecuencia")

graph1 <- proc_data %>% ggplot(aes(x = conf_inst)) + 
  
  geom_bar(fill = "coral")+
  
  labs(title = "Confianza en instituciones",
       
       x = "Confianza en instituciones",
       
       y = "Frecuencia") +
  
  theme_bw()

graph1

# y lo podemos guardar:

ggsave(graph1, file="C:/Users/Alumno.LAB-MP1AHLC2/Documents/GitHub/DM/Trabajo-2/output")

2

#Grafico de caja

sjt.xtab(proc_data$educacion, proc_data$sexo)

sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

tapply(proc_data$conf_inst, proc_data$educacion, mean)

proc_data %>% # se especifica la base de datos
  select(conf_inst,educacion) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(educacion)) %>% # se agrupan por la variable categórica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(conf_inst),SD=sd(conf_inst)) %>% # se agregan las operaciones a presentar en la tabla
  kable(, format = "markdown") # se genera la tabla

graph <- ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

graph

ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

datos <- proc_data %>% group_by(educacion) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()+
  ylim(0, 12)

proc_data$idenpa <- factor(proc_data$idenpa,
                           labels=c("Argentina",
                                    "Bolivia",
                                    "Brasil",
                                    "Chile",
                                    "Colombia",
                                    "Costa Rica",
                                    "Cuba",
                                    "República Dominicana",
                                    "Ecuador",
                                    "El Salvador",
                                    "Guatemala",
                                    "Honduras",
                                    "México",
                                    "Nicaragua",
                                    "Panamá",
                                    "Paraguay",
                                    "Uruguay",
                                    "Venezuela"),
                           levels=c("32",
                                    "68",
                                    "76",
                                    "152",
                                    "170",
                                    "188",
                                    "214",
                                    "218",
                                    "222",
                                    "320",
                                    "340",
                                    "484",
                                    "558",
                                    "591",
                                    "600",
                                    "604",
                                    "858",
                                    "862"))


graph_box <- ggplot(proc_data, aes(x = idenpa, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "País", y = "Confianza en instituciones") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x

graph_box

# y lo podemos guardar:

ggsave(graph_box, file="files/img/graph.png")
1

graph2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data, conf_gob,
                                              conf_cong,
                                              conf_jud,
                                              conf_partpol),
                                title = "Confianza en instituciones políticas") +
  theme(legend.position="bottom")

graph2

# Guardamos

ggsave(graph2, file="files/img/graph2.png")

graph3 <- proc_data %>% ggplot(aes(x = conf_inst, fill = sexo)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graph3

# Guardamos

ggsave(graph3, file="files/img/graph3.png")

proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad")+
  facet_wrap(~sexo)

graph4 <- ggplot(proc_data, aes(x = as.numeric(edad))) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Edad") +
  ylab("Cantidad")

graph4 

# Guardamos

ggsave(graph4, file="files/img/graph4.png")

datos <- proc_data %>% group_by(educacion, sexo) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 12)+
  facet_wrap(~sexo)

ggplot(datos, aes(x =sexo, y = promedio)) +
  geom_point() +
  labs(x = "Sexo", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 12)+
  facet_wrap(~educacion)

summary(proc_data$edad)

proc_data <- proc_data %>% 
  mutate(edad_groups = case_when(edad >=16 & edad<=25 ~ "Entre 16 y 25 años",
                                 edad >=26 & edad<=39 ~ "Entre 26 y 39 años",
                                 edad >=40 & edad<=65 ~ "Entre 40 y 65 años",
                                 edad >65 ~ "Más de 65 años"))

table(proc_data$edad_groups)

datos <- proc_data %>% group_by(educacion, edad_groups) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

datos <- proc_data %>% group_by(educacion, sexo, edad_groups) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio, color=sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

ggplot(datos, aes(x =educacion, y = promedio, color=sexo, shape=sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

ggplot(datos, aes(x = educacion, y = promedio, color = sexo, shape = sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw() +
  ylim(0, 7) +
  facet_wrap(~edad_groups) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData")) #Cargar base de datos
load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/data_wvs.RData")) #Cargar base de datos


context_data <- wvs %>% group_by(B_COUNTRY) %>% # Agrupar por país
  summarise(gdp = mean(GDPpercap1, na.rm = TRUE), # Promedio de GDP per capita
            life_exp = mean(lifeexpect, na.rm = TRUE), # Promedio esperanza de vida
            gini = mean(giniWB, na.rm = TRUE)) %>%  # Promedio gini
  rename(idenpa=B_COUNTRY) # Para poder vincular ambas bases, es necesario que la variable de identificación se llamen igual
context_data$idenpa <- as.numeric(context_data$idenpa) # Como era categórica, la dejamos numérica

proc_data <- proc_data %>% group_by(idenpa) %>%  # agrupamos por país
  summarise(promedio = mean(conf_inst, na.rm = TRUE)) # promedio de confianza en instituciones por país


data <- merge(proc_data, context_data, by="idenpa")


data1 <- right_join(proc_data, context_data, by="idenpa")



data2 <- left_join(proc_data, context_data, by="idenpa")


data3 <- full_join(proc_data, context_data, by="idenpa")


data <- data %>%
  mutate(idenpa = as.character(idenpa)) %>%
  mutate(idenpa = case_when(
    idenpa == "32" ~ "Argentina",
    idenpa == "68" ~ "Bolivia",
    idenpa == "76" ~ "Brasil",
    idenpa == "152" ~ "Chile",
    idenpa == "170" ~ "Colombia",
    idenpa == "188" ~ "Costa Rica",
    idenpa == "214" ~ "Cuba",
    idenpa == "218" ~ "República Dominicana",
    idenpa == "222" ~ "Ecuador",
    idenpa == "320" ~ "El Salvador",
    idenpa == "340" ~ "Guatemala",
    idenpa == "484" ~ "Honduras",
    idenpa == "558" ~ "México",
    idenpa == "591" ~ "Nicaragua",
    idenpa == "600" ~ "Panamá",
    idenpa == "604" ~ "Paraguay",
    idenpa == "858" ~ "Uruguay",
    idenpa == "862" ~ "Venezuela"))
data$gdp <- as.numeric(data$gdp)
data$gdp[data$gdp==0] <- NA
data <- na.omit(data)



data %>%
  ggplot(aes(x = gdp, y = promedio, label = idenpa)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  labs(x = "GDP", y = "Promedio") +
  theme_bw()

