
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
