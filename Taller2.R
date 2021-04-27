# Elaborado por: Gabriela Navarro (201821568), Daniela Quintero y Maria Alejandra Saavedra (201815221)
# Fecha de elaboración: 23/04/2021
# Ultima modificación:

### Taller A ###

# Configuración inicial

rm(list = ls()) #Limpia el entorno de R
pacman::p_load(here,tidyverse,reshape2, data.table) #Cargar y/o instalar paquetes requeridos

# Punto 1 - Loops

# 1.0. Se importa el archivo lista.rds con readRDS por el formato del archivo. 

lista_df = readRDS(file = "data/input/lista.rds")
lista_df

# Existe un error con dos dataframes donde se borro un titulo. Para evitar que se elimine la fila se arregla antes del loop. 
# En estos dos dataframes que tenemos una de las columnas esta vacia. El problema surge cuando eliminemos las filas de las listas que no tengan NA, pues se eliminaria la fila de los titulos tambien en estos dos casos.
# Por eso se va a convertir esa fila NA en un caracter, con el fin de preservar los titulos de estos dos dataframes

lista_df$`lesiones-personales-2012-2`[8,20] <- is.character(lista_df$`lesiones-personales-2012-2`[8,20])
lista_df$`lesiones-personales-2012-3`[8,20] <- is.character(lista_df$`lesiones-personales-2012-3`[8,20])

# 1.1. 

# Se utiliza un loop For In que evalue todos los elementos de la lista_df y ejecute las condiciones que quiero, como eliminar las filas con NA,
# fijar los titulos de columnas y modificar el titulo (ponerlo en minusculas)

# Adicionalmente se utiliza gsub para homogenizar los titulos de las diferentes listas y garantizar un mejor orden cuando se convierta en dataframe en el punto 1.3

for (i in (1:74)){
  lista_df[[i]] <- na.omit(lista_df[[i]])
  colnames(lista_df[[i]]) <- tolower(chartr("áéíóú","aeiou",lista_df[[i]][1,]))
  colnames(lista_df[[i]]) <- gsub("profesiones","profesion",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("pais nace","pais de nacimiento",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase sitio","clase de sitio",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase empleado","clase de empleado",colnames(lista_df[[i]]))
  lista_df[[i]] <- lista_df[[i]][-1,]
}

# 1.2. Asegúrese de crear una variable tipo_delito que almacene el tipo de delito.

# La forma mas eficaz de dividir los elementos de la lista por tipo de delito es crear un loop for in 
# que me haga el trabajo de agregar por categorias a las listas que se indiquen. 

# Adicionalmente para garantizar una mejor organizacion, con ayuda de los loops se pueden nombrar los delitos con su respectivo nombre y año

# HOMICIDIOS
homicidios <- list()
for (a in (1:10)){
  homicidios[a] <- lista_df[a]
  names(homicidios)[a] <- paste("Homicidios 201",as.character(a-1),sep = "")
}

# HURTO A ENTIDADES COMERCIALES
hurto_entidades_comerciales <- list()
iterar = 0
for (b in c((11:13),(23:29))){
  iterar = iterar + 1
  hurto_entidades_comerciales[iterar] <- lista_df[b]
  names(hurto_entidades_comerciales)[iterar] <- paste("Hurto a Entidades Comerciales 201",as.character(iterar-1),sep = "")
}

# HURTO A ENTIDADES FINANCIERAS
iterar = 0
hurto_entidades_financieras <- list()
for (c in (14:22)){
  iterar = iterar + 1
  hurto_entidades_financieras[iterar] <- lista_df[c]
  
  if (iterar >= 3){
    names(hurto_entidades_financieras)[iterar] <- paste("Hurto a Entidades Financieras 201",as.character(iterar),sep = "")
  } else {
    names(hurto_entidades_financieras)[iterar] <- paste("Hurto a Entidades Financieras 201",as.character(iterar - 1),sep = "")
  }
}

# En este caso en particular los años de Hurto a Entidades Financieras no estaba por años siguientes como las variables anteriores
# Por eso, para remedir esta situacion hice uso de las condicionales IF-ELSE para que dentro de mi loop
# Nombre los años diferentes segun las condiciones establecidas

# HURTO DE AUTOMORES
iterar = 0
hurto_automores <- list()
for (d in (30:39)){
  iterar = iterar + 1
  hurto_automores[iterar] <- lista_df[d]
  names(hurto_automores)[iterar] <- paste("Hurto de Automores 201",as.character(iterar - 1),sep = "")
}

# HURTO DE MOTOCICLETAS
iterar = 0
hurto_motocicletas <- list()
for (e in c((54:60),40)) {
  iterar = iterar + 1
  hurto_motocicletas[iterar] <- lista_df[e]
  if (iterar == 1){
    names(hurto_motocicletas)[iterar] <- paste("Hurto de Motocicletas 201",as.character(iterar - 1),sep = "")
  } else if ((iterar <= 3) & (iterar > 1)){ 
    names(hurto_motocicletas)[iterar] <- paste("Hurto de Motocicletas 201",as.character(iterar),sep = "")
  }else{
    names(hurto_motocicletas)[iterar] <- paste("Hurto de Motocicletas 201",as.character(iterar + 1),sep = "")
  }
}

# En este caso Hurto de Motocicletas tiene una situacion similiar a Hurtos Financieros donde se saltan años
# Por eso, para remedir esta situacion hice uso de las condicionales IF-ELSEI-ELSE para que dentro de mi loop
# Se nombraran correctamente los años segun la lista original, lista_df

# HURTO DE PERSONAS
iterar = 0
iterar2 = 0
iterar3 = 0
hurtos_personas <- list()
for (f in (41:53)) {
  iterar = iterar + 1
  hurtos_personas[iterar] <- lista_df[f]
  if (iterar >= 8){
    iterar2 = iterar2 + ifelse((iterar %% 2) == 0,1,0)
    iterar3 = 8 
    iterar3 = iterar3 + iterar2
    names(hurtos_personas)[iterar] <- paste("Hurto de Personas 201",as.character(iterar3 - 2),ifelse((iterar %% 2) == 0,"-1","-2"), sep = "")
  } else {
    names(hurtos_personas)[iterar] <- paste("Hurto de Personas 201",as.character(iterar - 1),sep = "")}}

# LESIONES PERSONALES
iterar = 0
iterar2 = 0
iterar = 0
lesiones_personales <-list()
for (g in c(61:74)){
  iterar = iterar + 1
  lesiones_personales[iterar] <- lista_df[g]
  if (iterar <= 6){
    iterar2 = iterar2 + ifelse((iterar %% 2) == 0,0,1)
    iterar3 = 1
    iterar3 = iterar3 + iterar2
    names(lesiones_personales)[iterar] <- paste("Lesiones Personales 201",as.character(iterar3 - 2),ifelse((iterar %% 2) == 0,"-2","-1"), sep = "")
  } else if (iterar == 7){
    names(lesiones_personales)[iterar] <- paste("Lesiones Personales 201",as.character(iterar3 - 2),"-3",sep = "")
  } else {
    names(lesiones_personales)[iterar] <- paste("Lesiones Personales 201",as.character(iterar - 5),sep = "")
  }
}

# Lesiones Personales & Hurto de Personas tienen años que cuentan con dos o mas periodos de recoleccion de informacion
# Por eso, para remedir esta situacion hice uso de las condicionales IF-ELSEI-ELSE para que dentro de mi loop
# Se nombraran correctamente los años segun la lista original, lista_df
# Ademas, use ifelse para que me nombrara los periodos 1 y 2 de los años correctamente

# Ahora dentro de una sola variable llamada tipo_delito divide los delitos por categorias
# Dentro de cada variable de delito por ejemplo "Homicidios, Lesiones personales, etc" 
# Ingrese los elementos categorizados por años 
# Por ejemplo, dentro de homiciodios se va a ver "Homicidios 2010, Homicidios 2011, etc" 

tipo_delito <- list()
tipo_delito[1] <- list(homicidios)
names(tipo_delito)[1] <- "Homicidios"
tipo_delito[2] <- list(hurto_entidades_comerciales)
names(tipo_delito)[2] <- "Hurto de Entidades Comerciales"
tipo_delito[3] <- list(hurto_entidades_financieras)
names(tipo_delito)[3] <- "Hurto de Entidades Financieras"
tipo_delito[4] <- list(hurto_automores)
names(tipo_delito)[4] <- "Hurto de Automores"
tipo_delito[5] <- list(hurto_motocicletas)
names(tipo_delito)[5] <- "Hurto de Motocicletas"
tipo_delito[6] <- list(hurtos_personas)
names(tipo_delito)[6] <- "Hurto de Personas"
tipo_delito[7] <- list(lesiones_personales)
names(tipo_delito)[7] <- "Lesiones Personales"

# 1.3. Se usa la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
# de la lista. Rbindlist tiene como objetivo unir las listas dentro de un dataframe singular
# Le pedi que unificara mi lista, lista_df, a base de los nombres de las columnas
# Tambien le pedi que me llenara los elementos vacios con NA para tener un dataframe uniforme

df <- rbindlist(lista_df, use.names = TRUE, idcol = TRUE, fill = TRUE)

# Punto 2 - Familia apply 

# 2.1 - Use la función lapply para pintar sobre la consola una tabla de frecuencia para cada variable del objeto df.

tabla_frecuencias = list()
for (m in (1:31)){
  tabla_frecuencias[[m]] <- data.frame(lapply(df[,..m],table))
  colnames(tabla_frecuencias[[m]]) <- c("Variables","Frecuencia")
  names(tabla_frecuencias)[[m]] <- colnames(df[1,..m])
}


                                          



