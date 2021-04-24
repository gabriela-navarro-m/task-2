# Elaborado por: Gabriela Navarro (201821568), Daniela Quintero y Maria Alejandra Saavedra (201815221)
# Fecha de elaboración: 23/04/2021
# Ultima modificación:

### Taller A ###

# Configuración inicial
rm(list = ls()) #Limpia el entorno de R
pacman::p_load(here,tidyverse,reshape2) #Cargar y/o instalar paquetes requeridos

# Punto 1 - Loops

  # 1.0. Se importa el archivo lista.rds con readRDS por el formato del archivo. 

  lista_df = readRDS(file = "data/input/lista.rds")
  lista_df

  # 1.1. 

  # Existe un error con dos dataframes donde se borro un titulo. Para evitar que se elimine la fila se arregla antes del loop
  
  lista_df$`lesiones-personales-2012-2`[8,20] <- is.character(lista_df$`lesiones-personales-2012-2`[8,20])
  lista_df$`lesiones-personales-2012-3`[8,20] <- is.character(lista_df$`lesiones-personales-2012-3`[8,20])
                                                              
  for (i in (1:74)){
    lista_df[[i]] <- na.omit(lista_df[[i]])
    colnames(lista_df[[i]]) <- tolower(chartr("áéíóú","aeiou",lista_df[[i]][1,]))
    colnames(lista_df[[i]]) <- gsub("profesiones","profesion",colnames(lista_df[[i]]))
    colnames(lista_df[[i]]) <- gsub("pais nace","pais de nacimiento",colnames(lista_df[[i]]))
    colnames(lista_df[[i]]) <- gsub("clase sitio","clase de sitio",colnames(lista_df[[i]]))
    colnames(lista_df[[i]]) <- gsub("clase empleado","clase de empleado",colnames(lista_df[[i]]))
    lista_df[[i]] <- lista_df[[i]][-1,]
    } # Se realizo el loop para coger las 74 bases de datos y seleccionar la fila 9 como los títulos de las columnas y pasarlas a minuscula

  # 1.2. 
  
  # Para no tener que agregar todo individualmente se crea un loop que agrega a una variables las listas que se requieren 

  # HOMICIDIOS
  homicidios <- list()
  for (a in (1:10)){
    homicidios[a] <- lista_df[a]
    names(homicidios)[a] <- paste("Homicidios 201",as.character(a-1),sep = "")
    }

  # HURTO A ENTIDADES COMERCIALES
  hurto_entidades_comerciales <- list()
  iterar1 = 0
  for (b in c((11:13),(23:29))){
    iterar1 = iterar1 + 1
    hurto_entidades_comerciales[iterar1] <- lista_df[b]
    names(hurto_entidades_comerciales)[iterar1] <- paste("Hurto a Entidades Comerciales 201",as.character(iterar1-1),sep = "")
    }

  # HURTO A ENTIDADES FINANCIERAS
  iterar2 = 0
  hurto_entidades_financieras <- list()
  for (c in (14:22)){
    iterar2 = iterar2 + 1
    hurto_entidades_financieras[iterar2] <- lista_df[c]
  if (iterar2 >= 3){
    names(hurto_entidades_financieras)[iterar2] <- paste("Hurto a Entidades Financieras 201",as.character(iterar2),sep = "")
  } else {
    names(hurto_entidades_financieras)[iterar2] <- paste("Hurto a Entidades Financieras 201",as.character(iterar2 - 1),sep = "")
    }
  }

  # HURTO DE AUTOMORES
  iterar3 = 0
  hurto_automores <- list()
  for (d in (30:39)){
   iterar3 = iterar3 + 1
   hurto_automores[iterar3] <- lista_df[d]
   names(hurto_automores)[iterar3] <- paste("Hurto de Automores 201",as.character(iterar3 - 1),sep = "")
  }

  # HURTO DE MOTOCICLETAS
  iterar4 = 0
  hurto_motocicletas <- list()
  for (e in c((54:60),40)){
   iterar4 = iterar4 + 1
   hurto_motocicletas[iterar4] <- lista_df[e]
   if (iterar4 == 1){
     names(hurto_motocicletas)[iterar4] <- paste("Hurto de Motocicletas 201",as.character(iterar4 - 1),sep = "")
   } else if ((iterar4 <= 3) & (iterar4 > 1)){ 
     names(hurto_motocicletas)[iterar4] <- paste("Hurto de Motocicletas 201",as.character(iterar4),sep = "")
   }else{
    names(hurto_motocicletas)[iterar4] <- paste("Hurto de Motocicletas 201",as.character(iterar4 + 1),sep = "")
      }
    }

  # HURTO DE PERSONAS
  iterar5 = 0
  iterar6 = 0
  hurtos_personas <- list()
  for (f in (41:53)){
    iterar5 = iterar5 + 1
    hurtos_personas[iterar5] <- lista_df[f]
    if (iterar5 >= 8){
     iterar6 = iterar6 + ifelse((iterar5 %% 2) == 0,1,0)
      iterar7 = 8 
      iterar7 = iterar7 + iterar6
      names(hurtos_personas)[iterar5] <- paste("Hurto de Personas 201",as.character(iterar7 - 2),ifelse((iterar5 %% 2) == 0,"-1","-2"), sep = "")
   } else {
      names(hurtos_personas)[iterar5] <- paste("Hurto de Personas 201",as.character(iterar5 - 1),sep = "")
   }
  }

  # LESIONES PERSONALES
  iterar7 = 0
  iterar8 = 0
  iterar9 = 0
  lesiones_personales <-list()
  for (g in c(61:74)){
    iterar7 = iterar7 + 1
   lesiones_personales[iterar7] <- lista_df[g]
    if (iterar7 <= 6){
     iterar8 = iterar8 + ifelse((iterar7 %% 2) == 0,0,1)
     iterar9 = 1
     iterar9 = iterar9 + iterar8
     names(lesiones_personales)[iterar7] <- paste("Lesiones Personales 201",as.character(iterar9 - 2),ifelse((iterar7 %% 2) == 0,"-2","-1"), sep = "")
   } else if (iterar7 == 7){
      names(lesiones_personales)[iterar7] <- paste("Lesiones Personales 201",as.character(iterar9 - 2),"-3",sep = "")
    } else {
     names(lesiones_personales)[iterar7] <- paste("Lesiones Personales 201",as.character(iterar7 - 5),sep = "")
   }
  }

# Voy a dividir los delitos por variables y despues lo agrego en una sola variable

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

# 1.3. Use la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
  # de la lista. Asegúrese de llamar a este objeto df.

df <- rbindlist(lista_df, use.names = TRUE, idcol = TRUE, fill = TRUE)
