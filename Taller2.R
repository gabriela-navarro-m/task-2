# =========================================================== #
# Elaborado por: Gabriela Navarro (201821568)                 #
#                Daniela Quintero (201821754)                 #
#                Maria Alejandra Saavedra (201815221)         #                                                               
# Fecha de elaboración: 23/04/2021                            #
# Ultima modificación: 05/05/2021                             #
# =========================================================== #


### TASK 2 - TALLER A ###

# Configuracion inicial

rm(list = ls()) #Limpia el entorno de R
pacman::p_load(here,tidyverse,reshape2, data.table, skimr,base) #Cargar y/o instalar paquetes requeridos

# Punto 1 - Loops

# 1.0. 

# Se importa el archivo lista.rds con readRDS por el formato del archivo. 

lista_df = readRDS(file = "data/input/lista.rds")
lista_df

# 1.1. 

# Se utiliza un 'loop for in' que evalue todos los elementos de la lista_df y ejecute las condiciones que se requieren, como eliminar las filas con NA,
# fijar los titulos de columnas y modificar el titulo (ponerlo en minusculas).

# Adicionalmente se utiliza 'gsub' para homogenizar los titulos de las diferentes listas y garantizar un mejor orden cuando se convierta en dataframe en el punto 1.3

for (i in (1:length(lista_df))){
  lista_df[[i]] <- lista_df[[i]] %>% subset(is.na(...2) == F) %>% data.frame(stringsAsFactors = F)
  colnames(lista_df[[i]]) <- tolower(chartr("áéíóú","aeiou",lista_df[[i]][1,]))
  colnames(lista_df[[i]]) <- gsub("profesiones","profesion",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("pais nace","pais de nacimiento",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase sitio","clase de sitio",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase empleado","clase de empleado",colnames(lista_df[[i]]))
  lista_df[[i]] <- lista_df[[i]][-1,]
}

# 1.2. 

# Se crea una variable llamada 'tipo_delito' que almacena, como lo dice su nombre, el tipo de delito. 
# Para ello, se solicitan los nombres del contenido del objeto en lista_df y se hace uso de 'mutate' para crear la variable
# a partir de variables ya existentes. 

for (i in (1:length(lista_df))){
  lista_df[[i]] <- mutate(lista_df[[i]],tipo_delito = names(lista_df[i]))
}

# 1.3. 

# Se usa la funcion rbindlist de la libreria data.table para crear un dataframe que contenga todos los elementos
# de la lista. Rbindlist tiene como objetivo unir las listas dentro de un dataframe singular
# Le pedi que unificara mi lista, lista_df, a base de los nombres de las columnas
# Tambien le pedi que me llenara los elementos vacios con NA para tener un dataframe uniforme

df <- rbindlist(lista_df, use.names = TRUE,fill = TRUE)

#Para exportarla a output
saveRDS(object = df, file = "data/output/df.rds")

# Punto 2 - Familia apply 

# 2.1 

# Se pinta en la consola una tabla de frecuencia para cada variable del objeto df. 

skimr::skim(df) # Se usa esta funcion para tener un resumen de las variables con informacion estadistica separadas entre variables de caracteres y numericas
lapply(df,function(x) table(x) %>% sort(decreasing = T) %>% head(10)) # Otra opcion es el mismo lapply donde se usa la funcion table para la tabla de frecuencia pero al tener tanta informacion se organiza de forma decreciente y se filtra para que queden solo los 10 valores mas comunes de cada variable y su cantidad de veces 


# Punto 3 

#3.1

# Para probar la funcion se usa un vector con los nombres en mayuscula

vector=c('MARIA', 'DANIELA', 'ALEJANDRA', 'GABRIELA')

#la funcion se hace basada en un elemento del vector o dataframe a analizar
#por lo que se busca que solo corra para las variables que son de forma de caracter porque no se puede pasar a minuscula un numero.
#se usa la funcion preestablecida tolower para pasar de mayuscula a minuscula el elemento a analizar que cumpla la condicion de ser caracter

f_min <- function(elemento){ 
  minuscula = elemento
  if (is.character(elemento) == T) {
    minuscula = tolower(elemento) 
  }
  return(minuscula) #retornara el elemento que se requiere en minuscula 
}

f_min(vector)

#3.2

# Se usa lapply para coger todas las variables del dataframe df y se les aplica la funcion de f_min creada anteriormente para pasar todos los elementos de caracteres a minuscula.

df2 = lapply(df,function(x) f_min(x)) 

#Para exportarla a output
saveRDS(object = df2, file = "data/output/df_minuscula.rds")






