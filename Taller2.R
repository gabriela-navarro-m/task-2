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

# 1.1. 

# Se utiliza un loop For In que evalue todos los elementos de la lista_df y ejecute las condiciones que quiero, como eliminar las filas con NA,
# fijar los titulos de columnas y modificar el titulo (ponerlo en minusculas)

# Adicionalmente se utiliza gsub para homogenizar los titulos de las diferentes listas y garantizar un mejor orden cuando se convierta en dataframe en el punto 1.3

for (i in (1:length(lista_df))){
  lista_df[[i]] <- lista_df[[i]] %>% subset(is.na(...2) == F) %>% data.frame(stringsAsFactors = F)
  colnames(lista_df[[i]]) <- tolower(chartr("áéíóú","aeiou",lista_df[[i]][1,]))
  colnames(lista_df[[i]]) <- gsub("profesiones","profesion",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("pais nace","pais de nacimiento",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase sitio","clase de sitio",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase empleado","clase de empleado",colnames(lista_df[[i]]))
  lista_df[[i]] <- lista_df[[i]][-1,]
}

# 1.2. Asegúrese de crear una variable tipo_delito que almacene el tipo de delito.

vector_names = names(lista_df)
for (i in (1:length(lista_df))){
  lista_df[[i]] <- mutate(lista_df[[i]],tipo_delito = vector_names)
}
# 1.3. Se usa la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
# de la lista. Rbindlist tiene como objetivo unir las listas dentro de un dataframe singular
# Le pedi que unificara mi lista, lista_df, a base de los nombres de las columnas
# Tambien le pedi que me llenara los elementos vacios con NA para tener un dataframe uniforme

df <- rbindlist(lista_df, use.names = TRUE,fill = TRUE)

# Punto 2 - Familia apply 

# 2.1 - Use la función lapply para pintar sobre la consola una tabla de frecuencia para cada variable del objeto df.

skimr::skim(df)
lapply(df,function(x) table(x) %>% sort(decreasing = T) %>% head(10))

#3
#3.1
vector=c('MARIA', 'DANIELA', 'ALEJANDRA', 'GABRIELA')
f_min <- function(elemento){
  if (is.character(elemento)==T) {
    minuscula= tolower(elemento)
  }
  return(minuscula)
}
f_min(vector)

#3.2
df2= for (j in df) {
  f_min(j)
}