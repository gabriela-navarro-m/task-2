 
# TASK 2 #
 ### Taller A ###

# Punto 1 - Loops
# 1.0. Para realizar este punto debe importar el archivo data/output/lista.rds en un objeto llamado lista_df.
  # Esta lista contiene 74 dataframes con los de delitos cometidos en Colombia de 2010 a 2019.

lista_df = readRDS(file = "data/input/lista.rds")

# 1.1. Use un loop para eliminar las filas con NA y renombrar las columnas de cada elemento de la lista con
  # los nombres que se encuentran en una de las filas del dataframe. Además cambie todos los nombres de las
    # variables a minúsculas.

for (i in (1:74)){
  lista_df[[i]] <- na.omit(lista_df[[i]])
  colnames(lista_df[[i]]) <- tolower(lista_df[[i]][1,])
  lista_df[[i]] <- lista_df[[i]][-1,]
}

# 1.2. Asegúrese de crear una variable tipo_delito que almacene el tipo de delito.
delito_homicidios = list(c(lista_df[[1:10]]))
tipo_delito <- list(lista_df[[1:10]],lista_df[[11:13]],lista_df[[14:22]],lista_df[[22:29]],lista_df[[29:39]],lista_df[[40]],lista_df[41:48])
tipo_delito <- list("")

# 1.3. Use la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
  # de la lista. Asegúrese de llamar a este objeto df.

  
