 
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
 # Para no tener que agregar todo individualmente cree un loop que me agregre a una variables las
  # Listas que quiero
# Voy a dividir los delitos por variables y despues lo agrego en una sola variable

# HOMICIDIOS
homicidios <- list()
for (a in (1:10)){
  homicidios[a] <- list(lista_df[a])
}

# HURTO A ENTIDADES COMERCIALES
numero = 0
hurto_entidades_comerciales <- list()
for (b in c((11:13),(23:29))){
  numero = numero + 1
  hurto_entidades_comerciales[numero] <- list(lista_df[b])
}

# HURTO A ENTIDADES FINANCIERAS
numero = 0
hurto_entidades_financieras <- list()
for (c in (14:22)){
  numero = numero + 1
  hurto_entidades_financieras[numero] <- list(lista_df[c])
}

# DELITO AUTOMORES
numero = 0
delito_automores <- list()
for (d in (30:39)){
  numero = numero + 1
  delito_automores[numero] <- list(lista_df[d])
}

# DELITO MOTOCICLETAS
numero = 0
delito_motocicletas <- list()
for (e in c((54:60),40)){
  numero = numero + 1
  delito_motocicletas[numero] <- list(lista_df[e])
}

# HURTO DE PERSONAS
numero = 0
hurtos_personas <- list()
for (f in (41:53)){
  numero = numero + 1
  hurtos_personas[numero] <- list(lista_df[f])
}

# LESIONES PERSONALES
numero = 0
lesiones_personales <-list()
for (g in c(61:74)){
  numero = numero + 1
  hurtos_personas[numero] <- list(lista_df[g])
}

tipo_delito <- list()
tipo_delito[[1]] <- homicidios
tipo_delito[[2]] <- hurto_entidades_comerciales
tipo_delito[[3]] <- hurto_entidades_comerciales
tipo_delito[[4]] <- delito_automores
tipo_delito[[5]] <- delito_motocicletas
tipo_delito[[6]] <- hurtos_personas
tipo_delito[[7]] <- lesiones_personales

# 1.3. Use la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
  # de la lista. Asegúrese de llamar a este objeto df.




  
