 
# TASK 2 #
 ### Taller A ###

# Punto 1 - Loops
# 1.0. Para realizar este punto debe importar el archivo data/output/lista.rds en un objeto llamado lista_df.
  # Esta lista contiene 74 dataframes con los de delitos cometidos en Colombia de 2010 a 2019.

lista_df = readRDS(file = "data/input/lista.rds")

# 1.1. Use un loop para eliminar las filas con NA y renombrar las columnas de cada elemento de la lista con
  # los nombres que se encuentran en una de las filas del dataframe. Además cambie todos los nombres de las
    # variables a minúsculas.

# En estas dos dataframes que tenemos una de las columnas esta vacia
  # El problema surge cuando eliminemos las filas de las listas que no tengan NA
   # Se eliminaria la fila de los titulos tambien en estos dos casos por eso se va a convertir
     # Esa fila NA en un caracter para poder preservar los titulos de estas dos dataframes
lista_df$`lesiones-personales-2012-2`[8,20] <- as.character(lista_df$`lesiones-personales-2012-2`[8,20])
lista_df$`lesiones-personales-2012-3`[8,20] <- as.character(lista_df$`lesiones-personales-2012-3`[8,20])

# Ahora voy a utilizar un loop For In que me evalue todos los elementos de la lista_df 
  # Y ejecute las condiciones que quiero como eliminar las filas con NA
   # fijar los titulos de columnas y modificar el titulo a que sean minisculas
                                                              
for (i in (1:74)){
  lista_df[[i]] <- na.omit(lista_df[[i]])
  colnames(lista_df[[i]]) <- tolower(chartr("áéíóú","aeiou",lista_df[[i]][1,]))
  colnames(lista_df[[i]]) <- gsub("profesiones","profesion",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("pais nace","pais de nacimiento",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase sitio","clase de sitio",colnames(lista_df[[i]]))
  colnames(lista_df[[i]]) <- gsub("clase empleado","clase de empleado",colnames(lista_df[[i]]))
  lista_df[[i]] <- lista_df[[i]][-1,]
}

# Adicionalmente voy a utilizar gsub para permitirme homogenizar los titulos de las diferentes listas
  # Para garantizarme una mejor orden cuando lo convierta en dataframe en el punto 1.3


# 1.2. Asegúrese de crear una variable tipo_delito que almacene el tipo de delito.
 

# El punto quiere que se dividen los elementos de la lista por el tipo de delito
  # La forma mas eficaz fue crear un loop for in que me haga el trabajo de agregar por categorias
   # A las listas que le indicara

# Adicionalmente para garantizar una mejor organizacion
  # Con ayuda de los loops pude nombrar los delitos con su respectivo nombre y año

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

# En este caso en particular los años de Hurto a Entidades Financieras no estaba por años siguientes como las variables anteriores
  # Por eso, para remedir esta situacion hice uso de las condicionales IF-ELSE para que dentro de mi loop
   # Nombre los años diferentes segun las condiciones establecidas

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

# En este caso Hurto de Motocicletas tiene una situacion similiar a Hurtos Financieros donde se saltan años
  # Por eso, para remedir esta situacion hice uso de las condicionales IF-ELSEI-ELSE para que dentro de mi loop
    # Se nombraran correctamente los años segun la lista original, lista_df

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

# 1.3. Use la función rbindlist de la librería data.table para crear un dataframe que contenga todos los elementos
  # de la lista. Asegúrese de llamar a este objeto df.

# Rbindlist tiene como objetivo unir las listas dentro de un dataframe singular
  # Le pedi que unificara mi lista, lista_df, a base de los nombres de las columnas
    # Tambien le pedi que me llenara los elementos vacios con NA para tener un dataframe
      # Uniforme

df <- rbindlist(lista_df, use.names = TRUE, idcol = TRUE, fill = TRUE)
