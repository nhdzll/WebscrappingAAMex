#Librerias que vamos a utilizar
library(tidyverse)
library(polite)
library(rvest)

PaginasTotales <- function(paginaweb, atributo = 'b'){
  #Esta función nos permite obtener el número de grupos de tratamiento
  #en el estado id (almacenados en el tag <atributo>)
  
  #Primero obtenemos todos los elementos de HTML con el atributo definido y
  #los convertimos a texto
  elemento <- paginaweb %>% html_elements(atributo) %>% html_text()
  #Obtenemos la posición y longitud del número de establecimientos dentro del
  #elemento HTML obtenido
  posn <- gregexpr('[0-9]+',elemento)
  inicio <- as.numeric(unlist(posn))
  fin <- inicio + as.numeric(attr(posn[[1]], 'match.length'))
  n <- str_sub(attrb, inicio, fin)
  #DEvolvemos el número total de establecimientos en formato numérico
  return(as.numeric(n))
}


GuardarInfo <- function(info, id){
  con <- file(paste("dirID",id,"_",as.POSIXlt(Sys.Date(), format = "%Y%m%d"), ".csv", sep=""), "at")
  write.table(info, con, sep = ",", col.names=FALSE)
  close(con) 
}

setwd("C:/Users/inp.mx/Documents/Tavarich/2022/Web Scrapping/AA")
#Variables de entorno: id(1-32) se refiere a los estados de la república 
#y pagina(1-n) se refiere a la pagina dentro de cada estado y es variable.

url <- "https://aamexico.org.mx/directorio_grupos_estado.php?"

#Abrir consulta por estado
for (id in 1:32){
  #Abrimos la sesión
  sesion <- bow(paste(url,"id=",id,"&pagina=1", sep=""), user_agent = "@nfhdzll")
  #Obtenemos la información de la tabla en formato xml_nodeset
  webpage <- scrape(sesion) %>% html_nodes('.col-12')
  #extraemos el número de establecimientos
  n <- PaginasTotales(webpage)
  #Calculamos el número de iteraciones dentro del estado (cada página contiene 25 registros
  pagtot <- ceiling(n/25)
  for (pagina in 1:pagtot){
    #Abrimos la sesión en la pagina id y pagina
    sesion <- bow(paste(url,"id=",id,"&pagina=", pagina, sep=""), user_agent = "@nfhdzll")
    #Obtenemos la información de la tabla en formato xml_nodeset
    webpage <- scrape(sesion) %>% html_nodes('.col-12')
    #Obtenemos la tabla con los datos de los centros
    tabla <- webpage %>% html_table() %>% flatten_df()
    tabla$id <- id
    tabla$pagina <- pagina
    GuardarInfo(tabla, id)
  }
}



#elemento <- webpage %>% html_elements("a") %>% html_attrs()

