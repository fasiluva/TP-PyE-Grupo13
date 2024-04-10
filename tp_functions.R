moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

RI <- function(x) {
  x <- x[order(x)]
  cuartil1_index <- (length(x) + 1) / 4
  cuartil3_index <- cuartil1_index * 3
  
  if (cuartil1_index != round(cuartil1_index)) {
    cuartil1 <- (x[cuartil1_index] + x[cuartil1_index + 1]) / 2
  } else {
    cuartil1 <- x[cuartil1_index] 
  }
  if (cuartil3_index != round(cuartil3_index)) {
    cuartil3 <- (x[cuartil3_index] + x[cuartil3_index + 1]) / 2
  } else {
    cuartil3 <- x[cuartil3_index] 
  }
  
  return(cuartil3 - cuartil1)
}

cuartiles <- function(x) {
  cuartiles <- quantile(x, probs=c(0.25, 0.50, 0.75))
  return(cuartiles)
}

percentiles <- function(x, division) {
  percentiles <- quantile(x, probs= division / 100)
  return(percentiles)
}

rango <- function(x) {
  return(max(x) - min(x))
}

media <- function(x) {
  return(round(mean(x), 3))
}

mediana <- function(x) {
  return(median(x))
}

variacion <- function(x) {
  return(round(var(x), 3))
}

desvio <- function(x) {
  return(round(sd(x), 3))
}

relevantMetrics <- function(x) {
  minimo <- min(x)
  maximo <- max(x)
  media <- round(mean(x), 2)
  mediana <- median(x)
  moda <- Moda(x)
  variacion <- round(var(x), 2)
  desvio <- round(sd(x), 2)
  
  res <- c(min=minimo, 
           max=maximo, 
           media=media, 
           mediana=mediana, 
           moda=moda, 
           var=variacion, 
           desvio=desvio)
  
  print(paste("Min:", minimo))
  print(paste("Max:", maximo))
  print(paste("Media aritmetica:", media))
  print(paste("Mediana:", mediana))
  print(paste("Moda:", moda))
  print(paste("Variacion:", variacion))
  print(paste("Desvio estandar:", desvio))
  
  # Boxplot.
  boxplot(x, ylab="Values", main="Boxplot de la variable")
  grid()
  
  readline()
  par(mfrow = c(1, 1))
  
  return(res)
}

completaVentilacion <- function(datos) {
  
  for (i in 1:length(datos$C43)) {
    if (grepl("Gas", datos$C43[i]) || grepl("Leña", datos$C43[i])) {
       if (is.na(datos$C49[i]) || grepl(datos$C49[i], "No")) {
         datos$C49[i] <- "No"
       }
      else {
        datos$C49[i] <- "Si"
      }
    }
    else {
      datos$C49[i] <- "No aplica"
    }
  }
  
  return(datos)
}

calculaFuentesCalefaccion <- function(datos) {
  
  gasNatural <- 0
  gasEnvasado <- 0
  electricidad <- 0
  leniaCarbon <- 0
  
  for (i in 1:length(datos)) {
    if (grepl("Gas natural", datos[i])){
      gasNatural <- gasNatural + 1
    }
    
    if (grepl("Gas envasado", datos[i])) {
      gasEnvasado <- gasEnvasado + 1
    }
    
    if (grepl("Electricidad", datos[i])) {
      electricidad <- electricidad + 1
    }
    
    if (grepl("Leña", datos[i])) {
      leniaCarbon <- leniaCarbon + 1
    }
  
  }
  
  return(c(gasNatural, gasEnvasado, leniaCarbon, electricidad))
}

eliminaNA <- function(datos) {
  
  res <- c()
  
  for (i in 1:length(datos)) {
    if (is.na(datos[i]))
      next
    else {
      res <- c(res, datos[i])
    }
  }
  
  return(res)
}

tieneConectividad <- function(servicio, datos) {
  
  si <- 0
  no <- 0
  
  for (i in 1:length(servicio)) {
    if (grepl("Si", servicio[i]) || grepl("Sí", servicio[i])) {
      si <- si + 1
    }
    else {
      if (grepl("Sí", datos[i])) {
        si <- si + 1
      }
      else {
        no <- no + 1
      }
    }
  }
  
  return(c(si, no))
}

calculaFiltraciones <- function(datos) {
  
  banio <- 0
  cocina <- 0
  living <- 0
  dormitorios <- 0
  otro <- 0
  ninguno <- 0
  
  for (i in 1:length(datos)) {
    if (grepl("Dormitorios", datos[i])){
      dormitorios <- dormitorios + 1
    }
    
    if (grepl("Cocina", datos[i])) {
      cocina <- cocina + 1
    }
    
    if (grepl("Baño", datos[i])) {
      banio <- banio + 1
    }
    
    if (grepl("Living", datos[i])) {
      living <- living + 1
    }
    
    if (grepl("Otro", datos[i])) {
      otro <- otro + 1
    }
    
    if (grepl("No hay", datos[i])) {
      ninguno <- ninguno + 1
    }
  }
  
  return(c("Baño" = banio, 
         "Cocina" = cocina, 
         "Living" = living, 
         "Otro" = otro, 
         "Dormitorios" = dormitorios,
         "Ninguna filtración" = ninguno))
}

calculaRiesgoDerrumbe <- function(datos) {
  
  si <- 0
  no <- 0
  
  for (i in 1:length(datos)) {
    if (grepl("No hay", datos[i])) {
      no <- no + 1
    }
    else {
      si <- si + 1
    }
  }
  
  return(c("Con riesgo" = si, "Sin riesgo" = no))
}

calculaPlagas <- function(datos) {
  
  cucarachas <- 0
  ratas <- 0
  mosquitos <- 0
  nada <- 0
  
  for (i in 1:length(datos)) {
    if (datos[i] == "") {
      nada <- nada + 1
    }
    if (grepl("Ratas", datos[i])) {
      ratas <- ratas + 1
    }
    
    if (grepl("Mosquitos", datos[i])) {
      mosquitos <- mosquitos + 1
    }
    
    if (grepl("Cucarachas", datos[i])) {
      cucarachas <- cucarachas + 1
    }
    
  }
  
  return(c("Cucarachas" = cucarachas, "Mosquitos" = mosquitos, 
           "Ratas" = ratas, "Ninguna" = nada))
}

tendido_a_numeric <- function(x) {
  res <- c()
  
  for (i in 1:length(x)) {
    if (grepl("Todo", x[i])) {
      res <- c(res, 2)
    }
    else {
      res <- c(res, 1)
    }
  }
  
  return(res)
}

incendio_a_numeric <- function(x) {
  res <- c()
  
  for (i in 1:length(x)) {
    if (grepl("Si", x[i])) {
      res <- c(res, 2)
    }
    else {
      res <- c(res, 1)
    }
  }
  
  return(res)
}

riesgo_a_numeric <- function(x) {
  res <- c()
  
  for (i in 1:length(x)) {
    if (grepl("Con", x[i])) {
      res <- c(res, 1)
    }
    else {
      res <- c(res, 2)
    }
  }
  
  return(res)
}

presion_a_numeric <- function(x) {
  
  res <- c()
  
  for (i in 1:length(x)) {
    if (grepl("Buena", x[i])) {
      res <- c(res, 3)
    }
    else if (grepl("Muy", x[i])) {
      res <- c(res, 1)
    }
    else {
      res <- c(res, 2)
    }
  }
  
  return(res)
  
}

