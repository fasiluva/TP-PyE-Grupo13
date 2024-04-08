Moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

RI <- function(x) {
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
  
  # Doble grafico
  #par(mfrow = c(1, 2))
  
  # Grafico de dispersion.
  #plot(x, ylab="Values", main="Grafico de dispersion")
  
  # Boxplot.
  boxplot(x, ylab="Values", main="Boxplot")
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


