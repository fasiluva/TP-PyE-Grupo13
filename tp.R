# Instala los paquetes necesarios
install.packages("readxl")
install.packages("tidyr")
library(readxl)
library(tidyr)

# Incluye el archivo de funciones
source("tp_functions.R")

# Lee el excel completo
datos_excel <- read_excel("Datos_LP.xlsx")

# Lee solo las preguntas
datos_excel_solo_preguntas <- datos_excel[2,]

# Lee solo las encuestas y modifica los nombres de las columnas
datos_excel_solo_resultados <- read_excel("Datos_LP.xlsx", range=cell_rows(c(4, NA)), col_names = FALSE)
colnames(datos_excel_solo_resultados) <- paste0("C", seq_len(ncol(datos_excel_solo_resultados)))

# Cambia las columnas que tienen valores inutiles:
datos_excel_solo_resultados$C17[is.na(datos_excel_solo_resultados$C17)] <- 0 
datos_excel_solo_resultados$C20[is.na(datos_excel_solo_resultados$C20)] <- "No"
datos_excel_solo_resultados$C22[is.na(datos_excel_solo_resultados$C22)] <- "Sin aumento"
datos_excel_solo_resultados$C23[is.na(datos_excel_solo_resultados$C23)] <- "Sin aumento"

for (i in seq_along(datos_excel_solo_resultados$C24)){
  if (grepl("informalmente", datos_excel_solo_resultados$C24[i])){ 
    datos_excel_solo_resultados$C24[i] <- "Conexion a vecino o red publica sin medidor"
  }
  
  if (grepl("cisterna", datos_excel_solo_resultados$C24[i])){ 
    datos_excel_solo_resultados$C24[i] <- "Camion cisterna"
  }
  
  if (grepl("A través de una conexión con medidor a la red pública", datos_excel_solo_resultados$C24[i])) {
    datos_excel_solo_resultados$C24[i] <- "Conexion a la red publica con medidor"
  }
  
  if (grepl("acarrear", datos_excel_solo_resultados$C24[i])) {
    datos_excel_solo_resultados$C24[i] <- "Acarreo desde afuera"
  }
}

datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C27", c("C27", "C28"), na.rm=TRUE, sep=", de ")
datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C38", c("C38", "C39", "C40", "C41", "C42"), na.rm=TRUE, sep=", ")
datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C43", c("C43", "C44", "C45", "C46", "C47", "C48"), na.rm=TRUE, sep=", ")

datos_excel_solo_resultados <- completaVentilacion(datos_excel_solo_resultados)

datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C73", c("C73", "C74", "C75", "C76", "C77", "C78"), na.rm=TRUE, sep=", ")
datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C79", c("C79", "C80", "C81", "C82", "C83", "C84"), na.rm=TRUE, sep=", ")
datos_excel_solo_resultados$C88[is.na(datos_excel_solo_resultados$C88)] <- "No"
datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C93", c("C93", "C94", "C95"), na.rm=TRUE, sep=", ")
datos_excel_solo_resultados <- unite(datos_excel_solo_resultados, col="C96", c("C96", "C97", "C98", "C99", "C100", "C101", "C102", "C103", "C104"), na.rm=TRUE, sep=", ")

colnames(datos_excel_solo_resultados) <- paste0("C", seq_len(ncol(datos_excel_solo_resultados)))
datos_excel_solo_resultados$C39[is.na(datos_excel_solo_resultados$C39)] <- "No"

# ************************* VARIABLES GLOBALES *************************

cant_viviendas <- length(datos_excel_solo_resultados[["C1"]])
paleta <- c("#1c0b2b", "#413b6b", "#5c65c0", "#6f95ff")
fuente <- "Fuente: La Poderosa. Encuesta realizado a barrios populares de Argentina."

# *********************************************************************
  
# Total de provincias
datos_provincias <- table(datos_excel_solo_resultados[[2]])
datos_provincias <- datos_provincias[order(datos_provincias, decreasing = TRUE)]

frec_rel_prov <- round(datos_provincias/cant_viviendas * 100, 3)
tabla_frec_prov <- t(rbind(datos_provincias, frec_rel_prov))
colnames(tabla_frec_prov) <- c("Encuestas", "Frecuencia relativa (en %)")
tabla_frec_prov <- rbind(tabla_frec_prov, c(1222, 100))
rownames(tabla_frec_prov)[17] <- "Total"

tabla_frec_prov

# Total de barrios
datos_barrios <- table(datos_excel_solo_resultados[[3]])
datos_barrios <- datos_barrios[order(datos_barrios, decreasing=TRUE)]

frec_rel_bar <-  round(datos_barrios/cant_viviendas * 100, 3)
tabla_frec_bar <- t(rbind(datos_barrios, frec_rel_bar))
colnames(tabla_frec_bar) <- c("Encuestas", "Frecuencia relativa (en %)")
tabla_frec_bar <- rbind(tabla_frec_bar, c(1222, 100))
rownames(tabla_frec_bar)[24] <- "Total"

tabla_frec_bar


# ================== Descripcion grafica de RENABAP e intento de desalojos.
# ================== Descripcion grafica de arbolado. Descripcion grafica de presion del agua
# ================== Descripcion grafica de fuentes de energia de la vivienda. 

# ================== Descripcion grafica de integrantes de la familia
# Metricas de Edad jefe del hogar 
relevantMetrics(datos_excel_solo_resultados[["C4"]])

# Grafico de torta de cantidad de personas segun genero.
cant_varones <- datos_excel_solo_resultados[["C8"]]
cant_mujeres <- datos_excel_solo_resultados[["C9"]]
cant_disidentes <- datos_excel_solo_resultados[["C10"]]

cant_total_personas <- c(sum(cant_varones), sum(cant_mujeres), sum(cant_disidentes))
options_pie <- c("Hombres", "Mujeres", "Disidentes")

pie(cant_total_personas, 
    labels=paste(options_pie, c("\n"), round(cant_total_personas/sum(cant_total_personas) * 100, 2), c("%")), 
    clockwise=TRUE,
    col=paleta, 
    radius=0.9, 
    main="Cantidad de personas según género\n Barrios populares de Argentina, año 2020.")
mtext("Fuente: La Poderosa. Encuesta realizado a barrios populares de Argentina.", 
      side=1, line=1, at=-0.6)

cant_menores <- datos_excel_solo_resultados[["C11"]]
cant_discapacidad <- datos_excel_solo_resultados[["C12"]]
cant_menores_total <- sum(cant_menores)
cant_discapacidad_total <- sum(cant_discapacidad)

print(paste("Hay un total de", 
            cant_menores_total, 
            "menores en las viviendas, representando el", 
            round(cant_menores_total/sum(cant_total_personas) * 100, 2),
            "% del total de personas."))

print(paste("Hay un total de", 
            cant_discapacidad_total, 
            "personas con alguna discapacidad en las viviendas, representando el", 
            round(cant_discapacidad_total/sum(cant_total_personas) * 100, 2),
            "% del total de personas."))

# ================== Descripcion grafica de obtencion de agua y saneamiento.

# Como obtiene el agua?
par(las=1, cex.lab=1.2, mar=c(5,10,5,1))
cant_obtencion_agua <- table(datos_excel_solo_resultados[["C24"]])
barplot(height=cant_obtencion_agua[order(cant_obtencion_agua, decreasing=TRUE)],
        horiz=TRUE,
        xlim=c(0,700),
        col=paleta[2],
        main="Métodos de obtención del agua de las viviendas.\nBarrios populares de Argentina, 2020."
        )
abline(v=seq(0,700,50), col="grey", lty="dotted",)
mtext("Fuente: La Poderosa. Encuesta realizado a barrios populares de Argentina.", 
      side=1, line=3, at=100)

dev.off()
cant_presion_agua <- table(datos_excel_solo_resultados[["C26"]])
cant_presion_agua_totales <- c(cant_presion_agua[[1]], cant_presion_agua[[2]], cant_presion_agua[[3]])


pie(cant_presion_agua, 
    col= paleta,
    labels=paste(c("Buena", "Débil", "Muy débil"), c("\n"), 
                 round(cant_presion_agua_totales/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=0.9,
    main="Calidad de la presión de agua de las viviendas.\nBarrios populares de Argentina, 2020."
    )
mtext(fuente, side=1, line=1, at=-0.2)
tabla_presion_almacenamiento <- table(datos_excel_solo_resultados$C27[datos_excel_solo_resultados$C27 != "Sí"], 
                                      datos_excel_solo_resultados$C26[datos_excel_solo_resultados$C27 != "Sí"])

barplot(tabla_presion_almacenamiento, 
        beside=TRUE,
        horiz=TRUE,
        col=paleta,
        xlim=c(0,300),
        main="Presencia de capacidad de almacenamiento de agua en altura respecto a la presión del agua.\nBarrios populares de Argentina, 2020."
        )
abline(v=seq(0,300,25), col="grey", lty="dotted",)
legend(x =170, y = 15, 
       legend=c("No", "Si, entre 200 a 500 lts", "Si, de mas de 500 lts", "Si, de menos de 200 lts"), 
       fill = paleta)
mtext(fuente, side=1, line=3, at=100)

dev.off()

# ================== Descripcion grafica de cocina y calefaccion.

cant_fuentes_calefaccion <- datos_excel_solo_resultados[["C38"]]
fuentes <- calculaFuentesCalefaccion(cant_fuentes_calefaccion)
cant_ventilacion <- table(datos_excel_solo_resultados[["C39"]])


# ================== Descripcion grafica de alquiler.

par(mfrow = c(1, 2))

cant_propiedad <- table(datos_excel_solo_resultados[["C19"]])

barplot(cant_propiedad[order(cant_propiedad, decreasing=TRUE)], 
    col=paleta[3],
    ylim=c(0, 600), 
    main="Tipo de tenencia sobre la propiedad\nBarrios populares de Argentina, 2020.",
    ylab="Viviendas",
    xlab="Tenencia")
abline(h=seq(0,600, 50), col="black", lty=3)
mtext(fuente, side=1, line=4, at=1.5)

cant_costo <- datos_excel_solo_resultados[["C21"]]
cant_costo <- eliminaNA(cant_costo)

hist(cant_costo,
     breaks=seq(3000, 30000, 3000),
     col=paleta[4],
     axes=FALSE,
     main="Costo aproximado del alquiler de las viviendas\nBarrios populares de Argentina, 2020.",
     xlab="Costo",
     ylab="Viviendas"
     )
axis(side=1, at=seq(3000, 30000, 3000))
axis(side=2, at=seq(0, 40, 5))
abline(h=seq(5, 35, 5), col="black", lty=3)
mtext(fuente, side=1, line=4, at=5000)

dev.off()

# ================== Descripcion grafica de tendido electrico.

par(mfrow = c(1, 2))

cant_tendido_electrico <- table(datos_excel_solo_resultados[["C41"]])
cant_incendios <- table(datos_excel_solo_resultados[["C43"]])

pie(cant_tendido_electrico, 
    col= paleta,
    labels=paste(c("Total o parcialmente\n fuera de las paredes", "Totalmente dentro \nde las paredes"), c("\n"), 
                 round(cant_tendido_electrico/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1.2,
    main="Tendido eléctrico de las viviendas.\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2)

pie(cant_incendios, 
    col= c(paleta[2], paleta[1]),
    labels=paste(c("No", "Si"), c("\n"), 
                 round(cant_incendios/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1.2,
    main="Viviendas que experimentaron incendios en el último año\n debido a las condiciones de la instalacíon eléctrica.\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2)

dev.off()

# ================== Descripcion grafica de conectividad.

cant_conectividad <- datos_excel_solo_resultados[["C46"]]
cant_conectividad <- tieneConectividad(cant_conectividad, datos_excel_solo_resultados[["C47"]])

pie(cant_conectividad, 
    col= c(paleta[2], paleta[1]),
    labels=paste(c("Si", "No"), c("\n"), 
                 round(cant_conectividad/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1,
    main="Viviendas con al menos un teléfono celular con conectividad a Internet.\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2)



# ================== Medidas de posicion y dispersion de anteriores mencionados
# ================== Relacion entre 2 variables?


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

