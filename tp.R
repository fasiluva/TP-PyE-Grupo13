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
paleta_degradado <- colorRampPalette(c(paleta[4], paleta[2]))
paleta_degradado <- paleta_degradado(10)
fuente <- "Fuente: La Poderosa. Encuesta realizado a barrios populares de Argentina."

# *********************************************************************
  
# Total de provincias
datos_provincias <- table(datos_excel_solo_resultados[[2]])
datos_provincias <- datos_provincias[order(datos_provincias, decreasing = TRUE)]
# ---
moda(datos_excel_solo_resultados[[2]])
# ---

frec_rel_prov <- round(datos_provincias/cant_viviendas * 100, 3)
tabla_frec_prov <- t(rbind(datos_provincias, frec_rel_prov))
colnames(tabla_frec_prov) <- c("Encuestas", "Frecuencia relativa (en %)")
tabla_frec_prov <- rbind(tabla_frec_prov, c(1222, 100))
rownames(tabla_frec_prov)[17] <- "Total"

tabla_frec_prov

# Total de barrios
datos_barrios <- table(datos_excel_solo_resultados[[3]])
datos_barrios <- datos_barrios[order(datos_barrios, decreasing=TRUE)]
# ---
moda(datos_excel_solo_resultados[[3]])
# ---

frec_rel_bar <-  round(datos_barrios/cant_viviendas * 100, 3)
tabla_frec_bar <- t(rbind(datos_barrios, frec_rel_bar))
colnames(tabla_frec_bar) <- c("Encuestas", "Frecuencia relativa (en %)")
tabla_frec_bar <- rbind(tabla_frec_bar, c(1222, 100))
rownames(tabla_frec_bar)[24] <- "Total"

tabla_frec_bar

# ================== Descripcion grafica de integrantes de la familia

# Grafico de torta de cantidad de personas segun genero.
cant_varones <- datos_excel_solo_resultados[["C8"]]
# --- 
media(cant_varones)
mediana(cant_varones)
RI(cant_varones)
moda(cant_varones)
cuartiles(cant_varones)
percentiles(cant_varones, seq(0,100,10))
desvio(cant_varones)
# ---

cant_mujeres <- datos_excel_solo_resultados[["C9"]]
# --- 
media(cant_mujeres)
mediana(cant_mujeres)
RI(cant_mujeres)
moda(cant_mujeres)
cuartiles(cant_mujeres)
percentiles(cant_mujeres, seq(0,100,10))
desvio(cant_mujeres)
# ---

cant_disidentes <- datos_excel_solo_resultados[["C10"]]
# ---
media(cant_disidentes)
mediana(cant_disidentes)
RI(cant_disidentes)
moda(cant_disidentes)
cuartiles(cant_disidentes)
percentiles(cant_disidentes, seq(0,100,10))
desvio(cant_disidentes)
# ---

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
# ---
media(cant_menores)
mediana(cant_menores)
RI(cant_menores)
moda(cant_menores)
cuartiles(cant_menores)
percentiles(cant_menores, seq(0,100,10))
desvio(cant_menores)
# ---

cant_discapacidad <- datos_excel_solo_resultados[["C12"]]
# ---
media(cant_discapacidad)
moda(cant_discapacidad)
cuartiles(cant_discapacidad)
percentiles(cant_discapacidad, seq(0,100,10))
desvio(cant_discapacidad)
# ---
cant_menores_total <- sum(cant_menores)
cant_discapacidad_total <- sum(cant_discapacidad)

boxplot(cant_mujeres, cant_varones, cant_disidentes, 
        names=c("Mujeres", "Varones", "Disidentes"),
        col=paleta[-1],
        ylab="Cantidad de personas",
        axes=TRUE,
        main="Boxplot comparativo entre cantidad de mujeres, varones y disidentes en la vivienda.\nBarrios populares de Argentina, año 2020.")
axis(2, seq(1,10,2))
mtext(fuente, side=1, line=3, at=1)

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

tabla_integrantes_menores <- table(datos_excel_solo_resultados[["C6"]], cant_menores)
plot(x=datos_excel_solo_resultados[["C6"]], y=cant_menores, 
     type="p",
     xlab="Cantidad de integrantes",
     ylab="Cantidad de menores",
     col=determinaColor(tabla_integrantes_menores, paleta_degradado),
     )


# ================== Descripcion grafica de obtencion de agua y saneamiento.

# Como obtiene el agua?
par(las=1, cex.lab=1.2, mar=c(5,10,5,1))
cant_obtencion_agua <- datos_excel_solo_resultados[["C24"]]
# ---
moda(cant_obtencion_agua)
# ---

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

cant_presion_agua <- datos_excel_solo_resultados[["C26"]]
# ---
mediana(presion_a_numeric(cant_presion_agua))
moda(cant_presion_agua)
cuartiles(presion_a_numeric(cant_presion_agua))
# ---

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
        xlab="Viviendas",
        main="Presencia de capacidad de almacenamiento de agua en altura respecto a la presión del agua.\nBarrios populares de Argentina, 2020."
        )
axis(side=1, seq(25,275,25))
abline(v=seq(0,300,25), col="grey", lty="dotted",)
legend(x =170, y = 15, 
       legend=c("No", "Si, entre 200 a 500 lts", "Si, de mas de 500 lts", "Si, de menos de 200 lts"), 
       fill = paleta)
mtext("Nota: Se excluyeron 2 resultados en los que afirmaban tener almacenamiento de agua en altura, pero de 
      una capacidad desconocida.", side=1, at=65, line=4, cex=0.8)
mtext(fuente, side=1, line=4, at=240)
box()

dev.off()

# ================== Descripcion grafica de cocina y calefaccion.

cant_fuentes_calefaccion <- datos_excel_solo_resultados[["C38"]]
# ---
moda(cant_fuentes_calefaccion)
# ---

cant_fuentes_calefaccion <- calculaFuentesCalefaccion(cant_fuentes_calefaccion)
cant_fuentes_calefaccion <- c("Red de gas" = cant_fuentes_calefaccion[1], 
                              "Gas envasado" = cant_fuentes_calefaccion[2], 
                              "Leña/carbón" = cant_fuentes_calefaccion[3], 
                              "Electricidad" = cant_fuentes_calefaccion[4])
cant_ventilacion <- datos_excel_solo_resultados[["C39"]]
# ---
moda(cant_ventilacion)
# ---

cant_ventilacion <- table(datos_excel_solo_resultados[["C39"]])
par(mfrow = c(1, 2))

barplot(cant_fuentes_calefaccion,
        beside=TRUE,
        col=paleta[3],
        axes=FALSE,
        xlab="Fuente",
        ylab="Viviendas",
        main="Fuentes de calefacción de las viviendas.\nBarrios populares de Argentina, 2020."
        )
axis(side=2, at=seq(0, 550, 50))
abline(h=seq(0,550,50), col="grey", lty="dotted")
box()

pie(cant_ventilacion, 
    col= paleta,
    labels=paste(c("No", "No aplica", "Si"), c("\n"), 
                 round(cant_ventilacion/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1,
    main="Viviendas con ventilación para la calefacción.\nBarrios populares de Argentina, 2020."
)

mtext("Nota: No aplica representa las viviendas que sólo tienen calefacción mediante electricidad.", 
      side=1, line=1, at=0, cex=0.8)
mtext(fuente, side=1, line=4, at=0)
box()

dev.off()

# ================== Descripcion grafica de alquiler.

par(mfrow = c(1, 2))

cant_propiedad <- datos_excel_solo_resultados[["C19"]]
# ---
moda(cant_propiedad)
# ---

cant_propiedad <- table(datos_excel_solo_resultados[["C19"]])

barplot(cant_propiedad[order(cant_propiedad, decreasing=TRUE)], 
    col=paleta[3],
    ylim=c(0, 600), 
    main="Tipo de tenencia sobre la propiedad\nBarrios populares de Argentina, 2020.",
    ylab="Viviendas",
    xlab="Tenencia")
abline(h=seq(0,600, 50), col="black", lty=3)
box()

cant_costo <- datos_excel_solo_resultados[["C21"]]
cant_costo <- eliminaNA(cant_costo)
# ---
mediana(cant_costo)
RI(cant_costo)
cuartiles(cant_costo)
percentiles(cant_costo, seq(0,100,10))
rango(cant_costo)
# ---

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
mtext(fuente, side=1, line=4, at=100)
box()

dev.off()

# ================== Descripcion grafica de tendido electrico.

par(mfrow = c(1, 2))

cant_tendido_electrico <- datos_excel_solo_resultados[["C41"]]
# ---
mediana(tendido_a_numeric(cant_tendido_electrico))
moda(cant_tendido_electrico)
cuartiles(tendido_a_numeric(cant_tendido_electrico))
# ---

cant_tendido_electrico <- table(cant_tendido_electrico)
cant_incendios <- datos_excel_solo_resultados[["C43"]]
# ---
mediana(incendio_a_numeric(cant_incendios))
moda(cant_incendios)
cuartiles(incendio_a_numeric(cant_incendios))
# ---

cant_incendios <- table(cant_incendios)

pie(cant_tendido_electrico, 
    col= paleta,
    labels=paste(c("Total o parcialmente\n fuera de las paredes", "Totalmente dentro \nde las paredes"), c("\n"), 
                 round(cant_tendido_electrico/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1.2,
    main="Tendido eléctrico de las viviendas.\nBarrios populares de Argentina, 2020."
)

pie(cant_incendios, 
    col= c(paleta[2], paleta[1]),
    labels=paste(c("No", "Si"), c("\n"), 
                 round(cant_incendios/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1.2,
    main="Viviendas que experimentaron incendios en el último año\n debido a las condiciones de la instalacíon eléctrica.\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2, at=-2.5)

dev.off()

# ================== Descripcion grafica de conectividad.

cant_conectividad <- datos_excel_solo_resultados[["C46"]]
# ---
moda(cant_conectividad)
# ---

cant_conectividad <- tieneConectividad(cant_conectividad, datos_excel_solo_resultados[["C47"]])

pie(cant_conectividad, 
    col= c(paleta[2], paleta[1]),
    labels=paste(c("Si", "No"), c("\n"), 
                 round(cant_conectividad/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=0.9,
    main="Viviendas con al menos un teléfono celular con conectividad a Internet.\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2)

# ================== Descripcion grafica de filtraciones y riesgo de derrumbe.

par(mfrow = c(1, 2))

cant_filtraciones <- datos_excel_solo_resultados[["C63"]]
# ---
moda(cant_filtraciones)
# ---
cant_filtraciones <- calculaFiltraciones(cant_filtraciones)

barplot(cant_filtraciones[order(cant_filtraciones, decreasing=FALSE)], 
        col=paleta[3],
        ylim=c(0, 600), 
        main="Filtraciones/humedad en las viviendas\nBarrios populares de Argentina, 2020.",
        ylab="Viviendas",
        xlab="Ubicación de la filtración")
abline(h=seq(0,600, 50), col="black", lty=3)

cant_riesgo_derrumbe <- datos_excel_solo_resultados[["C64"]]
# ---
moda(cant_riesgo_derrumbe)
mediana(riesgo_a_numeric(cant_riesgo_derrumbe))
cuartiles(riesgo_a_numeric(cant_riesgo_derrumbe))
# ---

cant_riesgo_derrumbe <- calculaRiesgoDerrumbe(cant_riesgo_derrumbe)

pie(cant_riesgo_derrumbe, 
    col= c(paleta[2], paleta[1]),
    labels=paste(c("Con riesgo", "Sin riesgo"), c("\n"), 
                 round(cant_riesgo_derrumbe/cant_viviendas * 100, 2), c("%")), 
    clockwise=TRUE,
    radius=1,
    main="Viviendas con problemas estructurales graves y riesgo de derrumbe\nBarrios populares de Argentina, 2020."
)
mtext(fuente, side=1, line=2)

dev.off()

# ================== Descripcion grafica de plagas.

cant_plagas <- datos_excel_solo_resultados[["C73"]]
# ---
moda(cant_plagas)
# ---

cant_plagas <- calculaPlagas(cant_plagas)

barplot(cant_plagas[order(cant_plagas, decreasing=FALSE)], 
        col=paleta[3],
        ylim=c(0, 600), 
        main="Plagas presentes en las viviendas\nBarrios populares de Argentina, 2020.",
        xlab="Plagas",
        ylab="Cantidad de viviendas con dicha plaga")
axis(2, seq(50, 550, 50))
abline(h=seq(0,600, 50), col="black", lty=3)
box()
mtext(fuente, side=1, line=4, at=1)


