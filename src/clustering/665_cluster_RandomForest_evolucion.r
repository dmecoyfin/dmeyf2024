# ideas para un clustering derivado del Machnie Learning
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

require("randomForest")
require("ranger")

PARAM <- list()
PARAM$experimento <- "clu-randomforest"
PARAM$semilla_primigenia <- 955841   # aqui va SU semilla
PARAM$dataset <- "/Users/rmarques/UBA/DMEyF/dmeyf2024/datasets/competencia_01.csv"


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/UBA/DMEyF/dmeyf2024/src/rpart/buckets/b1")

# leo el dataset
dataset <- fread(PARAM$dataset)


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# campos arbitrarios, solo como ejemplo
# usted DEBE MANDARIAMENTE agregar más campos aqui
# no permita que la pereza se apodere de su alma

# campos_cluster <- c("cliente_edad", "cliente_antiguedad", "ctrx_quarter",
#   "mpayroll", "mcaja_ahorro", "mtarjeta_visa_consumo",
#   "mtarjeta_master_consumo", "mprestamos_personales",
#   "Visa_status", "Master_status", "cdescubierto_preacordado")

# Definición de las columnas base y las columnas de ingeniería de características (FE)
campos_cluster <- c(
  # Demografía del cliente
  "cliente_edad", 
  "cliente_antiguedad",
  
  # Actividad en la cuenta
  "ctrx_quarter",
  "mpayroll", 
  "mcaja_ahorro", 
  
  # Consumo y estado de tarjetas de crédito
  "mtarjeta_visa_consumo", 
  "mtarjeta_master_consumo", 
  "Visa_status", 
  "Master_status", 
  "Visa_mlimitecompra",
  "Master_mlimitecompra",
  
  # Préstamos y descubierto
  "mprestamos_personales", 
  "cdescubierto_preacordado"
)

# Generación del dataset chico para el análisis de clientes que se dieron de baja (BAJA+2)
dchico <- dataset[clase_ternaria == "BAJA+2", c("numero_de_cliente", campos_cluster), with = FALSE]

dchico[, limite_credito_total := Visa_mlimitecompra + Master_mlimitecompra]
dchico[, consumo_total := mtarjeta_visa_consumo + mtarjeta_master_consumo]
dchico[limite_credito_total == 0 | is.na(limite_credito_total), limite_credito_total := 1]
dchico[, utilizacion_credito := consumo_total / limite_credito_total]
dchico[, ratio_prestamo_ingreso := mprestamos_personales / mpayroll]
dchico[mpayroll == 0 | is.na(mpayroll), ratio_prestamo_ingreso := 0]

# Agregar las columnas de FE a 'campos_cluster'
campos_cluster <- c(campos_cluster, "limite_credito_total", "consumo_total", "utilizacion_credito", "ratio_prestamo_ingreso")
print(campos_cluster)

# Arreglo los valores NA
dchico  <- na.roughfix( dchico )
# no hace falta escalar

# invoco a la distancia de Random Forest
 # ahora, a esperar .. con esta libreria de la prehistoria
#  que NO corre en paralelo

set.seed(PARAM$semilla_primigenia)

modelo <- randomForest( 
  x= dchico[, campos_cluster, with=FALSE ],
  y= NULL,
  ntree= 10000, #se puede aumentar a 10000
  proximity= TRUE,
  oob.prox=  TRUE )

# genero los clusters jerarquicos
# distancia = 1.0 - proximidad
hclust.rf <- hclust( 
  as.dist ( 1.0 - modelo$proximity),
  method= "ward.D2" )

# Save hierarchical clustering plot as PDF
pdf("cluster_jerarquico.pdf")
plot(hclust.rf)
dev.off()

# # K-means clustering based on the proximity matrix
# proximity_matrix <- modelo$proximity
# distance_matrix <- 1 - proximity_matrix

# # Elbow Method to find optimal number of clusters
# wss <- numeric(15)  # We'll test for up to 15 clusters
# for (k in 1:15) {
#   kmeans_model <- kmeans(distance_matrix, centers = k, nstart = 10)
#   wss[k] <- kmeans_model$tot.withinss  # Total within-cluster sum of squares
# }

# # Plot the Elbow Method result
# pdf("elbow_plot_kmeans.pdf")
# plot(1:15, wss, type = "b", pch = 19, frame = FALSE,
#      xlab = "Number of Clusters", ylab = "Total Within-Cluster SS",
#      main = "Elbow Method for Optimal Number of Clusters")
# dev.off()


# imprimo un pdf con la forma del cluster jerarquico

pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


kclusters <- 5  # cantidad de clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=kclusters & distintos <=kclusters ) )
{
  h <- h - 1
  rf.cluster <- cutree( hclust.rf, h)

  dchico[, cluster := paste0("cluster_", rf.cluster) ]

  distintos <- nrow( dchico[, .N, cluster ] )
  cat( distintos, " " )
}


#--------------------------------------

setorder( dchico, cluster, numero_de_cliente )

fwrite(dchico,
       file= "dchico.txt",
       sep= "\t")

#--------------------------------------

# Calcular y mostrar estadísticas clave por cluster
calcular_estadisticas_cluster <- function(data, cluster_col) {
  data[, .(
    Edad_promedio = mean(cliente_edad, na.rm = TRUE),
    Antiguedad_promedio = mean(cliente_antiguedad, na.rm = TRUE),
    Transacciones_promedio = mean(ctrx_quarter, na.rm = TRUE),
    Ingreso_payroll_promedio = mean(mpayroll, na.rm = TRUE),
    Saldo_ahorro_promedio = mean(mcaja_ahorro, na.rm = TRUE),
    Consumo_Visa_promedio = mean(mtarjeta_visa_consumo, na.rm = TRUE),
    Consumo_Master_promedio = mean(mtarjeta_master_consumo, na.rm = TRUE),
    Limite_Visa_promedio = mean(Visa_mlimitecompra, na.rm = TRUE),
    Limite_Master_promedio = mean(Master_mlimitecompra, na.rm = TRUE),
    Préstamos_personales_promedio = mean(mprestamos_personales, na.rm = TRUE),
    Descubierto_preacordado_promedio = mean(cdescubierto_preacordado, na.rm = TRUE),
    Limite_credito_total_promedio = mean(limite_credito_total, na.rm = TRUE),
    Consumo_total_promedio = mean(consumo_total, na.rm = TRUE),
    Utilizacion_credito_promedio = mean(utilizacion_credito, na.rm = TRUE),
    Relacion_prestamo_ingreso_promedio = mean(ratio_prestamo_ingreso, na.rm = TRUE)
  ), by = cluster_col]
}

# Aplicar la función a los datos por cluster
estadisticas_clusters <- calcular_estadisticas_cluster(dchico, "cluster")

# Guardar las estadísticas en un archivo de texto
fwrite(estadisticas_clusters, 
      file = "estadisticas_por_cluster.txt", 
      sep = "\t")

# Imprimir las estadísticas
# print(estadisticas_clusters)


# #--------------------------------------
# # Analisis de resultados del clustering jerarquico
# # cantidad de registros por cluster

# dcentroides <- dchico[, lapply(.SD, mean, na.rm=TRUE), 
#     by= cluster, 
#     .SDcols= campos_cluster ]

# dcentroides

# fwrite(dcentroides,
#        file= "centroides.txt",
#        sep= "\t" )

# #--------------------------------------
# # gafico los clusters en forma bivariada

# # Solo voy a mostrar un porcentaje de dchico
# dchico[, azar := runif(nrow(dchico)) ]
# muestra <- 0.1  # me voy a quedar con los menores a este valor

# # calculo la cantidad de campos
# n <- length(campos_cluster)


# # voy a graficar en escala logaritmica
# # cuidado con 

# pdf("bivariado.pdf")

# for( i in 1:(n-1) ){
#   for( j in (i+1):n ){

#   grafico <- ggplot( dchico[azar< muestra],
#       aes_string(x= campos_cluster[i],
#                  y= campos_cluster[j],
#                  color= "cluster"))  +
#       scale_colour_brewer(palette = "Dark2") +
#       geom_point(alpha = 0.50) +
#       xlab(campos_cluster[i]) +
#       # scale_x_continuous(trans = pseudolog10_trans) +
#       ylab(campos_cluster[j]) 
#       # scale_y_continuous(trans = pseudolog10_trans)

#    print( grafico )
#   }
# }

# dev.off()

# # -----------------------------------------------------------------------------
# # Ahora incorporo la evolucion historica antes de la BAJA

# # leo la historia ( desde donde hay,  202101 )
# dhistoria <- fread(PARAM$dataset)
# thewalkingdead <- dhistoria[ clase_ternaria =="BAJA+2", unique(numero_de_cliente) ]

# dwalkingdead <- dhistoria[ numero_de_cliente %in% thewalkingdead ]


# # asigno el cluster a los 
# dwalkingdead[ dchico,
#            on= "numero_de_cliente",
#            cluster := i.cluster ]

# # asigno cuentra regresiva antes de la BAJA
# setorder( dwalkingdead, numero_de_cliente, -foto_mes )

# dwalkingdead[, periodo := - rowid(numero_de_cliente)]

# # ejemplo
# dwalkingdead[numero_de_cliente==1550236937, list( numero_de_cliente, foto_mes, periodo ) ]


# # grafico la evolucion de cada < cluster, variable >  univariado ------

# # todos los campos menos los que no tiene sentido
# campos_totales <- setdiff( colnames(dwalkingdead),
#   c("numero_de_cliente","foto_mes","clase_ternaria","cluster","periodo") )



# # Genero el grafico intervalo confianza 95%
# pdf("evol_RandomForest.pdf")

# for( campo in campos_totales ) {

#   cat( campo, " " )

#   grafico <- ggplot( dwalkingdead[periodo >= -6],
#     aes_string(x= "periodo",
#                y= campo,
#                color= "cluster"))  +
#     scale_colour_brewer(palette= "Dark2") +
#     xlab("periodo") +
#     ylab(campo) +
#     geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

#   print( grafico )
# }

# dev.off()



# #--------------------------------------------------------------------
# # quito los CEROS  de los graficos

# # reemplazo los CEROS  por NA
# #  los registros NA no se grafican
# dwalkingdead[ dwalkingdead==0, ] <- NA

# # Genero el grafico intervalo confianza 95%
# pdf("evol_noceros_RandomForest.pdf")

# for( campo in campos_totales ) {

#   cat( campo, " " )

#   grafico <- ggplot( dwalkingdead[periodo >= -6],
#     aes_string(x= "periodo",
#                y= campo,
#                color= "cluster"))  +
#     scale_colour_brewer(palette= "Dark2") +
#     xlab("periodo") +
#     ylab(campo) +
#     geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

#   print( grafico )
# }

# dev.off()

#--------------------------------------


# Renombrar los clusters por las personas para la presentación
dchico[, persona := fifelse(cluster == "cluster_1", "El Ahorrista Inactivo",
                   fifelse(cluster == "cluster_2", "El Usuario Moderado",
                   fifelse(cluster == "cluster_3", "El Prestatario Cauteloso",
                   fifelse(cluster == "cluster_4", "El Cliente Desvinculado",
                   "El Gran Consumidor"))))]



# Función ajustada para eliminar outliers basados en IQR y estadística del negocio
remove_outliers <- function(x, lower_quantile = 0.25, upper_quantile = 0.75, multiplier = 1.5) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  # Ajustar outliers basados en conocimiento del negocio
  lower_bound <- max(min(x, na.rm = TRUE), qnt[1] - multiplier * iqr)
  upper_bound <- min(max(x, na.rm = TRUE), qnt[2] + multiplier * iqr)
  
  return(pmin(pmax(x, lower_bound), upper_bound))  # Mantener dentro del rango ajustado
}

# Aplicar la función a las variables clave
dchico[, utilizacion_credito := remove_outliers(utilizacion_credito)]
dchico[, limite_credito_total := remove_outliers(limite_credito_total)]
dchico[, consumo_total := remove_outliers(consumo_total)]
dchico[, mprestamos_personales := remove_outliers(mprestamos_personales)]
dchico[, mpayroll := remove_outliers(mpayroll)]

# Crear gráficos para la presentación con las personas y sin outliers extremos
pdf("graficos_personas_presentacion_v3.pdf")

# Gráfico 1: Utilización de Crédito por Persona
grafico_utilizacion_credito <- ggplot(dchico, aes(x = persona, y = utilizacion_credito, fill = persona)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Persona", y = "Utilización de Crédito (%)", title = "Utilización de Crédito por Persona") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
print(grafico_utilizacion_credito)

# Gráfico 2: Límite de Crédito Total por Persona
grafico_limite_credito <- ggplot(dchico, aes(x = persona, y = limite_credito_total, fill = persona)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Persona", y = "Límite de Crédito Total", title = "Límite de Crédito Total por Persona") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
print(grafico_limite_credito)

# Gráfico 3: Consumo Total por Persona
grafico_consumo_total <- ggplot(dchico, aes(x = persona, y = consumo_total, fill = persona)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Persona", y = "Consumo Total", title = "Consumo Total por Persona") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
print(grafico_consumo_total)

# Gráfico 4: Préstamos Personales por Persona
grafico_prestamos <- ggplot(dchico, aes(x = persona, y = mprestamos_personales, fill = persona)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Persona", y = "Préstamos Personales", title = "Préstamos Personales por Persona") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
print(grafico_prestamos)

dev.off()