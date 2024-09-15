#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("." )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./competencia_01.csv", na.strings="")

#uso esta semilla para los canaritos
set.seed(102192)

mes_entrenamiento = 202104
mes_aplicacion = 202106

dataset[ foto_mes == mes_entrenamiento ,
				clase_binaria := ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==mes_entrenamiento ]
dapply <- dataset[ foto_mes==mes_aplicacion ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
    formula= "clase_binaria ~ . -clase_ternaria",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 2, # dejo que crezca y corte todo lo que quiera
    minbucket= 1,
    maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"POS"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 1.0/40.0 ) ) )

fwrite( entrega, paste0( "./kaggle/stopping_at_canaritos.csv"), sep="," )

pdf(file = "./work/stopping_at_canaritos.pdf", width=28, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

