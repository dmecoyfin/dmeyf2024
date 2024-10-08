#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("." )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./competencia_01.csv", na.strings="")

mes_entrenamiento = 202104
mes_aplicacion = 202106

dataset[ foto_mes == mes_entrenamiento ,
				clase_binaria := ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]

dataset_entrenar <- dataset[ foto_mes == mes_entrenamiento ]
dataset_aplicar <- dataset[ foto_mes == mes_aplicacion ]

for( vmaxdepth  in 4:25 )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ . -clase_ternaria",
                   data= dataset_entrenar,
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= -1,
                   minsplit= 5,
                   maxdepth=  vmaxdepth
                  )

  #aplico el modelo a los datos en donde entrene
  prediccion_train  <- predict( object=  modelo,
						         newdata= dataset_entrenar,
								 type = "prob")
  ganancia_train <- dataset_entrenar[ prediccion_train[, "POS"] > 1.0/40.0, sum(ifelse(clase_ternaria == "BAJA+2", 273000, -7000)) ]

  cat( vmaxdepth, "\t", ganancia_train, "\n" )

  prediccion_test  <- predict( object=modelo,
                                 newdata=dataset_aplicar,
                                 type = "prob")

  prob_pos  <- prediccion_test[, "POS"]
  estimulo  <- as.numeric(prob_pos > 0.025)

  entrega <-  as.data.table( list(  "numero_de_cliente"= dataset_aplicar$numero_de_cliente,
                                    "Predicted"=  estimulo ) )

  #genero el archivo para Kaggle
  fwrite( entrega,
          file= paste0("./kaggle/altura_", vmaxdepth, ".csv"))
}

