# Script para encontrar Visuamente  el data drifting
# focalizado solo en los campos de un buen arbol de deicision

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("yaml")

kmes0 <- 202104
kmes1 <- 202106

#------------------------------------------------------------------------------

precalcular_campo <- function( campo, pmes )
{
  tbl <- dataset[foto_mes == pmes, 
    list( "atributo" = get(campo), clase_ternaria)]

  setorderv( tbl, "atributo" )

  tbl[, tot := 1 ]
  tbl[, pos := ifelse( clase_ternaria %in% c("BAJA+2"), 1, 0  ) ]
  tbl[, neg := ifelse( clase_ternaria %in% c("BAJA+2"), 0, 1  ) ]

  tbl[, tot_acum := cumsum( tot ) ]
  tbl[, pos_acum := cumsum( pos ) ]
  tbl[, neg_acum := cumsum( neg ) ]

  tot_max <- tbl[, max(tot_acum, na.rm=TRUE) ]
  pos_max <- tbl[, max(pos_acum, na.rm=TRUE) ]
  neg_max <- tbl[, max(neg_acum, na.rm=TRUE) ]

  tbl[, tot_acum := tot_acum / tot_max ]
  tbl[, pos_acum := pos_acum / pos_max ]
  tbl[, neg_acum := neg_acum / neg_max ]

  return( tbl )
}
#------------------------------------------------------------------------------

graficar_drift <- function(campo, pmes0, pmes1) {
  # quito de grafico las colas del 5% de las densidades
  tbl0 <- precalcular_campo( campo, pmes0 )
  tbl1 <- precalcular_campo( campo, pmes1 )

  # paso a escala logaritmica los positivos y tambien los negativos
  tbl0[, atributo := sign(atributo) * log2(abs(atributo) + 1) ]
  tbl1[, atributo := sign(atributo) * log2(abs(atributo) + 1) ]

  xxmin <- pmin( min(tbl0$atributo, na.rm=TRUE), min(tbl1$atributo, na.rm=TRUE))
  xxmax <- pmax( max(tbl0$atributo, na.rm=TRUE), max(tbl1$atributo, na.rm=TRUE))


  plot(
    type="l",
    col = "blue",
    x = tbl0$atributo,
    y = tbl0$tot_acum,
    xlim = c(xxmin, xxmax),
    ylim = c(0, 1.1),
    main = campo,
    xlab = paste0( "sign(x) * log2(|x| + 1)  ;   x=" , campo),
    ylab = "registros"
  )

  lines( x=tbl0$atributo, y=tbl0$pos_acum, col="blue", type="l", lty=2 )

  lines( x=tbl1$atributo, y=tbl1$tot_acum, col="red")
  lines( x=tbl1$atributo, y=tbl1$pos_acum, col="red", type="l", lty=2 )

  legend("topright",
    legend = c( pmes0, pmes1),
    col = c("blue", "red"), lty=c(1,1)
  )


 # Curva ROC
  plot(
    type="l",
    col = "blue",
    x = tbl0$neg_acum,
    y = tbl0$pos_acum,
    xlim = c(0, 1.1),
    ylim = c(0, 1.1),
    main =  paste0("raw ROC curve ", campo),
    xlab = "negatives",
    ylab = "positives"
  )

  lines( x=tbl1$neg_acum, y=tbl1$pos_acum, col="red", type="l")
  legend("topright",
    legend = c( pmes0, pmes1),
    col = c("blue", "red")
  )

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
setwd("~/buckets/b1/") # Establezco el Working Directory


# cargo dataset
dataset <- fread( "./datasets/competencia_01.csv" )

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/DR3161/", showWarnings = FALSE)
setwd("./exp/DR3161/")

# creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[
  foto_mes == kmes0,
  clase_binaria := ifelse(clase_ternaria == "CONTINUA", "NEG", "POS")
]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados
# en una Bayesian Optimizationcon 5-fold Cross Validation
modelo <- rpart(
  formula = "clase_binaria ~ . -clase_ternaria",
  data = dataset[foto_mes == kmes0], # los datos donde voy a entrenar
  xval = 0,
  cp = -1,
  minsplit = 1144,
  minbucket = 539,
  maxdepth = 8
)


campos_modelo <- names(modelo$variable.importance)
campos_buenos <- c(campos_modelo, setdiff(colnames(dataset), campos_modelo))
campos_buenos <- setdiff(
  campos_buenos,
  c("foto_mes", "clase_ternaria", "clase_binaria")
)


# genero los graficos en un archivo
for( kmes0 in c(202101, 202102, 202103, 202104) )
{
  pdf( paste0("densidades_", kmes0, "_", kmes1, ".pdf") )

  for (campo in campos_buenos) {
    cat(campo, "  ")
    graficar_drift(campo, kmes0, kmes1)
  }

  dev.off()
}

