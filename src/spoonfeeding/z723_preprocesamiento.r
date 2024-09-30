# Este script esta pensado para correr en Google Cloud
#   8 vCPU
# 32 GB memoria RAM, en la medida que no agregue mas campos

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("ulimit")  # para controlar la memoria


# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "PP7230"

PARAM$input$dataset <- "./datasets/competencia_01.csv"

PARAM$semilla_azar <- 102191 # Aqui poner su  primer  semilla


PARAM$driftingcorreccion <- "ninguno"
PARAM$clase_minoritaria <- c("BAJA+1","BAJA+2")

# los meses en los que vamos a entrenar
#  la magia estara en experimentar exhaustivamente
PARAM$trainingstrategy$testing <- c(202104)
PARAM$trainingstrategy$validation <- c(202103)
PARAM$trainingstrategy$training <- c(202102)


PARAM$trainingstrategy$final_train <- c(202102, 202103, 202104)
PARAM$trainingstrategy$future <- c(202106)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$training_undersampling <- 1.0

# esta aberracion fue creada a pedido de Joaquin Tschopp
#  Publicamente Gustavo Denicolay NO se hace cargo de lo que suceda
#   si se asigna un valor menor a 1.0
PARAM$trainingstrategy$finaltrain_undersampling <- 1.0

#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {

  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------
# valores financieros
# meses que me interesan
vfoto_mes <- c(
  202101, 202102, 202103,
  202104, 202105, 202106
)

# los valores que siguen fueron calculados por alumnos
#  si no esta de acuerdo, cambielos por los suyos

# momento 1.0  31-dic-2020 a las 23:59
vIPC <- c(
  0.9680542110, 0.9344152616, 0.8882274350,
  0.8532444140, 0.8251880213, 0.8003763543
)

vdolar_blue <- c(
  157.900000, 149.380952, 143.615385,
  146.250000, 153.550000, 162.000000
)

vdolar_oficial <- c(
   91.474000,  93.997778,  96.635909,
   98.526000,  99.613158, 100.619048
)
  
vUVA <- c(
  0.9669867858358365, 0.9323750098728378, 0.8958202912590305,
  0.8631993702994263, 0.8253893405524657, 0.7928918905364516
)

#------------------------------------------------------------------------------

Corregir_interpolar <- function(pcampo, pmeses) {

  tbl <- dataset[, list(
    "v1" = shift(get(pcampo), 1, type = "lag"),
    "v2" = shift(get(pcampo), 1, type = "lead")
  ),
  by = numero_de_cliente
  ]

  tbl[, numero_de_cliente := NULL]
  tbl[, promedio := rowMeans(tbl, na.rm = TRUE)]

  dataset[
    ,
    paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
      get(pcampo),
      tbl$promedio
    )
  ]
}
#------------------------------------------------------------------------------

AsignarNA_campomeses <- function(pcampo, pmeses) {

  if( pcampo %in% colnames( dataset ) ) {
  
    dataset[ foto_mes %in% pmeses, paste0(pcampo) := NA ]
  }
}
#------------------------------------------------------------------------------

Corregir_atributo <- function(pcampo, pmeses, pmetodo)
{
  # si el campo no existe en el dataset, Afuera !
  if( !(pcampo %in% colnames( dataset )) )
    return( 1 )

  # llamo a la funcion especializada que corresponde
  switch( pmetodo,
    "MachineLearning"     = AsignarNA_campomeses(pcampo, pmeses),
    "EstadisticaClasica"  = Corregir_interpolar(pcampo, pmeses)
  )

  return( 0 )
}
#------------------------------------------------------------------------------

Corregir_Rotas <- function(dataset, pmetodo) {
  gc()
  cat( "inicio Corregir_Rotas()\n")
  # acomodo los errores del dataset

  Corregir_atributo("ccajeros_propios_descuentos",
    c(202102), pmetodo)

  Corregir_atributo("mcajeros_propios_descuentos",
    c(202102), pmetodo)

  Corregir_atributo("ctarjeta_visa_descuentos",
    c(202102), pmetodo)

  Corregir_atributo("mtarjeta_visa_descuentos",
    c(202102), pmetodo)

  Corregir_atributo("ctarjeta_master_descuentos",
    c(202102), pmetodo)

  Corregir_atributo("mtarjeta_master_descuentos",
    c(202102), pmetodo)

  cat( "fin Corregir_rotas()\n")
}
#------------------------------------------------------------------------------

drift_UVA <- function(campos_monetarios) {
  cat( "inicio drift_UVA()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.UVA,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_UVA()\n")
}
#------------------------------------------------------------------------------

drift_dolar_oficial <- function(campos_monetarios) {
  cat( "inicio drift_dolar_oficial()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD / i.dolar_oficial,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_oficial()\n")
}
#------------------------------------------------------------------------------

drift_dolar_blue <- function(campos_monetarios) {
  cat( "inicio drift_dolar_blue()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD / i.dolar_blue,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_blue()\n")
}
#------------------------------------------------------------------------------

drift_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_deflacion()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_deflacion()\n")
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {

  cat( "inicio drift_estandarizar()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = "foto_mes"]

    dataset[, (campo) := NULL]
  }
  cat( "fin drift_estandarizar()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )


 # tabla de indices financieros
tb_indices <- as.data.table( list( 
  "IPC" = vIPC,
  "dolar_blue" = vdolar_blue,
  "dolar_oficial" = vdolar_oficial,
  "UVA" = vUVA
  )
)

tb_indices$foto_mes <- vfoto_mes

tb_indices

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)



# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# Catastrophe Analysis  -------------------------------------------------------
# corrijo las variables que con el script Catastrophe Analysis detecte que
#   eestaban rotas

# ordeno dataset
setorder(dataset, numero_de_cliente, foto_mes)
# corrijo usando el metido MachineLearning
Corregir_Rotas(dataset, "MachineLearning")


# Data Drifting  --------------------------------------------------------------
#  atencion que lo que mejor funciona
#  no necesariamente es lo que usted espera

# ordeno dataset
setorder(dataset, numero_de_cliente, foto_mes)

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor

# defino cuales son los campos monetarios de mi dataset
campos_monetarios <- colnames(dataset)
# a continuacion una expresion regular
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

switch(PARAM$driftingcorreccion,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "dolar_blue"     = drift_dolarblue(campos_monetarios),
  "dolar_oficial"  = drift_dolaroficial(campos_monetarios),
  "UVA"            = drift_UVA(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)



# Feature Engineering Intra-mes Manual Artesanal  -----------------------------
#  esta seccion es POCO importante
# el mes 1,2, ..12
dataset[, kmes := foto_mes %% 100]

# creo un ctr_quarter que tenga en cuenta cuando
# los clientes hace 3 menos meses que estan
# ya que seria injusto considerar las transacciones medidas en menor tiempo

dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]

dataset[
  cliente_antiguedad == 3,
  ctrx_quarter_normalizado := ctrx_quarter * 1.2
]

# variable extraida de una tesis de maestria de Irlanda
#  perdi el link a la tesis, NO es de mi autoria
dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]


# Por supuesto, usted puede COMENTARIAR todo lo que desee
dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]
dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]
dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

# valvula de seguridad para evitar valores infinitos
# paso los infinitos a NULOS
infinitos <- lapply(
  names(dataset),
  function(.name) dataset[, sum(is.infinite(get(.name)))]
)

infinitos_qty <- sum(unlist(infinitos))
if (infinitos_qty > 0) {
  cat(
    "ATENCION, hay", infinitos_qty,
    "valores infinitos en tu dataset. Seran pasados a NA\n"
  )
  dataset[mapply(is.infinite, dataset)] <- NA
}


# valvula de seguridad para evitar valores NaN  que es 0/0
# paso los NaN a 0 , decision polemica si las hay
# se invita a asignar un valor razonable segun la semantica del campo creado
nans <- lapply(
  names(dataset),
  function(.name) dataset[, sum(is.nan(get(.name)))]
)

nans_qty <- sum(unlist(nans))
if (nans_qty > 0) {
  cat(
    "ATENCION, hay", nans_qty,
    "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
  )

  cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
}



# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso !  corta la bocha

campitos <- c( "numero_de_cliente",
  "foto_mes",
  "clase_ternaria")


cols_lagueables <- copy(
  setdiff(colnames(dataset), campitos)
)

# ordeno el dataset, FUNDAMENTAL
setorder(dataset, numero_de_cliente, foto_mes)

# creo los lags de orden 1
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
  by = numero_de_cliente,
  .SDcols = cols_lagueables
]

# agrego los delta lags de orden 1
for (vcol in cols_lagueables)
{
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
}


# Training Strategy  ----------------------------------------------

dataset[, part_future := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$future,
  part_future := 1L]

dataset[, part_validation := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$validation,
  part_validation := 1L]

dataset[, part_testing := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$testing,
  part_testing := 1L]

set.seed(PARAM$semilla_azar, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(nrow(dataset))]

dataset[, part_training := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$training &
  (azar <= PARAM$trainingstrategy$training_undersampling |
   clase_ternaria %in% PARAM$clase_minoritaria ),
  part_training := 1L
]


dataset[, part_final_train := 0L ]
dataset[ foto_mes %in% PARAM$trainingstrategy$final_train &
  (azar <= PARAM$trainingstrategy$finaltrain_undersampling |
   clase_ternaria %in% PARAM$clase_minoritaria ),
  part_final_train := 1L
]

# elimino el campo azar, ya no lo uso mas
dataset[, azar := NULL ]

# Grabo el dataset
fwrite( dataset,
  file = "dataset.csv.gz",
  sep = "\t"
)

cat("\n\nEl preprocesamiento ha terminado\n")
