#!/usr/bin/env Rscript
cat( "ETAPA  z1101_DT_incorporar_dataset.r  INIT\n")

# Workflow  Inforporar Dataset

# inputs
#  * dataset
# output  
#   dataset :
#     misma cantidad de registros
#     misma cantidad de atributos
#     valores modificados para las  < variables, mes > que habian sido dañadas con un  0

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)


# cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z1101_DT_incorporar_dataset.r  START\n")
action_inicializar() 


# cargo el dataset
cat("lectura del dataset\n")
arch <- envg$PARAM$archivo
action_verificar_archivo( arch )
cat( "Iniciando lectura del archivo\n" )
dataset <- fread( arch )
cat( "Finalizada lectura del archivo\n" )

#--------------------------------------
# verifico que existan los campos de la metadata

cat("verificacion de nombres de campo\n")
campos <- copy( colnames( dataset ) )

campitos <- c( envg$PARAM$primarykey, envg$PARAM$entity_id, envg$PARAM$periodo, envg$PARAM$clase )
campitos <- unique( campitos )

for( vcampo in campitos ){

  if( ! (vcampo %in% campos ) ) 
    action_abortar( paste0( "No existe el campo : ", vcampo ) )
}

GrabarOutput()
#--------------------------------------
# verifico primarykey

cat("verificacion de primary key\n")
pk_qty <- nrow(unique(dataset[ ,envg$PARAM$primarykey, with=FALSE ]))

if( pk_qty != nrow( dataset ) )
  accion_abortar( "Primary Key inconsistente"  )

#--------------------------------------
# ordeno el dataset

cat("ordeno el dataset\n")
setorderv(dataset, envg$PARAM$primarykey)

#------------------------------------------------------------------------------
# grabo el dataset

cat("grabo el dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )
#--------------------------------------
# grabo metadata

cat("grabo metadata\n")

dataset_metadata <- copy( envg$PARAM )
dataset_metadata$archivo <- NULL
dataset_metadata$semilla <- NULL

dataset_metadata$cols <- colnames( dataset )

write_yaml( dataset_metadata, file="dataset_metadata.yml" )

#------------------------------------------------------------------------------
# guardo los campos que tiene el dataset

tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "ETAPA  z1101_DT_incorporar_dataset.r  END\n")
