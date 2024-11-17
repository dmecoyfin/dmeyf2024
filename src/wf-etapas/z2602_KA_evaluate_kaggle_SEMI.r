#!/usr/bin/env Rscript
cat( "ETAPA  z2602_KA_evaluate_kaggle_semillerio.r  INIT\n")

# Workflow  KA_evaluate_kaggle

# input   archivos predicciones del tipo  < primarykey,  prob > (de varias semillas)
# outputs
#   para cada archivo varios submits a Kaggle, segun el rango de cantidad de envios
#   archivo con las ganancias de kaggle y sus promedios
#   graficos con las ganancias

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose=FALSE) # garbage collection

require("data.table")
require("yaml")

#cargo la libreria
# args <- c( "~/labo2024ba", "SC-0002" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/mlog.r" ) )
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------

graficar_ganancias <- function( tb_ganancias_local, irank, ibayesiana, qsemillas )
{

  campos_buenos <- setdiff( colnames(tb_ganancias_local), c("envios","rank"))
  ymin <- min(tb_ganancias_local[, campos_buenos, with=FALSE], na.rm = TRUE) * 0.9
  ymax <- max(tb_ganancias_local[, campos_buenos, with=FALSE], na.rm = TRUE) * 1.1


  arch_grafico <- paste0(
    "modelo_",
    sprintf("%02d", irank),
    "_",
    sprintf("%03d", ibayesiana),
    ".pdf"
  )

  pdf(arch_grafico)

  plot(
    x = tb_ganancias_local[ , envios],
    y = tb_ganancias_local[ , m1 ],
    type = "l",
    col = "gray",
    ylim = c(ymin, ymax),
    main = paste0("Mejor gan prom = ", tb_ganancias_local[ , max(gan_sum)]),
    xlab = "Envios",
    ylab = "Ganancia",
    panel.first = grid()
  )

  # las siguientes curvas
  if ( qsemillas > 1) {
    for (s in 2:qsemillas)
    {
      lines(
        x = tb_ganancias_local[, envios],
        y = tb_ganancias_local[ , get(paste0("m", s))],
        col = "gray"
       )
    }
  }

  # finalmente la curva promedio
  lines(
    x = tb_ganancias_local[, envios],
    y = tb_ganancias_local[, gan_sum],
    col = "red"
  )

  dev.off()

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2602_KA_evaluate_kaggle_semillerio.r  START\n")
action_inicializar() 

GrabarOutput()

cat( "lectura tb_future_prediccion.txt\n")
arch_future_prediccion <- paste0( "./", envg$PARAM$input[1], "/tb_future_prediccion.txt")
action_verificar_archivo( arch_future_prediccion )
tb_future_prediccion <- fread(arch_future_prediccion)

cat( "lectura tb_predicciones.txt\n")
arch_tb_predicciones <- paste0( "./", envg$PARAM$input[1], "/tb_predicciones.txt")
action_verificar_archivo( arch_tb_predicciones )
tb_predicciones <- fread(arch_tb_predicciones)

# filtro solo los semillerios
tb_predicciones <- tb_predicciones[ semillerio==1 ]


envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[1], "/dataset_metadata.yml" ) )


# Inicializo MLFlow
mlog_init()

datahash <- mlog_table_hash( tb_future_prediccion[, envg$PARAM$dataset_metadata$primarykey, with=FALSE ] )
mlflow_exp_det <- paste0( "/futu-", datahash )

mlog_addfile("ganancias_log.txt",
             mlflow_exp= mlflow_exp_det,
             mlflow_run= envg$PARAM$experimento,
             cols_fijas= list(expw=envg$PARAM$experimento_largo) )



cortes <- seq(
    from = envg$PARAM$envios_desde,
    to = envg$PARAM$envios_hasta,
    by = envg$PARAM$envios_salto
  )


ranks <- tb_predicciones[ , unique(rank) ]

ganancia_mejor <- -Inf
vganancias <- c()

for ( irank in ranks ) {

  tb_ganancias_local <- as.data.table( list( envios =  cortes))
  tb_ganancias_local[, rank := irank ]
  tb_ganancias_local[ , gan_sum := 0 ]

  irepes <- tb_predicciones[ rank==irank, unique( repeticion )]
  irepes <- intersect( irepes, envg$PARAM$irepes_submit )

  for (vrepe in irepes)
  {
    cat( irank, vrepe, "\n")
    envg$OUTPUT$status$rank <- irank
    envg$OUTPUT$status$irepe <- vrepe
    GrabarOutput()

    nombre_raiz <- paste0(
      sprintf("%02d", irank),
      "_",
      sprintf("%03d", tb_predicciones[ rank==irank & repeticion==vrepe, iteracion_bayesiana]),
      "_r",
      vrepe
      )
  
    campito <- tb_predicciones[ rank==irank & repeticion==vrepe, campo]
    temp_pred <- tb_future_prediccion[ , c( envg$PARAM$dataset_metadata$entity_id, campito ), with=FALSE ]
    setorderv( temp_pred, campito, -1 )

    for (icorte in cortes)
    {
      temp_pred[, Predicted := 0L]
      temp_pred[1:icorte, Predicted := 1L]

      nom_submit <- paste0(
        envg$PARAM$experimento,
        "_",
        nombre_raiz,
        "_",
        sprintf("%05d", icorte),
        ".csv"
       )

      cat( "write prediccion Kaggle\n")
      cat( "Columnas del dataset: ",  colnames(temp_pred), "\n" )
      fwrite(temp_pred[, c(envg$PARAM$dataset_metadata$entity_id, "Predicted"), with=FALSE],
        file = nom_submit,
        sep = ","
      )
      cat( "written prediccion Kaggle\n")

      # hago el submit
      submitear <- TRUE
      if( "rango_submit"  %in%  names(envg$PARAM) )
      {
        if( !(sem %in% envg$PARAMrango_submit) ) submitear <- FALSE
      }

      if( "competition" %in% names(envg$PARAM) & submitear)
      {
        l1 <- "#!/bin/bash \n"
        l2 <- "source ~/.venv/bin/activate  \n"
        l3 <- paste0( "kaggle competitions submit -c ", envg$PARAM$competition)
        l3 <- paste0( l3, " -f ", nom_submit )
        l3 <- paste0( l3,  " -m ",  "\"", envg$PARAM$experimento,  " , ",  nom_submit , "\"",  "\n")
        l4 <- "deactivate \n"

        cat( paste0( l1, l2, l3, l4 ) , file = "subir.sh" )
        Sys.chmod( "subir.sh", mode = "744", use_umask = TRUE)

        res <- system( "./subir.sh", intern= TRUE )
        Sys.sleep( 30 )  # espero para no saturar
        res <- "Successfully"  # pequena ayuda ...

        if( substr(res, 1, 12) == "Successfully" ) {
          res <- system( paste0("~/install/list2 ", nom_submit), intern= TRUE )
          cat( "res= ", res, "\n" )
          tb_ganancias_local[ envios == icorte, paste0("m", vrepe) := as.numeric(res) ]
          tb_ganancias_local[ envios == icorte, gan_sum := gan_sum + as.numeric(res) ]

          # MLFlow
          linea <- list()
          linea$rank <- irank
          linea$iteracion_bayesiana <- tb_predicciones[ rank==irank & repeticion==vrepe, iteracion_bayesiana]
          linea$qsemillas <- 1
          linea$semilla <- tb_predicciones[ rank==irank & repeticion==vrepe, semilla]
          linea$corte <- icorte
          linea$ganancia <- as.numeric(res)
          linea$metrica <- as.numeric(res)
          mlog_log(linea, arch = "ganancias_log.txt")


        }

      }

    }

    rm(temp_pred)
    gc(verbose= FALSE)
  }

  tb_ganancias_local[ , gan_sum := gan_sum / length(irepes)]
  vganancias <- c( vganancias, tb_ganancias_local[ , max( gan_sum )] )

  graficar_ganancias( 
      tb_ganancias_local,
      irank,
      ibayesiana= tb_predicciones[ rank==irank, min( iteracion_bayesiana )],
      qsemillas= length(irepes) )

  # MLFlow

  semillas_qty <- length(irepes)
  for (icorte in cortes)
  {
     ganancia_media <- tb_ganancias_local[ envios == icorte, gan_sum ]
     linea <- list()
     linea$rank <- irank
     linea$iteracion_bayesiana <- tb_predicciones[ rank==irank & repeticion==vrepe, min(iteracion_bayesiana) ]
     linea$qsemillas <- semillas_qty
     linea$semilla <- -1
     linea$corte <- icorte
     linea$ganancia <- ganancia_media
     linea$metrica <- ganancia_media

     superacion <- FALSE
     if( ganancia_media  > ganancia_mejor )
     {
       ganancia_mejor <- ganancia_media
       superacion <- TRUE
     }
     mlog_log(linea, arch = "ganancias_log.txt", parentreplicate= superacion)
  }

  # Acumulo
  if( ! exists( "tb_ganancias" ) ) {
    tb_ganancias <- copy( tb_ganancias_local )
  } else {
    tb_ganancias <- rbind( tb_ganancias, tb_ganancias_local) 
  }

  fwrite( tb_ganancias,
          file= "tb_ganancias.txt",
          sep= "\t" )
}

#------------------------------------------------------------------------------

envg$OUTPUT$ganancias_suavizadas <- vganancias

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("tb_ganancias.txt")) 
cat( "ETAPA  z2602_KA_evaluate_kaggle_semillerio.r  END\n")
