#!/usr/bin/env Rscript
cat( "ETAPA  z2501_EV_evaluate_conclase.r  INIT\n")

# Workflow  EV_evaluate_conclase

# input   archivos predicciones del tipo  < primarykey,  prob > 
#   (de varios ranks y semillas)
# outputs
#   archivo con las ganancias y sus promedios
#   graficos con las ganancias


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)

#cargo la libreria
# args <- c( "~/labo2024ba", "SC-0002" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/mlog.r" ) )
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "ETAPA  z2501_EV_evaluate_conclase.r  START\n")
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


envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[1], "/dataset_metadata.yml" ) )

# Inicializo MLFlow -------------------
mlog_init()

datahash <- mlog_table_hash( tb_future_prediccion[, envg$PARAM$dataset_metadata$primarykey, with=FALSE ] )
mlflow_exp_det <- paste0( "/futu-", datahash )

mlog_addfile("ganancias_log.txt",
             mlflow_exp= mlflow_exp_det,
             mlflow_run= envg$PARAM$experimento,
             cols_fijas= list(expw=envg$PARAM$experimento_largo) )
#--------------------------------------

tb_ganancias <- as.data.table( list( envios =  seq( nrow(tb_future_prediccion))))

ranks <- tb_predicciones[ , unique(rank) ]

ganancia_mejor <- -Inf
vganancias_suavizadas <- c()

for ( irank in ranks ) {

  gan_sum <- paste0( "gan_sum_", irank )
  tb_ganancias[ , paste0(gan_sum) := 0 ]

  isems <- tb_predicciones[ rank==irank, unique( isem )]

  for (vsem in isems)
  {
    cat( irank, vsem, "\n")
    envg$OUTPUT$status$rank <- irank
    envg$OUTPUT$status$isem <- vsem
    GrabarOutput()

    campito <- tb_predicciones[ rank==irank & isem==vsem, campo]
    temp_pred <- tb_future_prediccion[ , c( campito, envg$PARAM$dataset_metadata$clase ), with=FALSE ]
    setorderv( temp_pred, campito, -1 )
    temp_pred[, ganancia_acum :=
        cumsum(ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, envg$PARAM$train$gan1, envg$PARAM$train$gan0))]

    temp_pred[, gan_suavizada := frollmean(
      x = ganancia_acum,
      n = envg$PARAM$graficar$ventana_suavizado,
      align = "center",
      na.rm = TRUE,
      hasNA = TRUE
    )]

    ganancia_suavizada_max <- temp_pred[, max(gan_suavizada, na.rm = TRUE)]
    corte_mejor <- which.max( temp_pred[, gan_suavizada ] )

    tb_ganancias[, paste0(gan_sum) := get(gan_sum) + temp_pred$ganancia_acum ]
    tb_ganancias[, paste0(campito) := temp_pred$ganancia_acum ]

    # MLFlow
    linea <- list()
    linea$rank <- irank
    linea$iteracion_bayesiana <- tb_predicciones[ rank==irank & isem==vsem, iteracion_bayesiana]
    linea$qsemillas <- 1
    linea$semilla <- tb_predicciones[ rank==irank & isem==vsem, semilla]
    linea$corte <- corte_mejor
    linea$ganancia <- ganancia_suavizada_max
    linea$metrica <- ganancia_suavizada_max
    mlog_log(linea, arch = "ganancias_log.txt")

    ymax <- max(tb_ganancias, na.rm = TRUE)


    rm(temp_pred)
    gc(verbose= FALSE)
  }

  tb_ganancias[, paste0(gan_sum) := get(gan_sum) / length(isems) ]

  # calculo la mayor ganancia  SUAVIZADA
  tb_ganancias[, gan_suavizada := frollmean(
    x = get(gan_sum),
    n = envg$PARAM$graficar$ventana_suavizado,
    align = "center",
    na.rm = TRUE,
    hasNA = TRUE
  )]

  ganancia_suavizada_max <- tb_ganancias[, max(gan_suavizada, na.rm = TRUE)]
  corte_mejor <- which.max( tb_ganancias[, gan_suavizada ] )

  # MLFlow
  linea <- list()
  linea$rank <- irank
  linea$iteracion_bayesiana <- tb_predicciones[ rank==irank, min(iteracion_bayesiana) ]
  linea$qsemillas <- length(isems) 
  linea$semilla <- -1
  linea$corte <- corte_mejor
  linea$ganancia <- ganancia_suavizada_max
  linea$metrica <- ganancia_suavizada_max

  superacion <- FALSE
  if( ganancia_suavizada_max  > ganancia_mejor )
  {
    ganancia_mejor <- ganancia_suavizada_max
    superacion <- TRUE
  }
  mlog_log(linea, arch = "ganancias_log.txt", parentreplicate= superacion)

  ymax <- max(tb_ganancias, na.rm = TRUE)

  campos_ganancias <- setdiff(colnames(tb_ganancias), "envios")
  ymin <- min(tb_ganancias[envios >= envg$PARAM$graficar$envios_desde & envios <= envg$PARAM$graficar$envios_hasta, campos_ganancias, with=FALSE ],
      na.rm = TRUE
    )

  arch_grafico <- paste0(
    "modelo_",
    sprintf("%02d", irank),
    "_",
    sprintf("%03d", tb_predicciones[ rank==irank, min( iteracion_bayesiana )]),
    ".pdf"
  )

  pdf(arch_grafico)

  plot(
    x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
    y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, get(tb_predicciones[ rank==irank & isem==1, campo]) ],
    type = "l",
    col = "gray",
    xlim = c(envg$PARAM$graficar$envios_desde, envg$PARAM$graficar$envios_hasta),
    ylim = c(ymin, ymax),
    main = paste0("Mejor gan prom = ", as.integer(ganancia_suavizada_max)),
    xlab = "Envios",
    ylab = "Ganancia",
    panel.first = grid()
  )

  # las siguientes curvas
  if ( length(isems) > 1) {
    for (s in 2:length(isems))
    {
      lines(
        x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
        y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, get(tb_predicciones[ rank==irank & isem==s, campo])],
        col = "gray"
       )
    }
  }

  # finalmente la curva promedio
  lines(
    x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
    y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, get(gan_sum) ],
    col = "red"
  )

  dev.off()

  # Aqui se deberia grabar en una tabla general  ganancia_suavizada_max

  # grabo las ganancias, para poderlas comparar con OTROS modelos
  arch_ganancias <- paste0(
    "ganancias_",
    sprintf("%02d", irank),
    "_",
    sprintf("%03d", tb_predicciones[ rank==irank, min( iteracion_bayesiana )]),
    ".txt"
   )

  fwrite(tb_ganancias,
    file = arch_ganancias,
    sep = "\t",
  )

  vganancias_suavizadas <- c( vganancias_suavizadas, ganancia_suavizada_max)
}

#------------------------------------------------------------------------------

envg$OUTPUT$ganancias_suavizadas <- vganancias_suavizadas

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c()) 
cat( "ETAPA  z2501_EV_evaluate_conclase.r  END\n")
