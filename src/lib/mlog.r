
require("rlang")
require("rlist")
require("yaml")
require("mlflow")
require("digest")


if( !exists("envg") ) envg <- env()  # global environment 

#------------------------------------------------------------------------------

safe_mlflow_set_tracking_uri <- function( uri )
{
  res <- list()
  res$error <- FALSE
  tryCatch( { res$out <- mlflow_set_tracking_uri(uri) }
           , error = function(e) {res$error <- TRUE})

 return( res )
}
#------------------------------------------------------------------------------

safe_mlflow_set_experiment  <- function(
  experiment_name = NULL,
  experiment_id = NULL,
  artifact_location = NULL
)
{
  res <- list()
  res$error <- FALSE
  tryCatch( { res$out <- mlflow_set_experiment(experiment_name, experiment_id, artifact_location)}
           , error = function(e) {res$error <- TRUE})

 return( res )
}
#------------------------------------------------------------------------------

safe_mlflow_start_run  <- function(
  run_id = NULL,
  experiment_id = NULL,
  start_time = NULL,
  tags = NULL,
  client = NULL,
  nested = FALSE
)
{
  res <- list()
  res$error <- FALSE
  tryCatch( { res$out <- mlflow_start_run(run_id, experiment_id, start_time, tags, client, nested )}
           , error = function(e) {res$error <- TRUE})

 return( res )
}
#------------------------------------------------------------------------------

safe_mlflow_log_batch  <- function(
  metrics = NULL,
  params = NULL,
  tags = NULL,
  run_id = NULL,
  client = NULL
)
{
  res <- list()
  res$error <- FALSE
  tryCatch( { res$out <- mlflow_log_batch(metrics, params, tags, run_id, client )}
           , error = function(e) {res$error <- TRUE})

 return( res )
}
#------------------------------------------------------------------------------

safe_mlflow_end_run  <- function(
  status = "FINISHED",
  end_time = NULL,
  run_id = NULL,
  client = NULL
)
{
  res <- list()
  res$error <- FALSE
  tryCatch( { res$out <- mlflow_end_run(status, end_time, run_id, client )}
           , error = function(e) {res$error <- TRUE})

 return( res )
}
#------------------------------------------------------------------------------

#inicializo el ambiente de mlflow

mlog_mlflow_iniciar  <- function()
{
  #leo uri, usuario y password
  envg$mlog$mlflow$conn <- read_yaml(  paste0("/home/", envg$mlog$usuario, "/install/mlflow.yml" ) )
  envg$mlog$mlflow$conn$tracking_uri <- gsub( "\"", "", envg$mlog$mlflow$conn$tracking_uri )

  # seteo variables de entorno que necesita mlflow CLI
  Sys.setenv( MLFLOW_TRACKING_USERNAME= envg$mlog$mlflow$conn$tracking_username )
  Sys.setenv( MLFLOW_TRACKING_PASSWORD= envg$mlog$mlflow$conn$tracking_password )
  
  res <- safe_mlflow_set_tracking_uri( envg$mlog$mlflow$conn$tracking_uri )
  if( res$error ) return( res )

  Sys.setenv( PATH=paste0( "/home/", envg$mlog$usuario, "/.venv/bin:",
                           Sys.getenv("PATH")) )

  # mas seteo variables de entorno que necesita mlflow CLI
  Sys.setenv(MLFLOW_BIN= Sys.which("mlflow") )
  Sys.setenv(MLFLOW_PYTHON_BIN= Sys.which("python3") )
  Sys.setenv(MLFLOW_TRACKING_URI= envg$mlog$mlflow$conn$tracking_uri, intern= TRUE )

  return( res )
}
#------------------------------------------------------------------------------

mlog_list2tables  <- function( preg, prev="")
{
  tb_metrics <- data.table(
    "key1"= character(),
    value= numeric()
  )

  tb_params <- data.table(
    key1= character(),
    value= character()
  )

  prefijo <- ifelse( prev=="", "", paste0(prev,".") )

  for( campo in names(preg) )
  {
    campo_mostrar <- paste0(prefijo, campo)
    tipo <- typeof( preg[[ campo ]] )

    if( tipo %in% c("double","integer") )
       tb_metrics <- rbindlist( list(tb_metrics, list( campo_mostrar, preg[[ campo ]] ) ))

    if( tipo %in% c("logical") )
      tb_metrics <- rbindlist( list( tb_metrics, list( campo_mostrar, as.integer(preg[[ campo ]])) ))

    if( tipo %in% c("character", "symbol") )
     tb_params <- rbindlist( list(tb_params, list( campo_mostrar, preg[[ campo ]]) ))

   if( tipo %in% c("list") )
   {
      nuevo_prefijo <- paste0( prefijo, campo )
      tbls <- mlog_list2tables( preg[[ campo ]], nuevo_prefijo )
      tb_metrics <- rbindlist( list( tb_metrics, tbls$tb_metrics ) )
      tb_params <- rbindlist( list( tb_params, tbls$tb_params ) )
   }
  }

  return( list( "tb_metrics"=tb_metrics, "tb_params"=tb_params) )
}
#------------------------------------------------------------------------------

mlog_log_mlflow_reg  <- function( preg, iter, prev="", onlymetrics=FALSE )
{
  t1 <- as.integer(Sys.time())

  res <- mlog_list2tables( preg )

  res$tb_metrics$step <- iter
  res$tb_metrics$timestamp <- t1

  # carpinteria fea
  setnames( res$tb_metrics, "key1", "key" ) 
  setnames( res$tb_params, "key1", "key" )

  if( onlymetrics == FALSE )
  {
    res <- safe_mlflow_log_batch( res$tb_metrics, res$tb_params )
    return( res )
  } else {
    res <- safe_mlflow_log_batch( res$tb_metrics )
    return( res )
  }
}
#------------------------------------------------------------------------------

mlog_log_mlflow  <- function( reg, archivo, t0, parentreplicate=FALSE )
{
  #Inicio mlflow de ser necesario
  if( ! envg$mlog$mlflow$iniciado )
  {
    res <- mlog_mlflow_iniciar()
   if( res$error )   return(1)
    if( !res$error )
      envg$mlog$mlflow$iniciado <- TRUE
  }

  tarch <- envg$mlog$larch$archivos[[ archivo ]]
  tarch$mlflow$contador <- tarch$mlflow$contador + 1

  # seteo el experimento, lo crea si no existe
  if( !( "exp_id" %in% names( tarch$mlflow ) ) )
  {
   res <- safe_mlflow_set_experiment( tarch$mlflow$exp_name )
   if( res$error ) return( 1 )
   tarch$mlflow$exp_id <- res$out
  }

  reg_base <- list()
  reg_base$mlfowexp <- tarch$mlflow$exp_name
  reg_base$mlfowrun <- tarch$mlflow$run_name
  reg_base$usuario <- envg$mlog$usuario 
  reg_base$maquina <- envg$mlog$maquina
  reg_base$fecha <-  as.numeric(format(t0, "%Y%m%d.%H%M%S"))
  reg_base <- c( reg_base, tarch$cols_fijas )

  reg_padre <- copy(reg_base)
  reg_padre$jerarquia <- "padre"

  reg_hijo <- copy(reg_base)
  reg_hijo$jerarquia <- "hijo"
  reg_hijo <- c(reg_hijo, reg)


  if( !tarch$mlflow$padre_creado )
  {
    # creo el experimento padre si hace falta
    res <- safe_mlflow_start_run( experiment_id= tarch$mlflow$exp_id )
    if( res$error ) return( 2 )
    tarch$mlflow$padre_exp <- res$out

    res <- mlog_log_mlflow_reg( reg_padre, iter=tarch$mlflow$contador )
    if( res$error ) return( 3 )

    tarch$mlflow$padre_creado <- TRUE
    tarch$mlflow$padre_activo <- TRUE
  }

  # restauro el PADRE si hace falta
  if( ! tarch$mlflow$padre_activo )
  {
    res <- safe_mlflow_start_run( run_id= tarch$mlflow$padre_exp$run_uuid )
    if( res$error ) return( 4 )

    tarch$mlflow$padre_activo <- TRUE
  }

  if( parentreplicate )
  {
    res <- mlog_log_mlflow_reg( reg_hijo, iter=tarch$mlflow$contador, onlymetrics=TRUE )
    if( res$error ) return( 5 )
  }


  # inicio el hijo   NESTED
  res <- safe_mlflow_start_run( nested= TRUE )
  if( res$error ) return( 6 )
  
  tarch$mlflow$hijo_exp <- res$out
  tarch$mlflow$padre_activo <- FALSE

  # logueo el hijo completo
  res <- mlog_log_mlflow_reg( reg_hijo, iter=tarch$mlflow$contador )
  if( res$error ) return( 7 )

  # finalizo el experimento hijo
  res <- safe_mlflow_end_run(run_id= tarch$mlflow$hijo_exp$run_uuid)
  if( res$error ) return( 8 )

  # finalizo el PADRE
  res <- safe_mlflow_end_run(run_id= tarch$mlflow$padre_exp$run_uuid )
  if( res$error ) return( 9 )
  tarch$mlflow$padre_activo <- FALSE

  # persisto en la estructura global
  envg$mlog$larch$archivos[[ archivo ]] <- tarch

  # para poder retomar
  if( envg$mlog$persistir )
    write_yaml( envg$mlog , "mlog.yml" )

  return(0)  # salgo  SIN errores
}
#------------------------------------------------------------------------------
# funcion que se EXPORTA
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos
# aqui se agregara  mlflow

mlog_log  <- function( reg, arch=NA, parentreplicate=FALSE, verbose=TRUE )
{
  t0 <- Sys.time()
  archivo <- arch
  if( is.na(arch) ) archivo <- paste0( folder, substitute( reg), ext )


  if( !file.exists( archivo ) )
  {
    # Escribo los titulos
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  # escribo el registro
  linea  <- paste0( format(t0, "%Y%m%d.%H%M%S"),  "\t",     # la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  # grabo al archivo

  if( verbose )  cat( linea )   # imprimo por pantalla

  # grabo mlflow si corresponde
  if( archivo %in%  names( envg$mlog$larch$archivos ) )
    mlog_log_mlflow( reg, archivo, t0, parentreplicate )
}
#------------------------------------------------------------------------------
# funcion que se EXPORTA

mlog_table_hash <- function( tabla, digitos = 6)
{
  return(  substr(digest(tabla), 1, 6) )
}
#------------------------------------------------------------------------------

# funcion que se EXPORTA

mlog_addfile <- function( archivo, mlflow_exp, mlflow_run, cols_fijas)
{
  # si ya existe, lo elimino
  if( ! archivo %in% names(envg$mlog$larch$archivos) )
  {
    tarch <- list()

    tarch$nom_arch <- archivo
    tarch$cols_fijas <- cols_fijas
    tarch$mlflow$contador <- 0L
    tarch$mlflow$exp_name <- mlflow_exp
    tarch$mlflow$run_name <- mlflow_run
    tarch$mlflow$padre_creado <- FALSE

    envg$mlog$larch$archivos[[ archivo ]] <- tarch
    envg$mlog$larch$qty <- envg$mlog$larch$qty + 1
  }
}
#------------------------------------------------------------------------------
# funcion que se EXPORTA

mlog_init <- function( persistir=TRUE, recreate=FALSE )
{
  if( recreate==FALSE & file.exists( "mlog.yml" ) )
  {
    envg$mlog <- read_yaml( "mlog.yml" )
  }  else {
    envg$mlog <- list()
    envg$mlog$larch$qty <- 0
    envg$mlog$larch$archivos <- list()
  }
  
  envg$mlog$mlflow$iniciado <- FALSE
  envg$mlog$usuario <- Sys.info()["user"]
  envg$mlog$maquina <- Sys.info()["nodename"]
  envg$mlog$persistir <- persistir

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
