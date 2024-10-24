# action_lib.r
# para esr llamada desde cada script que realiza una accion
require("rlang", quietly=TRUE)

if( !exists("envg") ) envg <- env()  # global environment 

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)

  t <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t, "\n",
    file = "z-Rabort.txt",
    append = TRUE
  )

  arch <- "z-Rcanresume.txt"
  if( file.exists( arch ) )  file.remove( arch )

  stop("exiting after script error")
})

#------------------------------------------------------------------------------

require( "ulimit", quietly=TRUE)  # para controlar la memoria

#------------------------------------------------------------------------------

carpeta_actual <- function()
{
  st <- getwd()

  largo <- nchar(st)
  i <- largo
  while( i >= 1 & substr( st, i, i) == "/" )  i <- i - 1   # quito ultimo
  largo <- i

  while( i >= 1 & substr( st, i, i) != "/" & substr( st, i, i) != "\\"  )  i <- i - 1

  if( substr(st, i, i) == "/" ) i <- i + 1

  res <- ""
  if( i <= largo )  res <- substr( st, i, largo )

  return( res )
}
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml( envg$OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------

action_abortar <- function( mensaje ){

  t0 <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t0, "\n",
       file = "z-Rabort.txt",
       append = TRUE
  )

  arch <- "z-Rcanresume.txt"
  if( file.exists( arch ) )  file.remove( arch )

  stop( paste0("Abortando, " , mensaje ) )
}
#------------------------------------------------------------------------------
# verifica la existencia del archivo, en caso contrario stop

action_verificar_archivo <- function( arch ) {

  cat( "Verificando archivo :", arch, "\n" )
  if( !file.exists( arch ) )
    action_abortar( paste0("No existe el archivo : ", arch, "\n" ) ) 
}
#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {

  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------

action_inicializar  <- function( memoria = TRUE) {

  t0 <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t0, "\n",
    file = "z-Rbegin.txt",
    append = TRUE
  )

  action_verificar_archivo( "parametros.yml"  )

  envg$PARAM <- list()
  envg$OUTPUT<- list()

  # Parametros del script
  envg$PARAM <- read_yaml( "parametros.yml" )


  envg$OUTPUT <- list()

  envg$OUTPUT$PARAM <- envg$PARAM
  envg$OUTPUT$time$start <- t0

  envg$PARAM$experimento <- carpeta_actual()  # nombre del experimento donde estoy parado
  envg$PARAM$experimento_largo <- getwd()  # nombre del experimento donde estoy parado

  if( memoria ) action_limitar_memoria( 4 )

  cat( "Inicio del programa\n")
}
#------------------------------------------------------------------------------

action_finalizar <- function( archivos = c() )  {

  cat( "fin del programa\n")

  if( length( archivos > 0 ) ) {

    for( arch in archivos )
      action_verificar_archivo( arch )

  }

  t0 <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t0, "\n",
    file = "z-Rend.txt",
    append = TRUE
  )

}
#------------------------------------------------------------------------------
