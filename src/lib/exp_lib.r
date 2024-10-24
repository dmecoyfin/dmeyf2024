# libreria complicada, que accede a variables de sus padres
#   para simplificar la vida a quien la usa

require( "data.table", quietly=TRUE) 
require( "tools" , quietly=TRUE) 
require("rlist", quietly=TRUE) 
require( "mlflow", quietly=TRUE) 
require("yaml", quietly=TRUE) 

dir.create( "~/tmp", showWarnings = FALSE)

#------------------------------------------------------------------------------
# sufijo de un diretorio

sufijo <- function( st )
{
  if( nchar( st ) > 0 )
  {
    i <- nchar( st )
    while ( i > 0 & substr( st, i, i ) == "/" )  i <- i - 1
    if( i==1 & substr( st, i, i ) == "/" ) return( "" )
    last <- i

    while ( i > 0 & substr( st, i, i ) != "/" )  i <- i - 1

    if( substr( st, i, i ) == "/" ) i <- i+1
    return(  substr( st, i, last ) )
  }

  return( ""  )
}
#------------------------------------------------------------------------------
# envio mensaje ( Zulip )

exp_message_send <- function( messenger, message )
{
  if( nchar( messenger ) > 0 )
  {
    servidor <- Sys.info()["nodename"]
    comando <- paste0( messenger, " '", message, ", ", servidor, "'" )
    system( comando )
  }
}
#------------------------------------------------------------------------------
# verifico si existe el archivo, sino aborto escribiendo tambien en el workflow

exp_checkfile_abort <- function( fname, direxit, nomexp )
{
  if( !file.exists( fname ) )
  {
    cat( "No encuentro el archivo :",  fname, "\n" )

    # escribo en la carpeta local
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rabort.txt",
      append = TRUE
    )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
        "abort\t",
        fname, "\n",
        file = "z-SHlog.txt",
        append = TRUE
       )

    setwd(direxit)
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
       file = "z-Rabort.txt",
       append = TRUE
       )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
       nomexp, "\t",
       "abort\n",
       file = "z-SHlog.txt",
       append = TRUE
      )

    stop("Saliendo")
  }
}
#------------------------------------------------------------------------------
# verifico si existe la carepta, sino aborto escribiendo tambien en el workflow

exp_checkdir_abort <- function( fname, direxit, nomexp )
{
  if( !dir.exists( fname ) )
  {
    cat( "No encuentro la carpeta :",  fname, "\n" )

    # escribo en la carpeta local
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rabort.txt",
      append = TRUE
    )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
        "abort\t",
        fname, "\n",
        file = "z-SHlog.txt",
        append = TRUE
       )

    setwd(direxit)
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
       file = "z-Rabort.txt",
       append = TRUE
       )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
       nomexp, "\t",
       "abort\n",
       file = "z-SHlog.txt",
       append = TRUE
      )

    stop("Saliendo")
  }
}
#------------------------------------------------------------------------------

# corre en Linux & friends

exp_softlink <- function( dest, source)
{
  st <- paste0( "ln -sf ", source, "   ", dest )
  system( st )
}
#------------------------------------------------------------------------------
# Elimina  Workflows  fallidos

exp_eliminar_workflows_fallidos <- function( )
{
  setwd( envg$EXPENV$wf_dir )
  dirs <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

  for( dir in dirs ) {
    if( file.exists( paste0( dir, "/z-Rabort.txt" ) ) )
      system( paste0( "rm -rf ", dir ) )
  }
}
#------------------------------------------------------------------------------
# precondicion el getwd() es donde la quiero crear
# SIEMPRE creo una carpeta nueva para el workflow

exp_wf_crear_nombre_carpeta <- function( carpetawf, pcarpeta )
{
  setwd( carpetawf ) # cambio a donde estan los workflows

  dirs <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
  dirs <- dirs[ grepl( paste0("^", pcarpeta, "-[0-9]+" ), dirs) ]

  if( length( dirs )  > 0 )
  {
    # ya existen sufijos para el nombre
    dirs <- dirs[ grepl( paste0("[0-9]$" ), dirs) ]
    dirs <- sort( dirs )
    dir_last <- dirs[ length( dirs ) ] # el mas avanzado
    # busco el sufijo numerico
    largo <- nchar(dir_last)
    i <- largo
    while( i > 0 & substr(dir_last,i,i) != "-" )  i <- i - 1
    sufijo <- substr( dir_last, i+1, largo )
    # incremento el sufijo numerico
    sufijo_num <- as.numeric( sufijo ) + 1
  } else {
    # es el primer sufijo que voy a asignar
    sufijo_num <- 1
  }

  # devuelvo un string para el sufijo con 0's al inicio
  return( paste0( pcarpeta, "-", sprintf( "%03d", sufijo_num ) ) )
}
#------------------------------------------------------------------------------

exp_gc_shutdown <- function( pbucket_dir, pwf_dir, pnombrewf )
{
  cat( "EXPLIB  exp_gc_shutdown()  START\n")
  cat( "EXPLIB  pbucket_dir = ", pbucket_dir, "\n" )
  cat( "EXPLIB  pwf_dir = ", pwf_dir, "\n" )
  cat( "EXPLIB  pnombrewf = ", pnombrewf, "\n" )

  i <- 1
  while( substr( pbucket_dir, i,i) ==  substr( pwf_dir, i,i) ) i <- i + 1
  while( substr( pwf_dir, i,i) == "/"  ) i <- i + 1

  j <- 1
  while( substr( pwf_dir, j,j) ==  substr( pnombrewf, j,j) ) j <- j + 1
  while( substr( pnombrewf, j,j) == "/"  ) j <- j + 1

  carpeta <- paste0( substr( pwf_dir, i, 256 ), substr( pnombrewf, j, 256) )

  system( "echo >> ~/install/rutashutdown_old.txt && cat  ~/install/rutashutdown.txt >> ~/install/rutashutdown_old.txt")
  cat( carpeta, file = "~/install/rutashutdown.txt" )
  cat( "EXPLIB ", carpeta, "\n" )
  cat( "EXPLIB  exp_gc_shutdown()  END\n")
  cat("------------------------------------------------------------\n")
}
#------------------------------------------------------------------------------
#  SIEMPRE crea una carpeta nueva

exp_wf_init <- function( pnombrewf )
{
  # creo la variable de salida
  output <- list()
  output$bucket_dir <- envg$EXPENV$bucket_dir
  output$exp_dir <- envg$EXPENV$exp_dir
  output$wf_dir <- envg$EXPENV$wf_dir
  output$repo_dir <- envg$EXPENV$repo_dir
  output$datasets_dir <- envg$EXPENV$datasets_dir
  output$semilla_primigenia <- envg$EXPENV$semilla_primigenia
  output$semilla <- envg$EXPENV$semilla_primigenia
  output$messenger <- envg$EXPENV$messenger
  output$scriptname <- envg$EXPENV$scriptname


  if( !missing(pnombrewf) ) {
    # si me llego ese nombre, lo utilizo
    posible_nombrewf <- pnombrewf
  } else {
    # utilizo el nombre de la funcion que invoco a esta -1
    posible_nombrewf <- as.list(sys.call(-1))[[1]]
  }

  # creo el nombre correcto para mi workflow, puede ser secuencia -001
  output$nombrewf <- exp_wf_crear_nombre_carpeta( 
    output$wf_dir, posible_nombrewf )

  # creo efectivamente la carpeta del workflow
  setwd( output$wf_dir )
  dir.create( output$nombrewf, showWarnings = FALSE)
  setwd( output$nombrewf )
  file.copy( paste0( output$repo_dir, "/", output$scriptname) , "." )
  cat( "EXPLIB  dentro de exp_wf_init(), output$nombrewf = " , output$nombrewf, "\n" )

  # inicializo el logueo MLFlow, estoy dento de output$nombrewf
  mlog_init()
  mlog_addfile("bitacora_run.txt",
               mlflow_exp= "/flow",
               mlflow_run= output$nombrewf,
               cols_fijas= list() )


  exp_gc_shutdown( output$bucket_dir, output$wf_dir, output$nombrewf )

  output$lastexp <- ""
  output$instruction <- 0L
  output$resultado <- 0L

  return( output )
}
#------------------------------------------------------------------------------
# A esta carepta del workflow ingrese una sola vez

exp_wf_end <- function()
{
  # traigo el nombre del workflow, de la variable localvars del padre
  env_up <- parent.frame( n=1 )
  cat( "EXPLIB  exp_wf_end()",  names(env_up),"\n")
 
  lastexp <- env_up$param_local$lastexp
  nombrewf <- env_up$param_local$nombrewf
  wf_dir  <- env_up$param_local$wf_dir

  # no puede suceder esto en este punto
  setwd( wf_dir )
  if( !dir.exists( nombrewf ) ) {
    stop( paste0( "Falla catastrofica, al final workflow pero no existe la carpeta", nombrewf) )
  }

  # agrego que termino todo OK, si ninguno de los hijos aborto
  if( !file.exists("z-Rabort.txt") )
  {
    setwd( nombrewf )
    # dejo la marca final
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rend.txt",
      append = TRUE
    )
  }

 return( lastexp )
}
#------------------------------------------------------------------------------
# no puedo verificar nada, porque aun no se el nombre del experimento
# no creo ninguna carpeta ni tampoco el softlink
# Esta funcion tiene que poder venir sin pinputexps

exp_init <- function( pbypass=FALSE )
{
  # variables del abuelo
  env_up1 <- parent.frame( n=1 )
  # cat( "imprimo  env_up1\n" )
  # print( names(env_up1) )

  env_up2 <- parent.frame( n=2 )
  # cat( "imprimo  env_up2\n" )
  # print( names(env_up2) )

  output <- list()
  # info que proviene del abuelo, y NO de global
  output$expenv$bucket_dir <- env_up2$param_local$bucket_dir
  output$expenv$exp_dir <- env_up2$param_local$exp_dir
  output$expenv$wf_dir <- env_up2$param_local$wf_dir
  output$expenv$repo_dir <- env_up2$param_local$repo_dir
  output$expenv$datasets_dir <- env_up2$param_local$datasets_dir
  output$expenv$semilla_primigenia <- env_up2$param_local$semilla_primigenia
  output$expenv$nombrewf <- env_up2$param_local$nombrewf
  output$expenv$messenger <- env_up2$param_local$messenger
  output$expenv$scriptname <- env_up2$param_local$scriptname

  cat( "EXPLIB  output$expenv$nombrewf  = ", output$expenv$nombrewf , "\n" )

  # la semilla va directo
  output$semilla <- env_up2$param_local$semilla

  output$expenv$myexp <- ""
  output$expenv$funcname <- as.character( as.list(sys.call(-1))[[1]] )
  output$expenv$bypass <- pbypass
  output$resultado <- 0L

  cat( "EXPLIB  exp_init()  env_up2$param_local$lastexp  = ", env_up2$param_local$lastexp, "\n")

  if( "pinputexps"  %in%  names(env_up1) ) 
  {
    cat( "existe pinputexps en  env_up1\n" )
    cat( "pinputexps largo= ",  length(env_up1$pinputexps), 
    " ; nchar= ", nchar(env_up1$pinputexps),
    " ; vacio= ", env_up1$pinputexps=="",
    " ; valor= ", env_up1$pinputexps, "\n")
    print( names(env_up1) )
  } else {
    cat( "NO existe pinputexps en  env_up1\n" )
  }

  # magia para no recibir  parametro de input
  output$input_exps <- c()
  if( !( "pinputexps"  %in%  names(env_up1) ) ){

    if( env_up2$param_local$lastexp != "" ) output$input_exps <- env_up2$param_local$lastexp

  } else {
  
    if( nchar(env_up1$pinputexps)[1] > 0  ) output$input_exps <- env_up1$pinputexps

    if( nchar(env_up1$pinputexps)[1] == 0 &  env_up2$param_local$lastexp != "" )
      output$input_exps <- env_up2$param_local$lastexp

  }


  # creo el softlink a ESTE experimento en su carpeta workflow
  # por mas que luego de error
  env_up2$param_local$instruction <- env_up2$param_local$instruction + 1
  output$expenv$instruction <- env_up2$param_local$instruction
  cat( "EXPLIB  env_up2$param_local$instruction = ", env_up2$param_local$instruction  , "\n" )
  output$expenv$softlink  <- paste0( sprintf( "%03d", output$expenv$instruction ),
     "-", output$expenv$funcname )


  # verifico la existencia de los inputs
  if( length( output$input_exps ) > 0)
  {
    setwd( output$expenv$exp_dir )
    for( vexp in output$input_exps )
    {
      if( !dir.exists( vexp ) )
      {
        cat( "NO existe el experimento input :",  vexp, "\n" )

        setwd(output$expenv$wf_dir)

        # esto se escribe en la carpeta del workflow
       cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
         file = "z-Rabort.txt",
         append = TRUE
       )

       cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
         "abort\n",
         file = "z-SHlog.txt",
         append = TRUE
       )

       setwd(output$expenv$wf_dir)
       stop("Saliendo")
      } else {
        if( file.exists( paste0( output$expenv$exp_dir, "/", vexp, "/z-Rabort.txt") ) )
        {
          cat( "Input abortado :",  vexp, "\n" )

          setwd(output$expenv$wf_dir)

          # esto se escribe en la carpeta del workflow
          cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
            file = "z-Rabort.txt",
            append = TRUE
          )

          cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
            "abort\n",
            file = "z-SHlog.txt",
           append = TRUE
          )

          setwd(output$expenv$wf_dir)
          stop("Saliendo")

        }
      }
    }
  }


  output$resultado <- 0L

  return(output)
}
#------------------------------------------------------------------------------

exp_extraer_archivo <- function( st )
{
  largo <- nchar(st)
  i <- largo
  while( i >= 1 & substr( st, i, i) != "/" )  i <- i - 1

  if( substr(st, i, i) == "/" ) i <- i + 1

  res <- ""
  if( i <= largo )  res <- substr( st, i, largo )

  return( res )
}
#------------------------------------------------------------------------------
# aqui busco si ya he corrido ese experimento y corrio OK

exp_buscar_experimento <- function( pparam )
{
  setwd( pparam$expenv$carpeta_wf )
  system( "rm -rf tempo")
  dir.create( "tempo", showWarnings = FALSE)
  setwd( "./tempo" )

  # se supone que estamos en la carepta tempo
  # grabo el archivo de parametros que va a recibir el script local
  param_simple <- data.table::copy( pparam )
  param_simple$resultado <- NULL
  param_simple$expenv <- NULL

  write_yaml( param_simple, "parametros.yml" )
  param_md5nuevo <- md5sum("parametros.yml")

  script_corto <- exp_extraer_archivo( pparam$meta$script )
  script_origen <- paste0( pparam$expenv$repo_dir, "/", pparam$meta$script )
  # copio el script R a la carpeta del experimento
  #   esto deberia manejarse con Git
  file.copy( script_origen, "." )
  script_md5nuevo <- md5sum(script_origen)

  output <- list()
  output$encontre <- FALSE
  output$Rend <- FALSE
  output$bypass <- FALSE
  output$resume <- FALSE
  output$folder <- ""

  # busco si ya existe este experimento
  # todos los experimentos
  setwd( pparam$expenv$exp_dir )
  dirs <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
  dirs <- dirs[order( dirs )]

  largo <- length(dirs)
  candidatos <- rep( 0, largo )
  for( i in 1:largo )
  {
    vdir <- dirs[i]
    script_old <- paste0( vdir, "/", script_corto )
    if( file.exists( script_old ) )
    {
      script_md5old <- md5sum( script_old )
      if( script_md5nuevo == script_md5old )
      {
         # coinciden los scripts a correr
         param_old <- read_yaml( paste0( vdir, "/parametros.yml" ) )
         param_old$experimento <- NULL
         write_yaml( param_old, "~/tmp/parametros.yml" )
         param_md5old <- md5sum("~/tmp/parametros.yml")
         if( param_md5nuevo == param_md5old )
         {
           cat( "coincide= ",  vdir ,"\n") 
           candidatos[i] <- -1

           if( candidatos[i] == -1 & 
              file.exists( paste0( vdir, "/z-Rend.txt" ))) candidatos[i] <- 3

           if( candidatos[i] == -1 &
              pparam$expenv$bypass &
              file.exists( paste0( vdir, "/z-Rcanbypass.txt" ))) candidatos[i] <- 2

           if( candidatos[i] == -1 &
              file.exists( paste0( vdir, "/z-Rcanresume.txt" )) &
              !file.exists( paste0( vdir, "/z-Rabort.txt" )) ) candidatos[i] <- 1

         }

      }
    }
  }

  cat( "candidatos = ", candidatos, "\n")
  excelentes <- which( candidatos==3 )
  if( length(excelentes) > 0 )  
  { 
    output$encontre <- TRUE
    output$Rend <- TRUE
    output$folder <- dirs[ excelentes[ length(excelentes) ] ]
  } else {
    aceptables <- which( candidatos==2 )
    # cat( "pparam$expenv$bypass= ", pparam$expenv$bypass , "\n")
    if( length( aceptables ) > 0  & pparam$expenv$bypass==TRUE )
    {
      output$encontre <- TRUE
      output$bypass <- TRUE
      output$folder <- dirs[ aceptables[ length(aceptables) ] ]
    } else {
      resumibles <- which( candidatos==1 )
      if( length( resumibles ) > 0 )
      {
        output$encontre <- TRUE
        output$resume <- TRUE
        output$folder <- dirs[ resumibles[ length(resumibles) ] ]
      }
    }
  }

  # elimino tempo
  # setwd( pparam$expenv$carpeta_wf )
  # system( "rm -rf tempo")
  cat(  "EXPLIB  exp_buscar_experimento()\n")
  # print( output)
  cat( "EXPLIB  ---------\n")
  return( output )
}
#------------------------------------------------------------------------------

exp_crear_nombre_experimento <- function( pmeta )
{
  if( !dir.exists( pcarpeta ) )
  {
     return( pcarpeta )
  }

  dirs <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
  dirs <- dirs[ grepl( paste0("^", pcarpeta, "-[0-9]+" ), dirs) ]
  if( length( dirs > 0 ) )
  {
    # ya existen sufijos para el nombre
    dirs <- dirs[ grepl( paste0("[0-9]$" ), dirs) ]
    dirs <- sort( dirs )
    dir_last <- dirs[ length( dirs ) ] # el mas avanzado
    # vusco el sufijo numerico
    largo <- nchar(dir_last)
    i <- largo
    while( i > 0 & substr(dir_last,i,i) != "-" )  i <- i - 1
    sufijo <- substr( dir_last, i+1, largo )
    # incremento el sufijo numerico
    sufijo_num <- as.numeric( sufijo ) + 1
  } else {
    # es el primer sufijo que voy a asignar
    sufijo_num <- 2
  }

  # devuelvo un string para el sufijo con 0's al inicio
  return( sprintf( "%03d", sufijo_num ) )
}
#------------------------------------------------------------------------------
# supongo que las funciones NO pueden comenzar con  "_"

funcion_prefijo <- function( st )
{
  i <- 1
  largo <- nchar(st)
  while( i <= largo & substr(st,i,i) != "_" )  i <- i + 1

  if( i > largo )  return( st )
  i <- i -1

  return( substr(st,1,i) )
}
#------------------------------------------------------------------------------

generar_experimento_nombre <- function( pparam_local )
{
  cat( "EXPLIB  generar_experimento_nombre() START" , "\n")
  prefijo <- funcion_prefijo( pparam_local$expenv$funcname )


  setwd( pparam_local$expenv$exp_dir ) # cambio a donde estan los workflows
  dirs <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
  dirs <- dirs[ grepl( paste0("^", prefijo, "-[0-9]+" ), dirs) ]

  if( length( dirs ) > 0 )
  {
    # ya existen sufijos para el nombre
    dirs <- dirs[ grepl( paste0("[0-9]$" ), dirs) ]
    dirs <- sort( dirs )
    dir_last <- dirs[ length( dirs ) ] # el mas avanzado
    # busco el sufijo numerico
    largo <- nchar(dir_last)
    i <- largo
    while( i > 0 & substr(dir_last,i,i) != "-" )  i <- i - 1
    sufijo <- substr( dir_last, i+1, largo )
    # incremento el sufijo numerico
    sufijo_num <- as.numeric( sufijo ) + 1
  } else {
    # es el primer sufijo que voy a asignar
    sufijo_num <- 1
  }

  # Creo la carpeta del experimento
  nom_experimento <- paste0( prefijo, "-", sprintf( "%04d", sufijo_num ) )

  cat( "EXPLIB  generar_experimento_nombre() END" , "\n")
  return( nom_experimento )
}
#------------------------------------------------------------------------------
# debe crear un experimento totalmente nuevo

crear_experimento_nuevo <- function( pparam_local )
{
  cat( "EXPLIB  crear_experimento_nuevo() START" , "\n")
  nom_experimento <- generar_experimento_nombre( pparam_local ) 

  exp_carpeta <- paste0( pparam_local$expenv$exp_dir, nom_experimento )

  dir.create( nom_experimento, showWarnings = TRUE)

  # Creo el softlink
  setwd(pparam_local$expenv$carpeta_wf)
  cat( "EXPLIB  crear_experimento_nuevo() ", pparam_local$expenv$softlink, exp_carpeta, "\n" )
  if( !dir.exists(pparam_local$expenv$softlink) )
      exp_softlink( pparam_local$expenv$softlink, exp_carpeta )


  setwd( exp_carpeta )
  param_simple <- data.table::copy( pparam_local )
  param_simple$resultado <- NULL
  param_simple$expenv <- NULL
  write_yaml( param_simple, "parametros.yml" )

  script_corto <- exp_extraer_archivo( pparam_local$meta$script )
  script_origen <- paste0( pparam_local$expenv$repo_dir, "/", pparam_local$meta$script )
  # copio el script R a la carpeta del experimento
  #   esto deberia manejarse con Git
  exp_checkfile_abort( script_origen, pparam_local$expenv$carpeta_wf, nom_experimento )
  # existe el archivo de script
  file.copy( script_origen, "." )

  # Si es un DT que toma como input un archivo y lo pasa a dataset
  if( "archivo" %in% names( pparam_local ) )
  {
    cat( "EXPLIB  pparam_local$expenv$datasets_dir = ", pparam_local$expenv$datasets_dir, "\n" )
    dataset_abs <- pparam_local$archivo_absoluto
    exp_checkfile_abort( dataset_abs, pparam_local$expenv$carpeta_wf, nom_experimento )
    cat( "EXPLIB  exp_carpeta : ", exp_carpeta, "\n" )
    setwd( exp_carpeta )
    exp_softlink( "datasets", pparam_local$expenv$datasets_dir )
  }

  # si hay experimentos inputs
  if( "input_exps" %in%  names( pparam_local ) )
  {
    setwd(pparam_local$expenv$exp_dir)
    for( vexp in pparam_local$input_exps )
    {
      exp_checkdir_abort( vexp, pparam_local$expenv$carpeta_wf, nom_experimento )

      if( file.exists( paste0( pparam_local$expenv$exp_dir, "/", vexp, "/z-Rabort.txt") ) |
          ( !file.exists( paste0( pparam_local$expenv$exp_dir, "/", vexp, "/z-Rend.txt") ) ) &
          ( !file.exists( paste0( pparam_local$expenv$exp_dir, "/", vexp, "/z-Rcanbypass.txt") ) ) &
          ( !file.exists( paste0( pparam_local$expenv$exp_dir, "/", vexp, "/z-SHbypassartifact.txt") ) ) 
        )
      {
        cat( "EXPLIB  Fatal Error al querer correr ",  vexp, "\n" )
        cat( "EXPLIB  pparam_local$expenv$bypass =  ", pparam_local$expenv$bypass, "\n" )
        cat( "EXPLIB  z-Rcanbypass.txt =  ", file.exists( paste0( pparam_local$expenv$exp_dir, "/", vexp, "/z-Rcanbypass.txt") ), "\n" )
        cat( "EXPLIB  Input no usable :",  vexp, "\n" )

        # esto se escribe en la carpeta del workflow
        cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
          file = "z-Rabort.txt",
          append = TRUE
        )

        cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
          vexp, "\t",
          "abort\n",
          file = "z-SHlog.txt",
         append = TRUE
        )

        setwd( pparam_local$expenv$carpeta_wf )
        cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
           file = "z-Rabort.txt",
           append = TRUE
        )

        cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
          nom_experimento, "\t",
          "abort\n",
          file = "z-SHlog.txt",
          append = TRUE
        )

        stop("Saliendo")

      }
    }

   # si llega aqui es porque todos los inputs son viables
   # creo los softlinks
    setwd(exp_carpeta)
    for( vexp in pparam_local$input_exps )
    {
      if( !dir.exists(vexp) )
        exp_softlink( vexp, paste0( pparam_local$expenv$exp_dir, "/", vexp ) )
    }

  }


  # creo el script que corre el experimento
  if( !file.exists("run.sh"))
  {
    linea1  <- "#!/bin/bash\n"
    linea2  <- "tabulador=\"\t\"\n"
    linea3  <- "echo \"timestamp\tevent\"  >  z-SHlog.txt \n"
    linea4  <- "fecha0=$(date +\"%Y%m%d %H%M%S\") \n"
    linea5  <- "echo \"$fecha0\"\"$tabulador\"\"SH_START\" >> z-SHlog.txt \n"
    linea6  <- "~/install/memcpu & \n"
    linea7  <- "memcpu_PID=$! \n"
    linea8  <- "source /home/$USER/.venv/bin/activate \n"

    linea9 <- paste0( "nice -n 15 Rscript --vanilla ",
                      script_corto,
                      "  " ,
                      pparam_local$expenv$repo_dir,
                      "   ",
                      "parametros.yml",
                      "  2>&1 | tee z-SHoutfile.txt \n" )

    linea10  <- "fecha1=$(date +\"%Y%m%d %H%M%S\") \n"
    linea11  <- "echo \"$fecha1\"\"$tabulador\"\"SH_END\" >> z-SHlog.txt \n"
    linea12 <- "kill -SIGTERM $memcpu_PID \n"
    linea13 <- "deactivate \n"

    comando  <- paste0( linea1, linea2, linea3, linea4, linea5, linea6, linea7, linea8, linea9, linea10, linea11, linea12, linea13 )

    # creacion del archivo
    shell_script <- "run.sh"
    cat( comando, 
         file= shell_script )

    #doy permisos de ejecucion al shell script
    Sys.chmod( shell_script, mode = "744", use_umask = TRUE)
  }

  if( file.exists( "z-Rabort.txt" ) )  file.remove( "z-Rabort.txt" )

  return( nom_experimento )
}
#------------------------------------------------------------------------------
#  en pparam_local tengo todo lo que necesito
#  no hace falta ir a global ni a progenitores

exp_correr_script <- function( pparam_local )
{
  cat( "EXPLIB  exp_correr_script()", " START\n")
  if( missing( pparam_local ) )
  {
    # esto se escribe en la carpeta del workflow
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rabort.txt",
      append = TRUE
    )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
      "init_datos", "\t",
      "abort\n",
      file = "z-SHlog.txt",
      append = TRUE
    )

    stop( "Falla catastrofica, se le debe pasar parametro a  exp_correr_script()" )
  }

  env_up <- parent.frame( n=2 )

  output <- list()
  # info que proviene del abuelo, y NO de global
  output$expenv$exp_dir <- pparam_local$expenv$exp_dir

  pparam_local$expenv$carpeta_wf <-
    paste0( pparam_local$expenv$wf_dir, pparam_local$expenv$nombrewf )

  # busco si en ya existe el mismo experimento
  busqueda <- exp_buscar_experimento( pparam_local )
  cat( "EXPLIB  exp_correr_script() busqueda\n" )
  # print( busqueda )

  exp_carpeta <- paste0( pparam_local$expenv$exp_dir, busqueda$folder)

  setwd( pparam_local$expenv$carpeta_wf )
  # encontre el experimento y esta corrido o me pidieron bypass
  # no debo volver a correr nada, sigo de largo
  if( busqueda$encontre & busqueda$Rend )
  {
    # estoy seguro que termino bien, y NO aborto
    cat( "EXPLIB  exp_correr_script() experimento ya corrido", busqueda$folder, "\n" )
    setwd(pparam_local$expenv$carpeta_wf)
    if( !dir.exists(pparam_local$expenv$softlink) )
      exp_softlink( pparam_local$expenv$softlink, exp_carpeta )

    # logueo  MLFlow
    setwd( pparam_local$expenv$carpeta_wf )
    linea <- list()
    linea$id <- pparam_local$expenv$instruction
    linea$status <- "previo_end"
    linea$workflow <- pparam_local$expenv$carpeta_wf
    linea$expw <- exp_carpeta
    linea$funcion <- pparam_local$expenv$funcname
    mlog_log( linea, "bitacora_run.txt")

    env_up$param_local$lastexp <- busqueda$folder  # fundamental
    return( busqueda$folder )
  }

  # me pidieron hacer bypass y el experimento lo permite
  if( busqueda$encontre & pparam_local$expenv$bypass 
     & busqueda$bypass )
  {
    nom_experimento_bypass <- generar_experimento_nombre( pparam_local )
    exp_bypass_carpeta <- paste0( pparam_local$expenv$exp_dir, "/", nom_experimento_bypass)

    setwd( pparam_local$expenv$exp_dir )
    system( paste0( "cp -r  ./", busqueda$folder, "  ./", nom_experimento_bypass) )
    setwd( nom_experimento_bypass )
    cat( format(Sys.time(), "%Y%m%d %H%M%S")  , file = "z-SHbypassartifact.txt" )
    file.remove( "z-Rcanbypass.txt" )
    file.remove( "z-Rcanresume.txt" )

    cat( "EXPLIB  exp_correr_script() nuevo bypass", nom_experimento_bypass, "\n" )
    setwd(pparam_local$expenv$carpeta_wf)
    if( !dir.exists(pparam_local$expenv$softlink) )
      exp_softlink( pparam_local$expenv$softlink, exp_bypass_carpeta )


    # logueo  MLFlow
    setwd( pparam_local$expenv$carpeta_wf )
    linea <- list()
    linea$id <- pparam_local$expenv$instruction
    linea$status <- "previo_bypass"
    linea$workflow <- pparam_local$expenv$carpeta_wf
    linea$expw <- exp_carpeta
    linea$funcion <- pparam_local$expenv$funcname
    mlog_log( linea, "bitacora_run.txt")


    env_up$param_local$lastexp <- nom_experimento_bypass  # fundamental
    return( nom_experimento_bypass )
  }

  cat( "EXPLIB  exp_correr_script() encontre1\n" )
  # debo continuar con el procesamiento
  #  que no termino en una corrida anterior
  if( busqueda$encontre & busqueda$resume & !busqueda$bypass )
  {
    setwd(pparam_local$expenv$carpeta_wf)
    if( !dir.exists(pparam_local$expenv$softlink) )
      exp_softlink( pparam_local$expenv$softlink, busqueda$folder )

    # logueo  MLFlow
    setwd( pparam_local$expenv$carpeta_wf )
    linea <- list()
    linea$id <- pparam_local$expenv$instruction
    linea$status <- "previo_resume"
    linea$workflow <- pparam_local$expenv$carpeta_wf
    linea$expw <- exp_carpeta
    linea$funcion <- pparam_local$expenv$funcname
    mlog_log( linea, "bitacora_run.txt")


    cat( "EXPLIB  paso a la carpeta = ", exp_carpeta, "\n")
    exp_gc_shutdown( pparam_local$expenv$bucket_dir, pparam_local$expenv$exp_dir, busqueda$folder )
    setwd( exp_carpeta )
    system( "./run.sh" )

    exp_gc_shutdown( pparam_local$expenv$bucket_dir, pparam_local$expenv$wf_dir, pparam_local$expenv$nombrewf )
    setwd( pparam_local$expenv$carpeta_wf )
    if( file.exists( paste0( exp_carpeta, "/z-Rabort.txt") ) )
    {
      # esto se escribe en la carpeta del workflow
      cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
        file = "z-Rabort.txt",
        append = TRUE
      )

      cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
        "exp_correr_script", "\t",
        "abort\n",
        file = "z-SHlog.txt",
        append = TRUE
      )

      mensaje <- paste0( "ABORT, ", pparam_local$expenv$nombrewf,", ", sufijo(exp_carpeta))
      exp_message_send( pparam_local$expenv$messenger, mensaje )

      stop( "correr_script   aborted\n" )
    } else {
      # Termino OK
      mensaje <- paste0( "Ok, ", pparam_local$expenv$nombrewf,", ", sufijo(exp_carpeta))
      exp_message_send( pparam_local$expenv$messenger, mensaje )
      setwd( pparam_local$expenv$carpeta_wf )
      env_up$param_local$lastexp <- busqueda$folder  # fundamental
      return( busqueda$folder )
    }
  }

  cat( "EXPLIB  exp_correr_script() encontre2\n" )

  listoparacorrer <- TRUE

  # Ahora realmente voy a tener que correr el experimento de cero
  # verifico los inputs
  pparam_local$expenv$nom_experimento <- crear_experimento_nuevo( pparam_local )

  # logueo  MLFlow
  setwd( pparam_local$expenv$carpeta_wf )
  linea <- list()
  linea$id <- pparam_local$expenv$instruction
  linea$status <- "actual_run"
  linea$workflow <- pparam_local$expenv$carpeta_wf
  linea$expw <- paste0( pparam_local$expenv$exp_dir, pparam_local$expenv$nom_experimento)
  linea$funcion <- pparam_local$expenv$funcname
  mlog_log( linea, "bitacora_run.txt")

  # ejecuto el experimento
  setwd( pparam_local$expenv$exp_dir )
  setwd( pparam_local$expenv$nom_experimento )
  #ejecuto en linux el script recien creado
  exp_gc_shutdown( pparam_local$expenv$bucket_dir, pparam_local$expenv$exp_dir, pparam_local$expenv$nom_experimento )
  system( paste0( "./run.sh") )
  exp_gc_shutdown( pparam_local$expenv$bucket_dir, pparam_local$expenv$wf_dir, pparam_local$expenv$carpeta_wf )

  # el proceso hijo aborto,  yo tambien debo abortar
  if( file.exists( "z-Rabort.txt" ) )
  {
    setwd( pparam_local$expenv$carpeta_wf )
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
      file = "z-Rabort.txt",
      append = TRUE
    )

    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
      pparam_local$expenv$nom_experimento, "\t",
      "abort\n",
      file = "z-SHlog.txt",
      append = TRUE
    )

    mensaje <- paste0( "ABORT, ", pparam_local$expenv$nombrewf,", ", sufijo(pparam_local$expenv$nom_experimento))
    exp_message_send( pparam_local$expenv$messenger, mensaje )

    stop( paste0("Falla catastrofica, ha terminado anormalmente", pparam_local$expenv$nom_experimento) )
  }


  mensaje <- paste0( "Ok, ", pparam_local$expenv$nombrewf,", ", sufijo(pparam_local$expenv$nom_experimento))
  exp_message_send( pparam_local$expenv$messenger, mensaje )

  env_up$param_local$lastexp <- pparam_local$expenv$nom_experimento  # fundamental
  cat( "EXPLIB  exp_correr_script() env_up$param_local$lastexp = ", env_up$param_local$lastexp , "\n" )
  return( pparam_local$expenv$nom_experimento  )
}
#------------------------------------------------------------------------------
